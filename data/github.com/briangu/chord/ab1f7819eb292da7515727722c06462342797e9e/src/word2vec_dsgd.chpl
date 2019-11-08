/*
  Distributed Word2Vec
*/

use BlockDist, Logging, Time, PrivateDist, VisualDebug;

type elemType = real(64);

const MAX_STRING = 100;
const EXP_TABLE_SIZE = 1000;
const MAX_EXP = 6;
const MAX_SENTENCE_LENGTH = 1000;
const MAX_CODE_LENGTH = 40;

const vocab_hash_size = 1024 * 1024 * 32;  // Maximum 30 * 0.7 = 21M words in the vocabulary
const vocab_hash_size_mask = (2 ** 25) - 1;

// Command line arguments
config var vocab_max_size = 1000;
config const min_count = 5;
config const train_file = "";
config const save_vocab_file = "";
config const read_vocab_file = "";
config const output_file: string = "";
config const hs = 0;
config const negative = 5;
config const iterations = 5;
config const window = 5;
config const cbow = 1;
config const binary = 0;
config const classes = 0;
config var alpha = 0.05; //0.025 * 2;
config const min_alpha = 0.0001;
config const sample = 1e-3;
config const size = 100;
config const debug_mode = 2;
config const num_threads = here.maxTaskPar;
config const num_param_locales = 1;
config const save_interval = 0;
config const update_alpha = 0.1;
config const update_delta = 0.1;
config const override_train_words = 0;

const SPACE = ascii(' '): uint(8);
const TAB = ascii('\t'): uint(8);
const CRLF = ascii('\n'): uint(8);

const layer1_size = size;

writeln("num_threads = ", num_threads);
writeln("numLocales = ", numLocales);
writeln("iterations = ", iterations);

const computeLocalesStart = num_param_locales;
const numComputeLocales = Locales.size - num_param_locales;
const computeLocales = Locales[computeLocalesStart..];

writeln("computeLocalesStart ", computeLocalesStart);
writeln("numComputeLocales ", numComputeLocales);
writeln("computeLocales ", computeLocales);
writeln("num_param_locales ", num_param_locales);

proc reportStats(statsTimer, locale_word_count, max_locale_words, alpha) {
  var now = statsTimer.elapsed(TimeUnits.milliseconds);
  writef("\rAlpha: %r  Progress: %0.3r%%  Words/sec: %rk  Words/thread/sec: %rk   ",
        alpha,
        min(100.0, (locale_word_count / max_locale_words:real) * 100),
        (locale_word_count * numComputeLocales:real) / ((now + 1) / 1000) / 1000,
        (locale_word_count / num_threads:real) / ((now + 1) / 1000) / 1000);
  stdout.flush();
}

record VocabEntry {
  var len: int;
  var word: [0..#MAX_STRING] uint(8);
  var cn: int(64);
};

record VocabTreeEntry {
  var codelen: uint(8);
  var code: [0..#MAX_CODE_LENGTH] uint(8);
  var point: [0..#MAX_CODE_LENGTH] int;
};

class VocabContext {
  var vocab_size: int;
  var vocab_max_size = 1000;
  var vocabDomain = {0..#vocab_max_size};
  var vocab: [vocabDomain] VocabEntry;
  var vocab_hash: [0..#vocab_hash_size] int = -1;
  var vocabTreeDomain: domain(1);
  var vocab_tree: [vocabTreeDomain] VocabTreeEntry;

  var train_words: int = 0;
  var min_reduce = 1;

  var table_size: int = 1e8:int;
  var table: [0..#table_size] int;

  var atCRLF = false;

  proc loadFromFile(train_file, read_vocab_file, save_vocab_file, negative) {
    if (read_vocab_file != "") then ReadVocab(read_vocab_file); else LearnVocabFromTrainFile(train_file);
    if (save_vocab_file != "") then SaveVocab(save_vocab_file);
    CreateBinaryTree();
    if (negative > 0) then InitUnigramTable();
  }

  /*proc clone(vocabContext: VocabContext) {
    this.vocab_size = vocabContext.vocab_size;
    this.vocab_max_size = vocabContext.vocab_max_size;
    this.vocabDomain = {vocabContext.vocabDomain.low..vocabContext.vocabDomain.high};

    forall i in 0..#this.vocab_size {
      this.vocab[i].len = vocabContext.vocab[i].len;
      this.vocab[i].word[0..#this.vocab[i].len] = vocabContext.vocab[i].word[0..#this.vocab[i].len];
      this.vocab[i].cn = vocabContext.vocab[i].cn;
    }
    this.vocab_hash[0..#vocab_hash_size] = vocabContext.vocab_hash[0..#vocab_hash_size];
    this.vocabTreeDomain = {vocabContext.vocabTreeDomain.low..vocabContext.vocabTreeDomain.high};
    this.vocab_tree[vocabTreeDomain] = vocabContext.vocab_tree[vocabTreeDomain];
    this.train_words = vocabContext.train_words;
    this.min_reduce = vocabContext.min_reduce;
    this.table[0..#table_size] = vocabContext.table[0..#table_size];
  }*/

  proc InitUnigramTable() {
    var a, i: int;
    var d1, train_words_pow: real;
    var power: real = 0.75;
    for a in 0..#vocab_size do train_words_pow += vocab[a].cn ** power;
    i = 0;
    d1 = (vocab[i].cn ** power) / train_words_pow;
    for a in 0..#table_size {
      table[a] = i;
      if (a / table_size:real > d1) {
        i += 1;
        d1 += (vocab[i].cn ** power) / train_words_pow;
      }
      if (i >= vocab_size) then i = vocab_size - 1;
    }
  }

  inline proc readNextChar(ref ch: uint(8), reader): bool {
    if (atCRLF) {
      atCRLF = false;
      ch = CRLF;
      return true;
    }
    return reader.read(ch);
  }

  proc ReadWord(word: [?] uint(8), reader): int {
    var a: int;
    var ch: uint(8);

    while readNextChar(ch, reader) {
      if (ch == 13) then continue;
      if ((ch == SPACE) || (ch == TAB) || (ch == CRLF)) {
        if (a > 0) {
          // Readers do not have ungetc, so Simulate ungetc using the atCRLF flag
          if (ch == CRLF) then atCRLF = true;
          break;
        }
        if (ch == CRLF) then return writeSpaceWord(word);
                        else continue;
      }
      word[a] = ch;
      a += 1;
      if (a >= MAX_STRING - 1) then a -= 1; // Truncate too long words
    }
    return a;
  }

  inline proc GetWordHash(word: [?] uint(8), len: int): int {
    if (len == 1) {
      return word[0]: int;
    } else if (len == 2) {
      return (word[0] * 257 + word[1]): int;
    }
    var hash: uint;
    for ch in 0..#len do hash = hash * 257 + word[ch]: uint;
    return (hash & vocab_hash_size_mask): int;
  }

  // Returns position of a word in the vocabulary; if the word is not found, returns -1
  proc SearchVocab(word: [?D] uint(8), len: int): int {
    var hash = GetWordHash(word, len);

    while (1) {
      if (vocab_hash[hash] == -1) then return -1;
      const wordIdx = vocab_hash[hash];
      if (len == vocab[wordIdx].len) {
        var found = true;
        for i in 0..#len {
          if (word[i] != vocab[wordIdx].word[i]) {
            found = false;
            break;
          }
        }
        if found then return vocab_hash[hash];
      }
      hash = (hash + 1) & vocab_hash_size_mask;
    }

    return -1;
  }

  proc ReadWordIndex(reader): int {
    var word: [0..#MAX_STRING] uint(8);
    var len = ReadWord(word, reader);
    if (len == 0) then return -2;
    return SearchVocab(word, len);
  }

  // Adds a word to the vocabulary
  proc AddWordToVocab(word: [?D] uint(8), length: int): int {
    var len = if (length > MAX_STRING) then MAX_STRING else length;
    const wordIdx = vocab_size;
    for i in 0..#len do vocab[wordIdx].word[i] = word[i];
    vocab[wordIdx].len = len;
    vocab[wordIdx].cn = 0;
    vocab_size += 1;
    // Reallocate memory if needed
    if (vocab_size + 2 >= vocab_max_size) {
      // TODO: research if the original += 1000 is adequate performance-wise
      vocab_max_size *= 2;
      vocabDomain = {0..#vocab_max_size};
    }
    var hash = GetWordHash(word, len);
    while (vocab_hash[hash] != -1) {
      hash = (hash + 1) & vocab_hash_size_mask;
    }
    vocab_hash[hash] = vocab_size - 1;
    return vocab_size - 1;
  }

  inline proc chpl_sort_cmp(a, b, param reverse=false, param eq=false) {
    if eq {
      if reverse then return a >= b;
      else return a <= b;
    } else {
      if reverse then return a > b;
      else return a < b;
    }
  }

  proc InsertionSort(Data: [?Dom] VocabEntry, doublecheck=false, param reverse=false) where Dom.rank == 1 {
    const lo = Dom.low;
    for i in Dom {
      const ithVal = Data(i);
      var inserted = false;
      for j in lo..i-1 by -1 {
        if (chpl_sort_cmp(ithVal.cn, Data(j).cn, reverse)) {
          Data(j+1) = Data(j);
        } else {
          Data(j+1) = ithVal;
          inserted = true;
          break;
        }
      }
      if (!inserted) {
        Data(lo) = ithVal;
      }
    }
  }

  proc QuickSort(Data: [?Dom] VocabEntry, minlen=7, doublecheck=false, param reverse=false) where Dom.rank == 1 {
    // grab obvious indices
    const lo = Dom.low,
          hi = Dom.high,
          mid = lo + (hi-lo+1)/2;

    // base case -- use insertion sort
    if (hi - lo < minlen) {
      InsertionSort(Data, reverse=reverse);
      return;
    }

    // find pivot using median-of-3 method
    if (chpl_sort_cmp(Data(mid).cn, Data(lo).cn, reverse)) then Data(mid) <=> Data(lo);
    if (chpl_sort_cmp(Data(hi).cn, Data(lo).cn, reverse)) then Data(hi) <=> Data(lo);
    if (chpl_sort_cmp(Data(hi).cn, Data(mid).cn, reverse)) then Data(hi) <=> Data(mid);
    const pivotVal = Data(mid);
    Data(mid) = Data(hi-1);
    Data(hi-1) = pivotVal;
    // end median-of-3 partitioning

    var loptr = lo,
        hiptr = hi-1;
    while (loptr < hiptr) {
      do { loptr += 1; } while (chpl_sort_cmp(Data(loptr).cn, pivotVal.cn, reverse));
      do { hiptr -= 1; } while (chpl_sort_cmp(pivotVal.cn, Data(hiptr).cn, reverse));
      if (loptr < hiptr) {
        Data(loptr) <=> Data(hiptr);
      }
    }

    Data(hi-1) = Data(loptr);
    Data(loptr) = pivotVal;

    //  cobegin {
      QuickSort(Data[..loptr-1], reverse=reverse);  // could use unbounded ranges here
      QuickSort(Data[loptr+1..], reverse=reverse);
      //  }
  }

  proc SortVocab() {
    var a, size, hash: int;

    // Sort the vocabulary and keep </s> at the first position
    QuickSort(vocab[1..], vocab_size - 1, reverse=true);
    for a in 0..#vocab_hash_size do vocab_hash[a] = -1;
    size = vocab_size;
    train_words = 0;
    for a in 0..#size {
      // Words occuring less than min_count times will be discarded from the vocab
      if ((vocab[a].cn < min_count) && (a != 0)) {
        vocab_size -= 1;
        vocab[a].len = 0;
        vocab[a].cn = 0;
      } else {
        // Hash will be re-computed, as after the sorting it is not actual
        hash = GetWordHash(vocab[a].word, vocab[a].len);
        while (vocab_hash[hash] != -1) do hash = (hash + 1) & vocab_hash_size_mask;
        vocab_hash[hash] = a;
        train_words += vocab[a].cn;
      }
    }
    vocabDomain = {0..#(vocab_size + 1)};
    // Allocate memory for the binary tree construction
    vocabTreeDomain = vocabDomain;
  }

  proc ReduceVocab() {
    var a, b: int;
    for a in 0..#vocab_size do if (vocab[a].cn > min_reduce) {
      vocab[b].cn = vocab[a].cn;
      vocab[b].word = vocab[a].word;
      b += 1;
    } else {
      vocab[a].len = 0;
      vocab[a].cn = 0;
    }
    vocab_size = b;
    for a in 0..#vocab_hash_size do vocab_hash[a] = -1;
    for a in 0..#vocab_size {
      // Hash will be re-computed, as it is not actual
      var hash = GetWordHash(vocab[a].word, vocab[a].len);
      while (vocab_hash[hash] != -1) do hash = (hash + 1) & vocab_hash_size_mask;
      vocab_hash[hash] = a;
    }
    min_reduce += 1;
  }

  proc CreateBinaryTree() {
    var b, i, min1i, min2i, pos1, pos2: int(64);
    var point: [0..#MAX_CODE_LENGTH] int(64);
    var code: [0..#MAX_CODE_LENGTH] uint(8);
    var dom = {0..#(vocab_size*2 + 1)};
    var count: [dom] int(64);
    var binary: [dom] int(64);
    var parent_node: [dom] int(64);
    count = 1e15: int(64);
    for a in 0..#vocab_size do count[a] = vocab[a].cn;
    pos1 = vocab_size - 1;
    pos2 = vocab_size;
    // Following algorithm constructs the Huffman tree by adding one node at a time
    for a in 0..#(vocab_size-1) {
      // First, find two smallest nodes 'min1, min2'
      if (pos1 >= 0) {
        if (count[pos1] < count[pos2]) {
          min1i = pos1;
          pos1 -= 1;
        } else {
          min1i = pos2;
          pos2 += 1;
        }
      } else {
        min1i = pos2;
        pos2 += 1;
      }
      if (pos1 >= 0) {
        if (count[pos1] < count[pos2]) {
          min2i = pos1;
          pos1 -= 1;
        } else {
          min2i = pos2;
          pos2 += 1;
        }
      } else {
        min2i = pos2;
        pos2 += 1;
      }
      count[vocab_size + a] = count[min1i] + count[min2i];
      parent_node[min1i] = vocab_size + a;
      parent_node[min2i] = vocab_size + a;
      binary[min2i] = 1;
    }
    // Now assign binary code to each vocabulary word
    for a in 0..#vocab_size {
      b = a;
      i = 0;
      while (1) {
        code[i] = binary[b]: uint(8);
        point[i] = b;
        i += 1;
        b = parent_node[b];
        if (b == vocab_size * 2 - 2) then break;
      }
      vocab_tree[a].codelen = i: uint(8);
      vocab_tree[a].point[0] = vocab_size - 2;
      for b in 0..#i {
        vocab_tree[a].code[i - b - 1] = code[b];
        vocab_tree[a].point[i - b] = point[b] - vocab_size;
      }
    }
  }

  proc SaveVocab(save_vocab_file) {
    var f = open(save_vocab_file, iomode.cw);
    var w = f.writer(locking=false);
    for i in 0..#vocab_size {
      for j in 0..#vocab[i].len do w.writef("%c", vocab[i].word[j]);
      w.writeln(" ", vocab[i].cn);
    }
    w.close();
    f.close();
  }

  proc ReadVocab(read_vocab_file) {
    var a: int(64);
    var cn: int;
    var c: uint(8);
    var word: [0..#MAX_STRING] uint(8);

    var f = open(read_vocab_file, iomode.r);
    /*if (fin == NULL) {
      printf("Vocabulary file not found\n");
      exit(1);
    }*/
    var r = f.reader(kind=ionative);

    vocab_hash = -1;
    vocab_size = 0;
    train_words = 0;

    while (1) {
      var len = ReadWord(word, r);
      if (len == 0) then break;
      a = AddWordToVocab(word, len);
      // read and compute word count
      len = ReadWord(word, r);
      if (len == 0) then break;
      vocab[a].cn = wordToInt(word, len);
      train_words += vocab[a].cn;
      // skip CRLF
      ReadWord(word, r);
    }

    r.close();
    f.close();

    if (override_train_words > 0) {
      writeln("overriding train words with ", override_train_words);
      train_words = override_train_words;
    }

    // NOTE: we don't SortVocab here because the vocab is already sorted when read
    if (debug_mode > 0) {
      writeln("Vocab size: ", vocab_size);
      writeln("Words in train file: ", train_words);
    }
    vocabTreeDomain = vocabDomain;
  }

  proc LearnVocabFromTrainFile(train_file) {
    var word: [0..#MAX_STRING] uint(8);
    var i: int(64);
    var len: int;
    for a in 0..#vocab_hash_size do vocab_hash[a] = -1;
    var f = open(train_file, iomode.r);
    /*if (fin == NULL) {
      printf("ERROR: training data file not found!\n");
      exit(1);
    }*/
    var r = f.reader(kind=ionative, locking=false);
    vocab_size = 0;
    writeSpaceWord(word);
    AddWordToVocab(word, 4);
    while (1) {
      len = ReadWord(word, r);
      if (len == 0) then break;
      train_words += 1;
      if (debug_mode > 0 && (train_words % 100000 == 0)) {
        write(train_words / 1000, "K\r");
        stdout.flush();
      }
      i = SearchVocab(word, len);
      if (i == -1) {
        var a = AddWordToVocab(word, len);
        vocab[a].cn = 1;
      } else {
        vocab[i].cn += 1;
      }
      if (vocab_size > vocab_hash_size * 0.7) then ReduceVocab();
    }
    SortVocab();
    if (debug_mode > 0) {
      info("Vocab size: ", vocab_size);
      info("Words in train file: ", train_words);
    }
    r.close();
    f.close();
  }

  // Utilities

  inline proc writeSpaceWord(word): int {
    word[0] = ascii('<');
    word[1] = ascii('/');
    word[2] = ascii('s');
    word[3] = ascii('>');
    word[4] = 0;
    return 4;
  }

  inline proc wordToInt(word: [?] uint(8), len: int): int {
    var cn = 0;
    var x = 1;
    for i in 0..#len by -1 {
      cn += x * (word[i] - 48);
      x *= 10;
    }
    return cn;
  }
}

class ModelTaskContext {
  var trainFileName: string;
  var id: int;
  var tid: int;
  var total_iterations: int;
  var current_iteration: int;
  var next_random: uint(64) = tid: uint(64);
  var word_count: uint(64);
  var last_word_count: uint(64);
  var trainFile = open(trainFileName, iomode.r);
  const fileChunkSize = trainFile.length() / numComputeLocales;
  const taskFileChunkSize = fileChunkSize / num_threads;
  const seekStart = fileChunkSize * id + taskFileChunkSize * tid;
  const seekStop = seekStart + taskFileChunkSize;
  var reader = trainFile.reader(kind = ionative, start=seekStart, end=seekStop, locking=false);
  var statsTimer: Timer;

  proc init() {
    reader.lock();
    reader._mark();
  }

  proc reset() {
    reader._revert();
    init();
  }

  proc close() {
    reader._revert();
    reader.close();
    trainFile.close();
  }

  proc isDone(): bool {
    return current_iteration >= total_iterations;
  }

  proc startStats() {
    statsTimer.clear();
    statsTimer.start();
  }

  proc pauseStats() {
    statsTimer.stop();
  }

  proc resumeStats() {
    statsTimer.start();
  }
}

class NetworkContext {
  var vocabContext: VocabContext;
  var layer1_size: int;
  var hs: int;
  var negative: int;
  var alpha: real;

  const starting_alpha: real = alpha;

  const vocab_size = vocabContext.vocab_size;
  const train_words = vocabContext.train_words;
  const max_locale_words = (train_words * iterations) / numComputeLocales;

  var ssyn0Domain: domain(2);
  var ssyn0: [ssyn0Domain] real;
  var ssyn1Domain: domain(2);
  var ssyn1: [ssyn1Domain] real;
  var ssyn1degDomain: domain(2);
  var ssyn1neg: [ssyn1degDomain] real;

  const syn0Domain = {0..#1,0..#(vocab_size*layer1_size)};
  var syn0: [syn0Domain] elemType;
  const syn1Domain = if (hs) then syn0Domain else {0..#1,0..#1};
  var syn1: [syn1Domain] elemType;
  const syn1negDomain = if (negative) then syn0Domain else {0..#1,0..#1};
  var syn1neg: [syn1negDomain] elemType;

  var expTable: [0..#(EXP_TABLE_SIZE+1)] real;

  const LayerSpace = {0..#layer1_size};

  var localCacheDomain: domain(2) = syn0Domain;
  var localCache: [localCacheDomain] elemType;

  proc InitExpTable() {
    for i in 0..#EXP_TABLE_SIZE {
      expTable[i] = exp((i / EXP_TABLE_SIZE:real * 2 - 1) * MAX_EXP); // Precompute the exp() table
      expTable[i] = expTable[i] / (expTable[i] + 1);                   // Precompute f(x) = x / (x + 1)
    }
  }

  proc RandomizeNetwork() {
    var next_random: uint(64) = 1;
    for a in 0..#vocab_size {
      for b in LayerSpace {
        next_random = next_random * 25214903917:uint(64) + 11;
        syn0[0,a * layer1_size + b] = ((((next_random & 0xFFFF) / 65536:elemType) - 0.5) / layer1_size):elemType;
      }
    }
  }

  proc Init() {
    InitExpTable();
    RandomizeNetwork();
  }

  proc initWith(network: NetworkContext) {
    copy(network, network.syn0Domain);
    InitExpTable();
  }

  proc copy(networkContext: NetworkContext, remdom) {
    const dom = {remdom.dim(1), remdom.dim(2)};

    localCache[syn0Domain] = networkContext.syn0[syn0Domain];
    local this.syn0[dom] = localCache[dom];

    if hs {
      localCache[syn1Domain] = networkContext.syn1[syn1Domain];
      local this.syn1[dom] = localCache[dom];
    }

    if negative {
      localCache[syn1negDomain] = networkContext.syn1neg[syn1negDomain];
      local this.syn1neg[dom] = localCache[dom];
    }
  }

  proc computeGradient(reference: NetworkContext) {
    if (reference.locale.id != here.id) then halt("reference.locale.id != here.id");

    local syn0[syn0Domain] -= reference.syn0[syn0Domain];
    if (hs) then {
      local syn1[syn1Domain] -= reference.syn1[syn1Domain];
    }
    if (negative) then {
      local syn1neg[syn1negDomain] -= reference.syn1neg[syn1negDomain];
    }
  }

  proc update(latest: NetworkContext, remdom, id, locale_word_count) {
    const dom = {remdom.dim(1), remdom.dim(2)};

    if (localCacheDomain.low > dom.low) then halt("localCacheDomain.low > dom.low", localCacheDomain);
    if (localCacheDomain.high < dom.high) then halt("localCacheDomain.high < dom.high ", localCacheDomain);
    if (syn0Domain.low > dom.low) then halt("syn0Domain.low > dom.low ", syn0Domain);
    if (syn0Domain.high < dom.high) then halt("syn0Domain.high < dom.high ", syn0Domain);

    var adaAlpha: [{dom.dim(1), remdom.dim(2).translate(1)}] elemType;
    var zAdaAlpha: [dom] => adaAlpha;

    {
      // speed up the local operations by first coping the entire array over the the current local before we do math
      localCache[syn0Domain] = latest.syn0[syn0Domain];
      local {
        ssyn0[dom] += localCache[dom] ** 2;
        adaAlpha = (1.0 / ((1.0 / update_alpha) * (update_delta + sqrt(ssyn0)))):elemType;
        syn0[dom] += localCache[dom] * zAdaAlpha[dom];
      }
    }
    if (hs) then {
      localCache[syn1Domain] = latest.syn1[syn1Domain];
      local {
        ssyn1[dom] += localCache[dom] ** 2;
        adaAlpha = (1.0 / ((1.0 / update_alpha) * (update_delta + sqrt(ssyn1)))):elemType;
        syn1[dom] += localCache[dom] * zAdaAlpha[dom];
      }
    }
    if (negative) then {
      localCache[syn1negDomain] = latest.syn1neg[syn1negDomain];
      local {
        ssyn1neg[dom] += localCache[dom] ** 2;
        adaAlpha = (1.0 / ((1.0 / update_alpha) * (update_delta + sqrt(ssyn1neg)))):elemType;
        syn1neg[dom] += localCache[dom] * zAdaAlpha[dom];
      }
    }
  }

  proc TrainModelThread(mt: ModelTaskContext) {
    var a, b, d, cw, word, last_word, sentence_length, sentence_position: int(64);
    var sen: [0..#(MAX_SENTENCE_LENGTH + 1)] int;
    var l1, l2, c, target, labelx: int(64);
    var f, g: real;
    var atEOF = false;

    var neu1: [LayerSpace] elemType;
    var neu1e: [LayerSpace] elemType;

    local while (1) {
      if (mt.word_count - mt.last_word_count > 10000) {
        mt.last_word_count = mt.word_count;
        alpha = starting_alpha * (1 - (mt.last_word_count:int * num_threads:int) / max_locale_words:real);
        if (alpha < starting_alpha * min_alpha) then alpha = starting_alpha * min_alpha;
      }
      if (sentence_length == 0) {
        while (1) {
          word = vocabContext.ReadWordIndex(mt.reader);
          if (word == -2) {
            atEOF = true;
            break;
          }
          if (word == -1) then continue;
          mt.word_count += 1;
          if (word == 0) then break;
          // The subsampling randomly discards frequent words while keeping the ranking same
          if (sample > 0) {
            var ran = (sqrt(vocabContext.vocab[word].cn / (sample * train_words):real) + 1) * (sample * vocabContext.train_words):real / vocabContext.vocab[word].cn;
            mt.next_random = (mt.next_random * 25214903917:uint(64) + 11):uint(64);
            if (ran < (mt.next_random & 0xFFFF):real / 65536:real) then continue;
          }
          sen[sentence_length] = word;
          sentence_length += 1;
          if (sentence_length >= MAX_SENTENCE_LENGTH) then break;
        }
        sentence_position = 0;
      }
      if (atEOF) {
        mt.reset();
        mt.current_iteration += 1;
        break;
      }
      word = sen[sentence_position];
      if (word == -1) then continue;
      for c in LayerSpace {
        neu1[c] = 0:elemType;
        neu1e[c] = 0:elemType;
      }
      mt.next_random = (mt.next_random * 25214903917:uint(64) + 11):uint(64);
      b = (mt.next_random % window: uint(64)):int(64);
      if (cbow) {  //train the cbow architecture
        // in -> hidden
        cw = 0;
        for a in b..(window * 2 - b) do if (a != window) {
          c = sentence_position - window + a;
          if (c < 0) then continue;
          if (c >= sentence_length) then continue;
          last_word = sen[c];
          if (last_word == -1) then continue;
          for c in LayerSpace do neu1[c] += syn0[0,c + last_word * layer1_size];
          cw += 1;
        }
        if (cw) {
          for c in LayerSpace do neu1[c] /= cw:elemType;
          if (hs) then for d in 0..#vocabContext.vocab_tree[word].codelen {
            f = 0;
            l2 = vocabContext.vocab_tree[word].point[d] * layer1_size;
            // Propagate hidden -> output
            for c in LayerSpace do f += neu1[c] * syn1[0,c + l2];
            if (f <= -MAX_EXP) then continue;
            else if (f >= MAX_EXP) then continue;
            else f = expTable[((f + MAX_EXP) * (EXP_TABLE_SIZE / MAX_EXP / 2)):int];
            // 'g' is the gradient multiplied by the learning rate
            g = (1 - vocabContext.vocab_tree[word].code[d] - f) * alpha;
            // Propagate errors output -> hidden
            for c in LayerSpace do neu1e[c] += (g * syn1[0,c + l2]):elemType;
            // Learn weights hidden -> output
            for c in LayerSpace do syn1[0,c + l2] += (g * neu1[c]):elemType;
          }
          // NEGATIVE SAMPLING
          if (negative > 0) then for d in 0..#(negative + 1) {
            if (d == 0) {
              target = word;
              labelx = 1;
            } else {
              mt.next_random = (mt.next_random * 25214903917:uint(64) + 11):uint(64);
              target = vocabContext.table[((mt.next_random >> 16) % vocabContext.table_size:uint(64)):int];
              if (target == 0) then target = (mt.next_random % (vocab_size - 1):uint(64) + 1):int;
              if (target == word) then continue;
              labelx = 0;
            }
            l2 = target * layer1_size;
            f = 0;
            for c in LayerSpace do f += neu1[c] * syn1neg[0,c + l2];
            if (f > MAX_EXP) then g = (labelx - 1) * alpha;
            else if (f < -MAX_EXP) then g = (labelx - 0) * alpha;
            else g = (labelx - expTable[((f + MAX_EXP) * (EXP_TABLE_SIZE / MAX_EXP / 2)):int]) * alpha;
            for c in LayerSpace do neu1e[c] += (g * syn1neg[0,c + l2]):elemType;
            for c in LayerSpace do syn1neg[0,c + l2] += (g * neu1[c]):elemType;
          }
          // hidden -> in
          for a in b..(window * 2 - b) do if (a != window) {
            c = sentence_position - window + a;
            if (c < 0) then continue;
            if (c >= sentence_length) then continue;
            last_word = sen[c];
            if (last_word == -1) then continue;
            for c in LayerSpace do syn0[0,c + last_word * layer1_size] += neu1e[c];
          }
        }
      } else {  //train skip-gram
        for a in b..(window * 2 - b) do if (a != window) {
          c = sentence_position - window + a;
          if (c < 0) then continue;
          if (c >= sentence_length) then continue;
          last_word = sen[c];
          if (last_word == -1) then continue;
          l1 = last_word * layer1_size;
          for c in LayerSpace do neu1e[c] = 0:elemType;
          // HIERARCHICAL SOFTMAX
          if (hs) then for d in 0..#vocabContext.vocab_tree[word].codelen {
            f = 0;
            l2 = vocabContext.vocab_tree[word].point[d] * layer1_size;
            // Propagate hidden -> output
            for c in LayerSpace do f += syn0[0,c + l1] * syn1[0,c + l2];
            if (f <= -MAX_EXP) then continue;
            else if (f >= MAX_EXP) then continue;
            else f = expTable[((f + MAX_EXP) * (EXP_TABLE_SIZE / MAX_EXP / 2)):int];
            // 'g' is the gradient multiplied by the learning rate
            g = (1 - vocabContext.vocab_tree[word].code[d] - f) * alpha;
            // Propagate errors output -> hidden
            for c in LayerSpace do neu1e[c] += (g * syn1[0,c + l2]):elemType;
            // Learn weights hidden -> output
            for c in LayerSpace do syn1[0,c + l2] += (g * syn0[0,c + l1]):elemType;
          }
          // NEGATIVE SAMPLING
          if (negative > 0) then for d in 0..#negative {
            if (d == 0) {
              target = word;
              labelx = 1;
            } else {
              mt.next_random = (mt.next_random * 25214903917:uint(64) + 11):uint(64);
              target = vocabContext.table[((mt.next_random >> 16) % vocabContext.table_size:uint(64)):int];
              if (target == 0) then target = (mt.next_random % (vocab_size - 1):uint(64) + 1):int;
              if (target == word) then continue;
              labelx = 0;
            }
            l2 = target * layer1_size;
            f = 0;
            for c in LayerSpace do f += syn0[0,c + l1] * syn1neg[0,c + l2];
            if (f > MAX_EXP) then g = (labelx - 1) * alpha;
            else if (f < -MAX_EXP) then g = (labelx - 0) * alpha;
            else g = (labelx - expTable[((f + MAX_EXP) * (EXP_TABLE_SIZE / MAX_EXP / 2)):int]) * alpha;
            for c in LayerSpace do neu1e[c] += (g * syn1neg[0,c + l2]):elemType;
            for c in LayerSpace do syn1neg[0,c + l2] += (g * syn0[0,c + l1]):elemType;
          }
          // Learn weights input -> hidden
          for c in LayerSpace do syn0[0,c + l1] += neu1e[c];
        }
      }
      sentence_position += 1;
      if (sentence_position >= sentence_length) {
        sentence_length = 0;
        continue;
      }
    }
  }
}

proc writeResults(output_file: string, network: NetworkContext) {
  const vocabContext = network.vocabContext;
  const layer1_size = network.layer1_size;
  const LayerSpace = {0..#layer1_size};

  var outputFile = open(output_file, iomode.cw);
  var writer = outputFile.writer(locking=false);

  writer.writeln(vocabContext.vocab_size, " ", layer1_size);
  for a in 0..#vocabContext.vocab_size {
    for j in 0..#vocabContext.vocab[a].len {
      writer.writef("%c", vocabContext.vocab[a].word[j]);
    }
    writer.write(" ");
    if (binary) then for b in LayerSpace do writer.writef("%|4r", network.syn0[0,a * layer1_size + b]);
    else for b in LayerSpace do writer.write(network.syn0[0,a * layer1_size + b], " ");
    writer.writeln();
  }

  writer.close();
  outputFile.close();
}

// Run K-means on the word vectors
proc runKMeans(output_file: string, network: NetworkContext, classes: int) {
  const vocabContext = network.vocabContext;
  const layer1_size = network.layer1_size;
  const LayerSpace = {0..#layer1_size};

  var clcn = classes;
  var iterX = 10;
  var closeid: int;
  var centcn: [0..#classes] int;
  var cl: [0..#vocabContext.vocab_size] int;
  var closev, x: real;
  var cent: [0..#classes*layer1_size] real;

  var outputFile = open(output_file, iomode.cw);
  var writer = outputFile.writer(locking=false);

  for a in 0..#vocabContext.vocab_size do cl[a] = a % clcn;
  for a in 0..iterX {
    for b in 0..#(clcn * layer1_size) do cent[b] = 0;
    for b in 0..#clcn do centcn[b] = 1;
    for c in 0..#vocabContext.vocab_size {
      for d in LayerSpace do cent[layer1_size * cl[c] + d] += network.syn0[0,c * layer1_size + d];
      centcn[cl[c]] += 1;
    }
    for b in 0..#clcn {
      closev = 0;
      for c in LayerSpace {
        cent[layer1_size * b + c] /= centcn[b];
        closev += cent[layer1_size * b + c] * cent[layer1_size * b + c];
      }
      closev = sqrt(closev);
      for c in LayerSpace do cent[layer1_size * b + c] /= closev;
    }
    for c in 0..#vocabContext.vocab_size {
      closev = -10;
      closeid = 0;
      for d in 0..#clcn {
        x = 0;
        for b in LayerSpace do x += cent[layer1_size * d + b] * network.syn0[0,c * layer1_size + b];
        if (x > closev) {
          closev = x;
          closeid = d;
        }
      }
      cl[c] = closeid;
    }
  }
  // Save the K-means classes
  for a in 0..#vocabContext.vocab_size {
    for j in 0..#vocabContext.vocab[a].len do writer.writef("%c", vocabContext.vocab[a].word[j]);
    writer.write(" ");
    if (binary) then writer.writef("%|4i", cl[a]);
    else writer.write(cl[a]);
    writer.writeln();
  }

  writer.close();
  outputFile.close();
}

proc TrainModel() {
  var a, b, c, d: int;
  var t: Timer;

  info("Starting training using file ", train_file);
  var vocabContext = new VocabContext();
  vocabContext.loadFromFile(train_file, read_vocab_file, save_vocab_file, negative);
  if (output_file == "") then return;
  var network = new NetworkContext(vocabContext, layer1_size, hs, negative, alpha);
  network.Init();
  /*var referenceNetwork = new NetworkContext(vocabContext, layer1_size, hs, negative, alpha);
  referenceNetwork.initWith(network);*/

  var vocabArr: [PrivateSpace] VocabContext;
  var networkArr: [PrivateSpace] NetworkContext;
  var referenceNetworkArr: [PrivateSpace] NetworkContext;

  // run on a single locale using all threads available
  coforall loc in Locales do on loc {
    if here.id == 0 {
      vocabArr[here.id] = vocabContext;
    } else {
      vocabArr[here.id] = new VocabContext();
      vocabArr[here.id].loadFromFile(train_file.localize(), read_vocab_file.localize(), save_vocab_file.localize(), negative);
    }

    referenceNetworkArr[here.id] = new NetworkContext(vocabArr[here.id], layer1_size, hs, negative, alpha);
    referenceNetworkArr[here.id].initWith(network);

    if (here.id >= computeLocalesStart) {
      if here.id == 0 {
        networkArr[here.id] = network;
      } else {
        networkArr[here.id] = new NetworkContext(vocabArr[here.id], layer1_size, hs, negative, alpha);
        networkArr[here.id].initWith(network);
      }
    } else {
      referenceNetworkArr[here.id].ssyn0Domain = network.syn0Domain;
      referenceNetworkArr[here.id].ssyn1Domain = network.syn1Domain;
      referenceNetworkArr[here.id].ssyn1degDomain = network.syn1negDomain;
    }
  }

  const referenceNetwork = referenceNetworkArr[0];

  var taskContexts: [PrivateSpace] [0..#num_threads] ModelTaskContext;

  for loc in computeLocales do on loc {
    for tid in 0..#num_threads {
      const id = here.id - computeLocalesStart;
      taskContexts[here.id][tid] = new ModelTaskContext(train_file.localize(), id, tid, iterations);
      taskContexts[here.id][tid].init();
    }
  }

  const domSliceSize:int = ((network.vocab_size * layer1_size) / num_param_locales: real): int;

  // run on a single locale using all threads available
  coforall loc in computeLocales do on loc {
    const workerId:int = here.id;
    const mtc = taskContexts[workerId][0];
    mtc.startStats();
    mtc.pauseStats();

    while (!mtc.isDone()) {
      mtc.resumeStats();
      /*startVdebug("network");*/
      forall tid in 0..#num_threads {
        networkArr[workerId].TrainModelThread(taskContexts[workerId][tid]);
      }
      /*stopVdebug();*/
      mtc.pauseStats();

      var locale_word_count = (+ reduce taskContexts[workerId][0..#num_threads].last_word_count);
      /*info(taskContexts[workerId][0].last_word_count, ' ', locale_word_count, ' ', networkArr[workerId].max_locale_words);*/
      reportStats(mtc.statsTimer, locale_word_count, networkArr[workerId].max_locale_words, networkArr[workerId].alpha);

      /*startVdebug("update");*/
      for rid in 0..#num_param_locales {
        const subDomainStart = (rid * domSliceSize):int;
        const subSyn0Domain = {network.syn0Domain.dim(1), subDomainStart..#domSliceSize};
        networkArr[workerId].computeGradient(referenceNetworkArr[workerId]);
        on referenceNetworkArr[rid] do referenceNetworkArr[rid].update(networkArr[workerId], subSyn0Domain, workerId, locale_word_count);
      }
      /*stopVdebug();*/

      /*startVdebug("copy");*/
      for rid in 0..#num_param_locales {
        const subDomainStart = (rid * domSliceSize):int;
        const subSyn0Domain = {network.syn0Domain.dim(1), subDomainStart..#domSliceSize};
        networkArr[workerId].copy(referenceNetworkArr[rid], subSyn0Domain);
      }
      referenceNetworkArr[workerId].copy(networkArr[workerId], networkArr[workerId].syn0Domain);
      /*stopVdebug();*/

      if ((workerId == computeLocalesStart) && (save_interval > 0) && (mtc.current_iteration % save_interval == 0)) then on referenceNetwork {
        info("collecting intermediate results");
        for rid in 1..#(num_param_locales-1) {
          const subDomainStart = (rid * domSliceSize):int;
          const subSyn0Domain = {referenceNetwork.syn0Domain.dim(1), subDomainStart..#domSliceSize};
          referenceNetwork.copy(referenceNetworkArr[rid], subSyn0Domain);
        }
        info("saving intermediate results");
        writeResults(output_file.localize() + "." + mtc.current_iteration, referenceNetwork);
      }
    }
  }

  for loc in Locales[computeLocalesStart..] do on loc {
    for tid in 0..#num_threads {
      taskContexts[here.id][tid].close();
    }
  }

  info("collecting results");

  for rid in 1..#(num_param_locales-1) {
    const subDomainStart = (rid * domSliceSize):int;
    const subSyn0Domain = {network.syn0Domain.dim(1), subDomainStart:int..#domSliceSize};
    referenceNetworkArr[0].copy(referenceNetworkArr[rid], subSyn0Domain);
  }

  info("writing results");

  if (classes == 0) {
    writeResults(output_file, referenceNetwork);
  } else {
    runKMeans(output_file, referenceNetwork, classes);
  }
}

proc main() {
  TrainModel();
}
