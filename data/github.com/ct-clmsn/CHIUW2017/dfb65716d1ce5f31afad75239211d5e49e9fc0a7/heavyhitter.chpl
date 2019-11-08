use BitOps;
use Sort;
use Random;

const LG_MIN_MAPSIZE = 3;
const SAMPLE_SIZE = 1024;

enum OptionState {
  EMPTY,
  FULL
}

record Option {
  type T;
  var state:OptionState;
  var value:T;

  proc Option(type T, state=OptionState.EMPTY) {}
  proc Option(type T, value:T, state=OptionState.FULL) {}
}

record Row {
  type T;
  var item:T;
  var est, ub, lb:int;
  proc Row(type T, item:T, est, ub, lb) {}
}

record RowComparator {
  type T;
  proc RowComparator(type T) {}
}

proc RowComparator.compare(a, b) {
  return if a.est < b.est then -1 else if a.est > b.est then 1 else 0;
}

record QuickSelect {}

proc type QuickSelect.partition(arr, lo, hi) {
    var i = lo;
    var j = hi+1;
    var v = arr(lo);
    while(true) {
      while(arr(i) < v) {
        if i == hi {
          break;
        }

        i+=1;
      }

      while v < arr(j) {
        if j == lo {
          break;
        }
      }

      if i >= j {
        break;
      }

     var x = arr(i);
     arr(i) = arr(j);
     arr(j) = x;
  }

  var x = arr(lo);
  arr(lo) = arr(j);
  arr(j) = x;
  return j;
}

proc type QuickSelect.pick(arr, lo_, hi_, pivot) {
  var hi = hi_;
  var lo = lo_;
  while hi > lo {
    var j = QuickSelect.partition(arr, lo, hi);
    if j == pivot {
      return arr(pivot);
    }

    if j > pivot {
      hi = j - 1;
    }
    else {
      lo = j + 1;
    }
  }

  return arr(pivot);
}

enum ErrorType {
  NO_FALSE_NEG,
  NO_FALSE_POS
}

proc hash(key_:int) {
  var key = key_;
  key ^= key >> 33;
  key *= 0xff51afd7ed558ccd:int;
  key ^= key >> 33;
  key *= 0xc4ceb9fe1a85ec53:int;
  key ^= key >> 33;
  return key;
}

record ReversePurgeItemHashMap {
  type T;
  var loadfactor = 0.75;
  var drift_limit = 1024;
  var lglen:int;
  var loadthreshold:int;

  var keydom:domain(1);
  var keys:[keydom] T;

  var valdom:domain(1);
  var vals:[valdom] int;

  var statedom:domain(1);
  var states:[statedom] int;
  var numactive:int;
  var hashfunc:func(T, int);

  proc ReversePurgeItemHashMap(type T, mapsize:int, hashfunc_:func(T, int)) {
    hashfunc = hashfunc_;
    lglen = log2(mapsize);
    loadthreshold = (mapsize:real*loadfactor):int;
    keydom = {0..#mapsize};
    valdom = {0..#mapsize};
    statedom = {0..#mapsize};
    states = 0;
    vals = 0;
  }

  proc active(probe) {
    return states(probe) > 0;
  }

  proc hashprobe(k:T) {
    var arraymask = keydom.high-1;
    var probe = hash(hashKey(k)) & arraymask;
    while(states(probe) > 0 && !(keys(probe)==k)) {
      probe = (probe+1) & arraymask;
    }

    return probe;
  }

  proc this(k:T) {
    var probe = hashprobe(k);
    return if states(probe) > 0 then vals(probe) else 0;
  }

  proc hashKey(k:T) {
    return hashfunc(k);
  }

  proc adjustOrPutValue(k:T, adjustamt) {
    var arrmask = keydom.high-1;
    var probe = hash(hashKey(k)) & arrmask;
    var drift = 1;

    while( states(probe) != 0 && !(keys(probe) != k) ) { // && (probe < keys.domain.high) ) {
      probe = (probe+1) & arrmask;
      drift+=1;
    }

    if states(probe) == 0 {
      keys(probe) = k;
      vals(probe) = adjustamt;
      states(probe) = drift;
      numactive+=1;
    }
    else {
      vals(probe) += adjustamt;
    }
  }

  proc keepOnlyPositiveCounts() {
    var firstprobe = statedom.high;
    while(states(firstprobe) > 0) {
      firstprobe-=1;
    }

    for probe in 0..firstprobe {
      if states(probe) > 0 && vals(probe) <= 0 {
        hashDelete(probe);
        numactive-=1;
      }
    }

    for probe in firstprobe..statedom.high {
      if states(probe) > 0 && vals(probe) <= 0 {
        hashDelete(probe);
        numactive-=1;
      }
    }
  }

  proc adjustAllValuesBy(adjustamt) {
    for i in 0..valdom.high {
      vals(i) += adjustamt;
    }
  }

  proc getActiveKeys() {
    var retkeydom : domain(1) = {0..#numactive};
    var retkeys:[retkeydom] T;

    var j = 0;
    for i in 0..keydom.high {
      if active(i) {
        retkeys(j) = keys(i);
        j+=1;
      }
    }

    return retkeys;
  }

  proc getActiveValues() {
    var retdom : domain(1) = {0..numactive};
    var ret : [retdom] T;

    var j = 0;
    for i in 0..keydom.high {
      if active(i){
        ret(j) = vals(i);
         j+=1;
      }
    }

    return ret;
  }

  proc resize(newsize) {
    var oldkeylen = keydom.high;
    keydom = {0..newsize};
    valdom = {0..newsize};
    statedom = {0..newsize};
    loadthreshold = (newsize:real * loadfactor):int;
    lglen = ctz(newsize);
    numactive = 0;

    for i in 0..#oldkeylen {
      if(states(i) > 0) {
        adjustOrPutValue(keys(i), vals(i));
      }
    }
  }

  proc purge(samplesize) {
    var limit = min(samplesize, numactive);
    var numsamples = 0;
    var i = 0;
    var samples : [0..#limit] int;
    while numsamples < limit {
      if active(i) {
        samples(numsamples) = vals(i);
        numsamples+=1;
      }

      i+=1;
    }

    var val = QuickSelect.pick(samples, 0, numsamples-1, limit/2);
    adjustAllValuesBy(-1*val);
    keepOnlyPositiveCounts();
    return val;
  }

  proc hashDelete(deleteprobe_) {
    var deleteprobe = deleteprobe_;
    states(deleteprobe) = 0;
    var drift = 1;
    var arraymask = keydom.high;
    var probe = (deleteprobe + drift) & arraymask;

    while states(probe) != 0 {
      if states(probe) > drift {
        keys(deleteprobe) = keys(probe);
        vals(deleteprobe) = vals(probe);
        states(deleteprobe) = states(probe)- drift;
        states(probe) = 0;
        drift = 0;
        deleteprobe = probe;
      }

      probe = (probe+1) & arraymask;
      drift+=1;
    }
  }

  proc hashProbe(key:T) {
    var arraymask = keydom.high;
    var probe = hashfunc(key) & arraymask;
    while states(probe) > 0 && !(keys(probe) == key) {
      probe = (probe+1) & arraymask;
    }

    return probe;
  }

  iter these() {
    for i in keydom {
      yield (keys(i), vals(i), states(i), numactive);
    }
  }
}

record HeavyHitter {
  type T;
  var lgMaxMapSize:int;
  var curMapCap:int;
  var offset:int;
  var streamLen:int;
  var sampleSize:int;

  var hashmap:ReversePurgeItemHashMap(T);

  proc HeavyHitter(type T, maxmapsize:int, hashfunc:func(T,int)) {
    lgMaxMapSize = max(log2(maxmapsize), LG_MIN_MAPSIZE);
    var lgcurmapsz = LG_MIN_MAPSIZE;
    hashmap = new ReversePurgeItemHashMap(T, 1<<lgcurmapsz, hashfunc);
    curMapCap = hashmap.loadthreshold;
    var maxmapcap = min(SAMPLE_SIZE, ((1<<lgMaxMapSize)*hashmap.loadfactor):int);
    offset = 0;
    sampleSize = min(SAMPLE_SIZE, maxmapcap);
  }

  proc update(element:T) {
    update(element, 1);
  }

  proc update(element:T, count) {
    streamLen+=count;

    hashmap.adjustOrPutValue(element, count);

    if hashmap.numactive > curMapCap {
      if hashmap.lglen < lgMaxMapSize {
        hashmap.resize(2*hashmap.keydom.high);
        curMapCap = hashmap.loadthreshold;
      }
      else {
        offset += hashmap.purge(sampleSize);
      }
    }
  }

  proc update(elements:[?Delements]T, counts:[?Dcounts] int) {
    for (i,j) in zip(Delements, Dcounts) {
      update(elements(i), counts(j));
    }
  }

  proc merge(other) {
    var streamlen = streamLen + other.streamLen;
    for (k,v,s,na) in other.hashmap {
      update(k,v);
    }
    offset += other.offset;
    streamLen = streamlen;
  }

  proc estimate(item:T) {
    var itemCount = hashmap(item);
    return if itemCount > 0 then itemCount + offset else 0;
  }

  proc upperbound(item:T) {
    return hashmap(item) + offset;
  }

  proc lowerbound(item:T) {
    return hashmap(item);
  }

  proc sortItems(threshold, err) {
    var rowlist = new list(Row(T));

    if err == ErrorType.NO_FALSE_NEG {
      for (k,v,s,na) in hashmap {
        var est = estimate(k);
        var ub = upperbound(k);
        var lb = lowerbound(k);
        if ub >= threshold {
          var row = new Row(T, k, est, ub, lb);
          rowlist.append(row);
        }
      }
    }
    else {
      for (k,v,s,na) in hashmap {
        var est = estimate(k);
        var ub = upperbound(k);
        var lb = lowerbound(k);
        if lb >= threshold {
          var row = new Row(T, k, est, ub, lb);
          rowlist.append(row);
        }
      }
    }

    var cmp = new RowComparator(Row(T));
    var rl :[0..#rowlist.size] Row(T);

    for rli in rl.domain {
      rl(rli) = rowlist.pop_front();
    }

    sort(rl, cmp);

    return rowlist;
  }

  proc frequentitems(threshold, errt) {
    return sortItems(if threshold > offset then threshold else offset, errt);
  }

  proc frequentitems(errt) {
    return sortItems(offset, errt);
  }

}

proc +(a:HeavyHitter(?T), b:HeavyHitter(T)) {
  a.merge(b);
  return a;
}

// demo program

/*
proc main() {
  const uintcmp : func(int, int) = lambda(x:int) : int { return x; };

  var hh = new HeavyHitter(int, 100, uintcmp);
  var values : [0..100] int;

  for i in values.domain {
    values(i) = i;
    hh.update(values(i), 1);
    writeln(i);
  }

  writeln(hh.frequentitems(ErrorType.NO_FALSE_POS));
  writeln(hh.frequentitems(ErrorType.NO_FALSE_NEG));

  writeln("here");
}
*/
