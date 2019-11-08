kLPar = 0x28
kRPar = 0x29
kQuote = 0x27

Nil = Origin mimic
kNil = Nil mimic

Num = Origin mimic
Num initialize = method(n, @data = n)

Sym = Origin mimic
Sym initialize = method(s, @data = s)

symTable = {"nil" => kNil}
makeSym = method(s,
  if(!symTable key?(s),
    symTable[s] = Sym mimic(s))
  symTable[s])

sym_t = makeSym("t")
sym_quote = makeSym("quote")
sym_if = makeSym("if")
sym_lambda = makeSym("lambda")
sym_defun = makeSym("defun")
sym_setq = makeSym("setq")

Error = Origin mimic
Error initialize = method(s, @data = s)

Cons = Origin mimic
Cons initialize = method(a, d,
  @car = a
  @cdr = d)
makeCons = method(a, d,
  Cons mimic(a, d))

Subr = Origin mimic
Subr call = method(args,
  Error mimic("invalid subr"))

Expr = Origin mimic
Expr initialize = method(a, b, e,
  @args = a
  @body = b
  @env = e)
makeExpr = method(args, env,
  Expr mimic(safeCar(args), safeCdr(args), env))

safeCar = method(obj,
  if(obj kind?("Cons"),
    obj car,
    kNil))

safeCdr = method(obj,
  if(obj kind?("Cons"),
    obj cdr,
    kNil))

nreverse = method(lst,
  ret = kNil
  while(lst kind?("Cons"),
    tmp = lst cdr
    lst cdr = ret
    ret = lst
    lst = tmp)
  ret)

pairlis = method(lst1, lst2,
  ret = kNil
  while(lst1 kind?("Cons") && lst2 kind?("Cons"),
    ret = makeCons(makeCons(lst1 car, lst2 car), ret)
    lst1 = lst1 cdr
    lst2 = lst2 cdr)
  nreverse(ret))

isSpace = method(c,
  c == 0x09 || c == 0x0a || c == 0x0d || c == 0x20)

isDelimiter = method(c,
  c == kLPar || c == kRPar || c == kQuote || isSpace(c))

skipSpaces = method(str,
  i = 0
  while(i < str length,
    if(!isSpace(str[i]),
      break)
    i++)
  str[i...str length])

isNumChar = method(c,
  0x30 <= c && c <= 0x39)  ; '0' <= c <= '9'

toNum = method(c,
  c - 0x30)  ; c - '0'

makeNumOrSym = method(str,
  i = 0
  sign = 1
  if(str[0] == 0x2d,  ; '-'
    sign = -1
    i = 1)
  is_num = false
  num = 0
  while(i < str length,
    if(isNumChar(str[i]),
      ;; then
      is_num = true
      num = num * 10 + toNum(str[i]),
      ;; else
      is_num = false
      break)
    i++)
  if(is_num,
    Num mimic(num * sign),
    makeSym(str)))

readAtom = method(str,
  next = ""
  i = 0
  while(i < str length,
    if(isDelimiter(str[i]),
      next = str[i...str length]
      str = str[0...i]
      break)
    i++)
  [makeNumOrSym(str), next])

read = method(str,
  str = skipSpaces(str)
  cond(
    str length == 0,
      [Error mimic("empty input"), ""],
    str[0] == kRPar,
      [Error mimic("invalid syntax: " + str), ""],
    str[0] == kLPar,
      readList(str[1...str length]),
    str[0] == kQuote,
      tmp = read(str[1...str length])
      [makeCons(sym_quote, makeCons(tmp[0], kNil)), tmp[1]],
    true,
      readAtom(str)))

readList = method(str,
  ret = kNil
  loop(
    str = skipSpaces(str)
    if(str length == 0,
      return [Error mimic("unfinished parenthesis"), ""])
    if(str[0] == kRPar,
      break)
    tmp = read(str)
    if(tmp[0] kind?("Error"),
      return tmp)
    ret = makeCons(tmp[0], ret)
    str = tmp[1])
  [nreverse(ret), str[1...str length]])

printObj = method(obj,
  cond(
    obj kind?("Nil"), "nil",
    obj kind?("Num"), obj data asText,
    obj kind?("Sym"), obj data,
    obj kind?("Error"), "<error: " + obj data + ">",
    obj kind?("Cons"), printList(obj),
    obj kind?("Subr"), "<subr>",
    obj kind?("Expr"), "<expr>",
    true, "<unknown>"))

printList = method(obj,
  ret = ""
  first = true
  while(obj kind?("Cons"),
    if(first,
      first = false,
      ret += " ")
    ret += printObj(obj car)
    obj = obj cdr)
  if(obj == kNil,
    "(" + ret + ")",
    "(" + ret + " . " + printObj(obj) + ")"))

findVar = method(sym, env,
  while(env kind?("Cons"),
    alist = env car
    while(alist kind?("Cons"),
      if(alist car car == sym,
        return alist car)
      alist = alist cdr)
    env = env cdr)
  kNil)

g_env = makeCons(kNil, kNil)

addToEnv = method(sym, val, env,
  env car = makeCons(makeCons(sym, val), env car))

eval1 = method(obj, env,
  if(obj kind?("Nil") || obj kind?("Num") || obj kind?("Error"),
    return obj)
  if(obj kind?("Sym"),
    bind = findVar(obj, env)
    if(bind == kNil,
      return Error mimic(obj data + " has no value"),
      return bind cdr))
  op = safeCar(obj)
  args = safeCdr(obj)
  if(op == sym_quote,
    return safeCar(args))
  if(op == sym_if,
    c = eval1(safeCar(args), env)
    cond(
      c kind?("Error"), return c,
      c == kNil, return eval1(safeCar(safeCdr(safeCdr(args))), env),
      true, return eval1(safeCar(safeCdr(args)), env)))
  if(op == sym_lambda,
    return makeExpr(args, env))
  if(op == sym_defun,
    expr = makeExpr(safeCdr(args), env)
    sym = safeCar(args)
    addToEnv(sym, expr, g_env)
    return sym)
  if(op == sym_setq,
    val = eval1(safeCar(safeCdr(args)), env)
    sym = safeCar(args)
    bind = findVar(sym, env)
    if(bind == kNil,
      addToEnv(sym, val, g_env),
      bind cdr = val)
    return val)
  apply(eval1(op, env), evlis(args, env)))

evlis = method(lst, env,
  ret = kNil
  while(lst kind?("Cons"),
    elm = eval1(lst car, env)
    if(elm kind?("Error"),
      return elm)
    ret = makeCons(elm, ret)
    lst = lst cdr)
  nreverse(ret))

progn = method(body, env,
  ret = kNil
  while(body kind?("Cons"),
    ret = eval1(body car, env)
    if(ret kind?("Error"),
      return ret)
    body = body cdr)
  ret)

apply = method(fn, args,
  cond(
    fn kind?("Error"), return fn,
    args kind?("Error"), return args,
    fn kind?("Subr"), return fn call(args),
    fn kind?("Expr"),
      return progn(fn body, makeCons(pairlis(fn args, args), fn env)),
    true, return Error mimic(printObj(fn) + " is not function")))

SubrCar = Subr mimic
SubrCar call = method(args,
  safeCar(safeCar(args)))

SubrCdr = Subr mimic
SubrCdr call = method(args,
  safeCdr(safeCar(args)))

SubrCons = Subr mimic
SubrCons call = method(args,
  makeCons(safeCar(args), safeCar(safeCdr(args))))

SubrEq = Subr mimic
SubrEq call = method(args,
  x = safeCar(args)
  y = safeCar(safeCdr(args))
  if(x kind?("Num") && y kind?("Num") && x data == y data,
    sym_t,
    if(x == y,
      sym_t,
      kNil)))

SubrAtom = Subr mimic
SubrAtom call = method(args,
  if(safeCar(args) kind?("Cons"), kNil, sym_t))

SubrNumberp = Subr mimic
SubrNumberp call = method(args,
  if(safeCar(args) kind?("Num"), sym_t, kNil))

SubrSymbolp = Subr mimic
SubrSymbolp call = method(args,
  if(safeCar(args) kind?("Sym"), sym_t, kNil))

SubrAddOrMul = Subr mimic
SubrAddOrMul call = method(args,
  ret = @init_val
  while(args kind?("Cons"),
    if(!(args car kind?("Num")),
      return Error mimic("wrong type"))
    ret = @calc(ret, args car data)
    args = args cdr)
  Num mimic(ret))
SubrAdd = SubrAddOrMul mimic
SubrAdd init_val = 0
SubrAdd calc = method(x, y, x + y)
SubrMul = SubrAddOrMul mimic
SubrMul init_val = 1
SubrMul calc = method(x, y, x * y)

SubrSubOrDivOrMod = Subr mimic
SubrSubOrDivOrMod call = method(args,
  x = safeCar(args)
  y = safeCar(safeCdr(args))
  if(!(x kind?("Num")) || !(y kind?("Num")),
    return Error miimc("wrong type"))
  Num mimic(@calc(x data, y data)))
SubrSub = SubrSubOrDivOrMod mimic
SubrSub calc = method(x, y, x - y)
SubrDiv = SubrSubOrDivOrMod mimic
SubrDiv calc = method(x, y, x / y)
SubrMod = SubrSubOrDivOrMod mimic
SubrMod calc = method(x, y, x % y)

addToEnv(makeSym("car"), SubrCar, g_env)
addToEnv(makeSym("cdr"), SubrCdr, g_env)
addToEnv(makeSym("cons"), SubrCons, g_env)
addToEnv(makeSym("eq"), SubrEq, g_env)
addToEnv(makeSym("atom"), SubrAtom, g_env)
addToEnv(makeSym("numberp"), SubrNumberp, g_env)
addToEnv(makeSym("symbolp"), SubrSymbolp, g_env)
addToEnv(makeSym("+"), SubrAdd, g_env)
addToEnv(makeSym("*"), SubrMul, g_env)
addToEnv(makeSym("-"), SubrSub, g_env)
addToEnv(makeSym("/"), SubrDiv, g_env)
addToEnv(makeSym("mod"), SubrMod, g_env)
addToEnv(sym_t, sym_t, g_env)

ireader = java:io:InputStreamReader new(java:lang:System field:in)
breader = java:io:BufferedReader new(ireader)
loop(
  "> " print
  line = breader readLine
  if(line == nil,
    break)
  line = line asText  ; Covert java.lang.String -> Text.
  printObj(eval1(read(line)[0], g_env)) println)
