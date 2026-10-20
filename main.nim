import std / [parseutils, strutils, sequtils, tables, math, rdstdin]

type
  LispError = object of CatchableError
  ExpKind = enum
    ekSymbol
    ekNumber
    ekFloat
    ekString 
    ekList
    ekProc
    ekLambda
  Exp = ref object
    case kind: ExpKind
    of ekSymbol: sym: string
    of ekNumber: num: int
    of ekFloat: flo: float
    of ekString: str: string
    of ekList: list: seq[Exp]
    of ekProc: pro: LispProc
    of ekLambda:
      params: seq[string]
      body: Exp
      closure: Env
  Env = ref object
    outer: Env
    bindings: Table[string, Exp]
  LispProc = proc (args: seq[Exp]): Exp
  List = seq[string]

var
  globalEnv: Env

proc `$`(exp: Exp): string
proc tokenize(chars: string): List
proc atom(token: string): Exp
proc readFromTokens(tokens: var List): Exp
proc parse(program: string): Exp
proc getFloat(exp: Exp): float
proc newSymbol(val: string): Exp
proc newNumber(val: int): Exp
proc newFloat(val: float): Exp
proc newList(val: seq[Exp]): Exp
proc newProc(val: LispProc): Exp
proc newLambda(params: seq[string], body: Exp, closure: Env): Exp
proc newNumericExp(val: float, isAllInt: bool): Exp
proc isEqual(a: Exp, b: Exp): bool
proc standardEnv(): Env
proc findEnv(env: Env, sym: string): Env
proc eval(x: Exp, env: Env): Exp
proc invoke(procedure: Exp, args: seq[Exp]): Exp
proc repl(prompt: string)

proc tokenize(chars: string): List =
  # 1. Loop through each line and remove any comments following a semicolon (;)
  var cleanedLines: seq[string] = @[]
  for line in chars.splitLines():
    let idx = line.find(';')
    let cleanLine = if idx != -1: line[0..<idx] else: line
    cleanedLines.add(cleanLine)
  
  # Combine lines safely with line breaks
  let joined = cleanedLines.join("\n")

  # 2. Token extraction
  var rawTokens: seq[string] = @[]
  var currentToken = ""
  var inString = false

  var i = 0
  while i < joined.len:
    let c = joined[i]

    if inString:
      # [Inside a string literal]
      currentToken.add(c)
      if c == '"':
        rawTokens.add(currentToken)
        currentToken = ""
        inString = false
    else:
      if c == '"':
        if currentToken.len > 0:
          rawTokens.add(currentToken)
          currentToken = ""
        currentToken.add(c)
        inString = true
      elif c in {' ', '\t', '\r', '\n'}:
        if currentToken.len > 0:
          rawTokens.add(currentToken)
          currentToken = ""
      elif c == '(' or c == ')':
        if currentToken.len > 0:
          rawTokens.add(currentToken)
          currentToken = ""
        rawTokens.add($c)
      else:
        currentToken.add(c)
    i += 1

  # Process any remaining tokens
  if currentToken.len > 0:
    rawTokens.add(currentToken)

  # Raise an error if a double quotation mark is not closed
  if inString:
    raise newException(LispError, "unclosed string literal")

  # Exclude empty tokens
  var res: List = @[]
  for t in rawTokens:
    if t != "":
      res.add(t)
  return res

proc atom(token: string): Exp =
  # 1. Attempt to parse a string
  if token.startsWith("\"") and token.endsWith("\"") and token.len >= 2:
    return Exp(kind: ekString, str: token[1..^2])

  # 2. Attempt to parse an integer (Int)
  # Treat as an integer only if it does not contain '.' or 'e'
  if '.' notin token and 'e' notin token and 'E' notin token:
    var intVal: int
    try:
      let parsedLen = parseutils.parseInt(token, intVal, 0)
      if parsedLen == token.len and token.len > 0:
        return Exp(kind: ekNumber, num: intVal)
    except ValueError:
      var isAllDigits = true
      for i, c in token:
        if i == 0 and (c == '-' or c == '+'):
          continue
        if c notin {'0'..'9'}:
          isAllDigits = false
          break
      if isAllDigits and token.len > 0:
        raise newException(LispError,
          "integer overflow error: '" & token & "' is out of range for standard Int")

  # 3. Attempt to parse a real number (Float)
  var floatVal: float
  try:
    let parsedFloatLen = parseutils.parseFloat(token, floatVal, 0)
    if parsedFloatLen == token.len and token.len > 0:
      # Nim's `parseFloat` (with a C backend) returns `inf` or `-inf` upon overflow,
      # and can also result in an incorrect parse length; therefore, we safely check for both cases
      if floatVal == Inf or floatVal == -Inf:
        raise newException(LispError,
          "float overflow error: '" & token & "' is out of range for standard Float")
      return Exp(kind: ekFloat, flo: floatVal)
  except ValueError:
    var isFloatFormat = true
    for i, c in token:
      if i == 0 and (c == '-' or c == '+'):
        continue
      if c notin {'0'..'9', '.', 'e', 'E', '-', '+'}:
        isFloatFormat = false
        break
    if isFloatFormat and token.len > 0:
      raise newException(LispError,
        "float overflow error: '" & token & "' is out of range for standard Float")

  # 4. If the process reaches this point despite starting with a digit (meaning numeric parsing failed),
  # it indicates the presence of an invalid character.
  if token.len > 0 and token[0] in {'0'..'9'} and token[0] notin {'+', '-'}:
    raise newException(LispError,
      "invalid numeric format or symbol name '" & token & "'")

  # 5. If it's neither, then it's a symbol
  return Exp(kind: ekSymbol, sym: token)

proc parse(program: string): Exp =
  var tokens = tokenize(program)
  return readFromTokens(tokens)

proc readFromTokens(tokens: var List): Exp =
  if tokens.len == 0:
    raise newException(LispError, "unexpected EOF (empty program)")
  let token = tokens[0]
  tokens.delete(0)
  if token == "(":
    var L: seq[Exp]
    while tokens.len > 0 and tokens[0] != ")":
      L.add(readFromTokens(tokens))
    if tokens.len == 0:
      raise newException(LispError , "unexpected EOF (missing ')')")
    tokens.delete(0) # pop off ")"
    return Exp(kind: ekList, list: L)
  elif token == ")":
    raise newException(LispError, "unexpected EOF (unexpected ')')")
  else:
    return atom(token)

proc `$`(exp: Exp): string =
  return
    case exp.kind
    of ekSymbol: exp.sym
    of ekNumber: $exp.num
    of ekFloat: $exp.flo
    of ekString: "\"" & exp.str & "\""
    of ekList: "(" & exp.list.mapIt($it).join(", ") & ")"
    of ekProc: "<proc>"
    of ekLambda: "<lambda>"

proc getFloat(exp: Exp): float =
  return
    case exp.kind
    of ekNumber: exp.num.float
    of ekFloat:  exp.flo
    else: raise newException(LispError, "expected a number, but got: " & $exp)

proc newSymbol(val: string): Exp =
  return Exp(kind: ekSymbol, sym: val)

proc newNumber(val: int): Exp =
  return Exp(kind: ekNumber, num: val)

proc newFloat(val: float): Exp =
  return Exp(kind: ekFloat, flo: val)

proc newList(val: seq[Exp]): Exp =
  return Exp(kind: ekList, list: val)

proc newProc(val: LispProc): Exp =
  return Exp(kind: ekProc, pro: val)

proc newLambda(params: seq[string], body: Exp, closure: Env): Exp =
  return Exp(kind: ekLambda, params: params, body: body, closure: closure)

proc newNumericExp(val: float, isAllInt: bool): Exp =
  if isAllInt:
    return newNumber(val.int)
  return newFloat(val)

proc isEqual(a: Exp, b: Exp): bool =
  if a.kind != b.kind: return false
  case a.kind
  of ekSymbol: return a.sym == b.sym
  of ekNumber: return a.num == b.num
  of ekFloat:  return a.flo == b.flo
  of ekString:  return a.str == b.str
  of ekProc, ekLambda: return a == b
  of ekList:
    if a.list.len != b.list.len:
      return false
    for i in 0..<a.list.len:
      if not isEqual(a.list[i], b.list[i]):
        return false
    return true

proc standardEnv(): Env =
  var env = Env(bindings: initTable[string, Exp]())

  env.bindings["+"] = newProc(proc(args: seq[Exp]): Exp =
    if args.len == 0: return newNumber(0)
    var sum = args[0].getFloat
    var allInt = args[0].kind == ekNumber

    for i in 1..<args.len:
      sum += args[i].getFloat
      if args[i].kind != ekNumber: allInt = false

    return newNumericExp(sum, allInt)
  )
  env.bindings["-"] = newProc(proc(args: seq[Exp]): Exp =
    if args.len == 0:
      raise newException(LispError, "invalid number of arguments: 0")
    if args.len == 1: # Invert the sign if there is a single argument
      return newNumericExp(-args[0].getFloat, args[0].kind == ekNumber)

    var sum = args[0].getFloat
    var allInt = args[0].kind == ekNumber

    for i in 1..<args.len:
      sum -= args[i].getFloat
      if args[i].kind != ekNumber: allInt = false

    return newNumericExp(sum, allInt)
  )
  env.bindings["*"] = newProc(proc(args: seq[Exp]): Exp =
    if args.len == 0: return newNumber(1)
    var sum = args[0].getFloat
    var allInt = args[0].kind == ekNumber

    for i in 1..<args.len:
      sum *= args[i].getFloat
      if args[i].kind != ekNumber: allInt = false

    return newNumericExp(sum, allInt)
  )
  env.bindings["/"] = newProc(proc(args: seq[Exp]): Exp =
    if args.len == 0:
      raise newException(LispError, "invalid number of arguments: 0")
    if args.len == 1:
      if args[0].getFloat == 0.0:
        let val = if args[0].kind == ekNumber: newNumber(args[0].num) else: newFloat(args[0].flo)
        raise newException(LispError,
          "operation was (/ 1 " & $val & ")")
      return newNumericExp(1.0 / args[0].getFloat, args[0].kind == ekNumber)

    var sum = args[0].getFloat
    var allInt = args[0].kind == ekNumber

    for i in 1..<args.len:
      let val = if args[i].kind == ekNumber: newNumber(args[i].num) else: newFloat(args[i].flo)
      if val.getFloat == 0.0:
        let realSum = if allInt: newNumber(sum.int) else: newFloat(sum)
        raise newException(LispError,
          "operation was (/ " & $realSum & " " & $val & ")")
      sum /= args[i].getFloat
      if args[i].kind != ekNumber: allInt = false

    return newNumericExp(sum, allInt)
  )
  # Comparison operations (return 1 for true, 0 for false)
  # If there is only one argument, return 1
  env.bindings[">"] = newProc(proc(args: seq[Exp]): Exp =
    # Example: (> 3 2 1) -> 1, (> 3 2 10) -> 0
    if args.len == 0:
      raise newException(LispError, "> requires values as argument")
    if args.len == 1:
      return newNumber(1)
    for i in 0..<args.len-1:
      if args[i].getFloat <= args[i+1].getFloat:
        return newNumber(0)
    return newNumber(1)
  )
  env.bindings[">="] = newProc(proc(args: seq[Exp]): Exp =
    # Example: (>= 3 2 2) -> 1, (>= 3 2 10) -> 0
    if args.len == 0:
      raise newException(LispError, ">= requires values as argument")
    if args.len == 1:
      return newNumber(1)
    for i in 0..<args.len-1:
      if args[i].getFloat < args[i+1].getFloat:
        return newNumber(0)
    return newNumber(1)
  )
  env.bindings["<"] = newProc(proc(args: seq[Exp]): Exp =
    # Example: (< 1 2 3) -> 1, (< 1 3 2) -> 0
    if args.len == 0:
      raise newException(LispError, "< requires values as argument")
    if args.len == 1:
      return newNumber(1)
    for i in 0..<args.len-1:
      if args[i].getFloat >= args[i+1].getFloat:
        return newNumber(0)
    return newNumber(1)
  )
  env.bindings["<="] = newProc(proc(args: seq[Exp]): Exp =
    # Example: (<= 1 2 2) -> 1, (<= 1 3 2) -> 0
    if args.len == 0:
      raise newException(LispError, "<= requires values as argument")
    if args.len == 1:
      return newNumber(1)
    for i in 0..<args.len-1:
      if args[i].getFloat > args[i+1].getFloat:
        return newNumber(0)
    return newNumber(1)
  )
  env.bindings["="] = newProc(proc(args: seq[Exp]): Exp =
    if args.len == 0:
      raise newException(LispError, "= requires values as argument")
    if args.len == 1:
      return newNumber(1) # In SBCL, (= value) always returns T
    let firstVal = args[0].getFloat
    for i in 1..<args.len:
      if args[i].getFloat != firstVal:
        return newNumber(0)
    return newNumber(1)
  )
  # Built-in functions
  env.bindings["pi"] =
    newFloat(PI)
  env.bindings["sin"] = newProc(proc(args: seq[Exp]): Exp =
    if args.len != 1:
      raise newException(LispError, "sin requires exactly 1 argument")
    return newFloat(sin(args[0].getFloat))
  )
  env.bindings["cos"] = newProc(proc(args: seq[Exp]): Exp =
    if args.len != 1:
      raise newException(LispError, "cos requires exactly 1 argument")
    return newFloat(cos(args[0].getFloat))
  )
  env.bindings["sqrt"] = newProc(proc(args: seq[Exp]): Exp =
    if args.len != 1:
      raise newException(LispError, "sqrt requires exactly 1 argument")
    return newFloat(sqrt(args[0].getFloat))
  )
  env.bindings["abs"] = newProc(proc(args: seq[Exp]): Exp =
    if args.len != 1:
      raise newException(LispError, "abs requires exactly 1 argument")
    return newNumericExp(abs(args[0].getFloat), args[0].kind == ekNumber)
  )
  env.bindings["pow"] = newProc(proc(args: seq[Exp]): Exp =
    # pow: same as expt
    if args.len != 2:
      raise newException(LispError, "expt requires exactly 2 arguments")
    let base = args[0].getFloat
    let power = args[1].getFloat
    return newNumericExp(pow(base, power), args[0].kind == ekNumber and args[1].kind == ekNumber)
  )
  env.bindings["expt"] = newProc(proc(args: seq[Exp]): Exp =
    # expt: same as pow
    if args.len != 2:
      raise newException(LispError, "expt requires exactly 2 arguments")
    let base = args[0].getFloat
    let power = args[1].getFloat
    return newNumericExp(pow(base, power), args[0].kind == ekNumber and args[1].kind == ekNumber)
  )
  env.bindings["max"] = newProc(proc(args: seq[Exp]): Exp =
    # max: Returns the maximum value among the arguments
    # Example: (man 1 2 3) -> 3
    if args.len == 0:
      raise newException(LispError, "max requires at least 1 argument")
    var maxVal = args[0].getFloat
    var allInt = args[0].kind == ekNumber
    for i in 1..<args.len:
      let val = args[i].getFloat
      if val > maxVal:
        maxVal = val
      if args[i].kind != ekNumber:
        allInt = false
    return newNumericExp(maxVal, allInt)
  )
  env.bindings["min"] = newProc(proc(args: seq[Exp]): Exp =
    # min: Returns the minimum value among the arguments
    # Example: (min 10 1 20) -> 1
    if args.len == 0:
      raise newException(LispError, "min requires at least 1 argument")
    var minVal = args[0].getFloat
    var allInt = args[0].kind == ekNumber
    for i in 1..<args.len:
      let val = args[i].getFloat
      if val < minVal: minVal = val
      if args[i].kind != ekNumber: allInt = false
    return newNumericExp(minVal, allInt)
  )
  env.bindings["round"] = newProc(proc(args: seq[Exp]): Exp =
    # round: Round a number to the nearest value
    # Example: (round 3.5) -> 4.0, (round 3.14) -> 3.0
    if args.len != 1:
      raise newException(LispError, "round requires exactly 1 argument")
    return newNumericExp(round(args[0].getFloat), args[0].kind == ekNumber)
  )
  env.bindings["car"] = newProc(proc(args: seq[Exp]): Exp = 
    # car: Returns the first element of the list
    # Example: (car (list 1 2 3)) -> 1
    if args.len != 1 or args[0].kind != ekList or args[0].list.len == 0:
      raise newException(LispError, "car requires a non-empty list")
    return args[0].list[0]
  )
  env.bindings["cdr"] = newProc(proc(args: seq[Exp]): Exp =
    # cdr: Return the elements other than the first one as a new list
    # Example: (cdr '(1 2 3)) -> (2 3)
    if args.len != 1 or args[0].kind != ekList or args[0].list.len == 0:
      raise newException(LispError, "cdr requires a non-empty list")
    # Create a new list by slicing the elements from the first to the last
    return newList(args[0].list[1..^1])
  )
  env.bindings["cons"] = newProc(proc(args: seq[Exp]): Exp =
    # cons: Add an element to the beginning of the list
    # Example: (cons 1 '(2 3)) -> (1 2 3)
    if args.len != 2 or args[1].kind != ekList:
      raise newException(LispError, "cons requires an element and a list")
    var newListExp = @[args[0]]
    newListExp.add(args[1].list)
    return newList(newListExp)
  )
  env.bindings["append"] = newProc(proc(args: seq[Exp]): Exp =
    # append: Combine multiple lists to create a single new list
    # Example: (append (list 1 2) (list 10 20)) -> (1, 2, 10, 20)
    if args.len == 0:
      return newList(@[])
    var combined: seq[Exp] = @[]
    for arg in args:
      if arg.kind != ekList:
        raise newException(LispError, "append requires lists as arguments")
      combined.add(arg.list)
    return newList(combined)
  )
  env.bindings["null?"] = newProc(proc(args: seq[Exp]): Exp =
    # null?: Returns 1 (true) if the list is empty (contains 0 elements), and 0 (false) otherwise
    if args.len != 1 or args[0].kind != ekList:
      raise newException(LispError, "null? requires a list")
    return newNumber(if args[0].list.len == 0: 1 else: 0)
  )
  env.bindings["begin"] = newProc(proc(args: seq[Exp]): Exp =
    # begin: Evaluates the arguments in order, then evaluates and returns the final element
    args[^1]
  )
  env.bindings["print"] = newProc(proc(args: seq[Exp]): Exp =
    # print: Display the argument to standard output and return its value as is
    if args.len != 1:
      raise newException(LispError, "print requires exactly 1 argument")
    echo $args[0]
    return args[0]
  )
  env.bindings["eq?"] = newProc(proc(args: seq[Exp]): Exp =
    # eq?: same as =
    if args.len == 0:
      raise newException(LispError, "eq? requires values as argument")
    if args.len == 1:
      return newNumber(1)
    let firstVal = args[0].getFloat
    for i in 1..<args.len:
      if args[i].getFloat != firstVal:
        return newNumber(0)
    return newNumber(1)
  )
  env.bindings["equal?"] = newProc(proc(args: seq[Exp]): Exp =
    # equal?: Returns 1 if the two arguments are structurally equal, and 0 if they are not
    if args.len != 2:
      raise newException(LispError, "equal? requires exactly 2 arguments")
    return newNumber(if isEqual(args[0], args[1]): 1 else: 0)
  )
  env.bindings["length"] = newProc(proc(args: seq[Exp]): Exp =
    # length: eturns the number of elements in the list
    if args.len != 1:
      raise newException(LispError, "length requires exactly 1 argument")
    if args[0].kind != ekList:
      raise newException(LispError, "length requires a list as an argument")
    return newNumber(args[0].list.len)
  )
  env.bindings["list"] = newProc(proc(args: seq[Exp]): Exp =
    # list: Create a new list containing all the arguments as its elements
    return newList(args)
  )
  env.bindings["list?"] = newProc(proc(args: seq[Exp]): Exp =
    # list?: Returns 1 if the argument is a list, and 0 otherwise
    if args.len != 1:
      raise newException(LispError, "list? requires exactly 1 argument")
    return newNumber(if args[0].kind == ekList: 1 else: 0)
  )
  env.bindings["map"] = newProc(proc(args: seq[Exp]): Exp =
    # map: Returns a new list obtained by applying a procedure to each element of the list
    if args.len != 2:
      raise newException(LispError, "map requires exactly 2 arguments (a procedure and a list)")
    let procedure = args[0]
    let listExp = args[1]
    if listExp.kind != ekList:
      raise newException(LispError, "second argument to map must be a list")
    var mappedList: seq[Exp] = @[]
    for item in listExp.list:
      mappedList.add(invoke(procedure, @[item]))
    return newList(mappedList)
  )
  env.bindings["not"] = newProc(proc(args: seq[Exp]): Exp =
    # not: Returns 1 if the argument is a value considered "false," and 0 otherwise
    # Example: (not 0) -> 1, (not 1) -> 0
    if args.len != 1:
      raise newException(LispError, "not requires exactly 1 argument")
    # eval内の if と同じ基準で「真」かどうかを判定
    let isTrue = case args[0].kind
                 of ekNumber: args[0].num != 0
                 of ekFloat:  args[0].flo != 0.0
                 else: true
    # 真なら 0 (偽)、偽なら 1 (真) を返す
    return newNumber(if isTrue: 0 else: 1)
  )
  env.bindings["number?"] = newProc(proc(args: seq[Exp]): Exp =
    # number?: Returns 1 if the argument is a number (an integer or a floating-point number), and 0 otherwise
    # Example: (number? 42) -> 1, (number? 3.14) -> 1, (number? (quote a)) -> 0
    if args.len != 1:
      raise newException(LispError, "number? requires exactly 1 argument")
    let isNum = args[0].kind == ekNumber or args[0].kind == ekFloat
    return newNumber(if isNum: 1 else: 0)
  )
  env.bindings["procedure?"] = newProc(proc(args: seq[Exp]): Exp =
    # procedure?: Returns 1 if the argument is a built-in function or a lambda, and 0 otherwise
    # Example: (procedure? +) -> 1, (procedure? (lambda (x) x)) -> 1, (procedure? 42) -> 0
    if args.len != 1:
      raise newException(LispError, "procedure? requires exactly 1 argument")
    let isProc = args[0].kind == ekProc or args[0].kind == ekLambda
    return newNumber(if isProc: 1 else: 0)
  )
  env.bindings["symbol?"] = newProc(proc(args: seq[Exp]): Exp =
    # symbol?: Returns 1 if the argument is a symbol, and 0 otherwise
    # Example: (symbol? (quote a)) -> 1, (symbol? 42) -> 0
    if args.len != 1:
      raise newException(LispError, "symbol? requires exactly 1 argument")
    return newNumber(if args[0].kind == ekSymbol: 1 else: 0)
  )
  # REPL commands
  env.bindings[":load"] = newProc(proc(args: seq[Exp]): Exp =
    # load: Load and execute an external file (Lisp code)
    # Example: (load "foo.scm")
    if args.len != 1 or (args[0].kind != ekSymbol and args[0].kind != ekString):
      raise newException(LispError,
        "load requires exactly 1 argument (filename as a symbol or string)")
    let filename = if args[0].kind == ekSymbol: args[0].sym else: args[0].str
    try:
      let content = readFile(filename)
      # 複数個の式に対応するためにトークナイズ
      var tokens = tokenize(content)
      var lastResult: Exp = newSymbol("ok") # デフォルトの戻り値
      # トークンが残っている間、式を1つずつ切り出して eval を繰り返す
      while tokens.len > 0:
        let parsedExpr = readFromTokens(tokens)
        lastResult = eval(parsedExpr, env)
      return lastResult # 最後に評価した式の結果を返す
    except IOError:
      raise newException(LispError, "could not read file '" & filename & "'")
  )
  return env

# A helper function that searches for a variable (Symbol) within an environment
# (recursively searching parent environments as well)
proc findEnv(env: Env, sym: string): Env =
  var current = env
  while current != nil:
    if current.bindings.hasKey(sym):
      return current
    current = current.outer
  return nil

proc eval(x: Exp, env: Env): Exp =
  case x.kind
  of ekSymbol:
    let targetEnv = findEnv(env, x.sym)
    if targetEnv != nil:
      return targetEnv.bindings[x.sym]
    else:
      raise newException(LispError, "undefined symbol: " & x.sym)
  of ekNumber, ekFloat, ekString, ekProc, ekLambda:
    return x
  of ekList:
    if x.list.len == 0:
      return x
    let first = x.list[0]
    if first.kind == ekSymbol:
      # (if test conseq alt)
      if first.sym == "if":
        if x.list.len != 4:
          raise newException(LispError,
            "if requires exactly 3 arguments (test conseq alt), got " & $(x.list.len - 1))
        let test = x.list[1]
        let conseq = x.list[2]
        let alt = x.list[3]
        let testRes = eval(test, env)
        let isTrue = case testRes.kind
                     of ekNumber: testRes.num != 0
                     of ekFloat: testRes.flo != 0.0
                     of ekList: testRes.list.len > 0
                     else: true
        if isTrue:
          return eval(conseq, env)
        else:
          return eval(alt, env)
      # (define symbol exp)
      elif first.sym == "define":
        if x.list.len != 3:
          raise newException(LispError, "define requires exactly 2 argument")
        if x.list[1].kind != ekSymbol:
          raise newException(LispError, "define first argument must be a symbol")
        let symbol = x.list[1].sym
        let exp = x.list[2]
        env.bindings[symbol] = eval(exp, env)
        return newSymbol(symbol)
      # (quote exp)
      elif first.sym == "quote":
        if x.list.len != 2:
          raise newException(LispError, "quote requires exactly 1 argument")
        return x.list[1]
      # (set! symbol exp)
      elif first.sym == "set!":
        if x.list.len != 3:
          raise newException(LispError, "set! requires exactly 2 arguments")
        let symbol = x.list[1].sym
        let exp = x.list[2]
        let targetEnv = findEnv(env, symbol)
        if targetEnv != nil:
          targetEnv.bindings[symbol] = eval(exp, env)
          return newSymbol(symbol) # Return the variable name as the return value of `set!`
        else:
          raise newException(LispError, "undefined symbol for set!: " & symbol)
      # (lambda (params...) body)
      elif first.sym == "lambda":
        if x.list.len < 3:
          raise newException(LispError, "lambda requires at least a parameter list and a body")
        let paramList = x.list[1]
        if paramList.kind != ekList:
          raise newException(LispError, "lambda parameters must be a list")
        var params: seq[string] = @[]
        for p in paramList.list:
          if p.kind != ekSymbol:
            raise newException(LispError, "lambda parameter must be a symbol")
          params.add(p.sym)
        let body = x.list[2]
        return newLambda(params, body, env)
      # (apply proc list)
      elif first.sym == "apply":
        if x.list.len != 3:
          raise newException(LispError, "apply requires exactly 2 arguments (a procedure and a list)")
        let procedure = eval(x.list[1], env)
        let listExp = eval(x.list[2], env)
        if listExp.kind != ekList:
          raise newException(LispError, "second argument to apply must be a list")
        return invoke(procedure, listExp.list)

    let procedure = eval(x.list[0], env)
    # Evaluate all arguments
    var args: seq[Exp] = @[]
    for i in 1..<x.list.len:
      args.add(eval(x.list[i], env))
    return invoke(procedure, args)

proc invoke(procedure: Exp, args: seq[Exp]): Exp =
  if procedure.kind == ekProc:
    return procedure.pro(args)
  elif procedure.kind == ekLambda:
    if args.len != procedure.params.len:
      raise newException(LispError,
        "wrong number of arguments: expected " & $procedure.params.len & ", got " & $args.len)
    var newEnv = Env(outer: procedure.closure, bindings: initTable[string, Exp]())
    for i in 0..<procedure.params.len:
      newEnv.bindings[procedure.params[i]] = args[i]
    return eval(procedure.body, newEnv)
  else:
    raise newException(LispError, "not a procedure or lambda: " & $procedure)

proc repl(prompt: string) =
  globalEnv = standardEnv()
  echo "The standard environment has been initialized. Number of registrations: ", globalEnv.bindings.len
  var line: string
  while true:
    let ok = readLineFromStdin(prompt, line)
    if not ok: break # ctrl-C or ctrl-Z + ENTER will cause a break
    # Trim leading and trailing whitespaces
    let trimmed = line.strip()
    # Skip parsing if the input is empty or contains only spaces
    if trimmed.len == 0: 
      continue
    try:
      # Parse and evaluate the trimmed input string
      echo eval(parse(trimmed), globalEnv)
    except LispError, ValueError:
      let e = getCurrentException()
      echo "Error: ", e.msg
    except IndexDefect:
      # Safely display "index out of range" errors, such as when arguments are missing
      echo "Error: malformed list or missing arguments"

repl("lisnim> ")
echo "Goodbye boyeeee!"
