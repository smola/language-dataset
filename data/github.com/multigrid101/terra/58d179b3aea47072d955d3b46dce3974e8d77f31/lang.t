-- implementation of the frontend (language-expression --> IR)
test = require('testsuite')

st = require('symboltable')
st = st.SymbolTable:get()


lang = {}


-------------------------------------------------------------------------------
-- Node Classes
-------------------------------------------------------------------------------
StencilNode = {}
function StencilNode:new(name, offsetx, offsety)
  local obj = {
         _name = name,
         _offsetx = offsetx,
         _offsety = offsety,
         _type = "StencilNode",
         }
  setmetatable(obj,self)
  self.__index = self

  return obj
end

bla = {}
terra bla.X(x:int ,y:int) return 1 end -- TODO need to define these programmatically when 'X' and 'Y' are some given strings
terra bla.Y(x:int ,y:int) return 1 end

-- test stuff start
-- test 1
print('\ntest 1')
a = symbol(int,'asdf')
function createexp()
  return `1
end

local exp = createexp()
print(exp)

-- stmt = quote [a] = [exp] end -- equivalent to below
stmt = quote a = exp end
print(stmt)

local terra generatedfn()
  var [a] = 5
  [stmt]
  return [a]
end
print(generatedfn)
print(generatedfn())

-- test 2
print('\ntest 2')
terra f(bla:int) return 2*bla  end
exp1 = `f
exp2 = `[exp1](5)
-- exp2 = `exp1(5) -- equivalent to above
terra doit()
  return [exp2]
end
print(doit)
print(doit())

-- test 3
print('\ntest 3')
struct mystruct { foo : int }
terra mystruct.metamethods.__apply(self:&mystruct, idx:int)
  return self.foo*idx
end


g = symbol(mystruct, 'g')

-- exp1 = `g
-- exp2 = `[exp1](5)
exp2 = `g(5)

terra doit()
  var [g]
  g.foo = 3
  return [exp2]
end
print(doit)
print(doit())
-- test stuff end


local x = symbol(int)
local y = symbol(int)
function StencilNode:toQuote()
  -- return `bla.[self._name]( [x] +[self._offsetx], [y]+[self._offsety])
  return `[st._data[self._name]._terrasym]( [x] +[self._offsetx], [y]+[self._offsety])
  -- return `applyfunc([self._name], [x], [y])
end

function StencilNode:asString()
  return self._name .. '(' .. self._offsetx .. ',' .. self._offsety .. ')'
end
StencilNode.__tostring = StencilNode.asString

-- tests begin
local foo = StencilNode:new('X', 2, 3)

-- terra stuff([x], [y])
--   return [foo:toQuote()]
-- end

-- print(stuff)
-- print(foo:toQuote())
-- tests end

lang.StencilNode = StencilNode
-------------------------------------------------------------------------------
local struct TerraImage { _data : &float }
terra TerraImage.metamethods.__apply(theimage:TerraImage, x:int, y:int)
  return theimage._data[555]
end
lang.TerraImage = TerraImage


-------------------------------------------------------------------------------
-- 'filename' is optional, will not be accessed for Unknown-pictures
-- TODO move this class somewhere else, its not really used in the IR
Image = {}
function Image:new(name, filename)
  local obj = {
         _name = name,
         _filename = filename,
         _terrasym = nil}
  setmetatable(obj, Image)
  Image.__index = Image

  st._data[name] = obj
  obj._terrasym = symbol(TerraImage, name)
  
  return obj
end

function Image:asString()
  return "(Image: " .. self._name .. ")"
end
Image.__tostring = Image.asString

function Image:call(x,y)
  return StencilNode:new(self._name, x, y)
end
Image.__call = Image.call

-- test start
theimage = Image:new('cool image')
print(theimage)
theimage(2,3)
-- test end


lang.Image = Image
-------------------------------------------------------------------------------
PlusNode = {}
function PlusNode:new(arg1, arg2)
  local obj = {
                _arg1 = arg1,
                _arg2 = arg2,
                _type = "PlusNode"}
  setmetatable(obj, self)
  self.__index = self

  return obj
end

function PlusNode:asString()
  return self._arg1:asString() .. ' + ' .. self._arg2:asString()
end
PlusNode.__tostring = PlusNode.asString

function PlusNode:toQuote()
  return `[self._arg1:toQuote()] + [self._arg2:toQuote()]
end

-- test start
print('')
image1 = Image:new('X')
image2 = Image:new('Y')
pnode = PlusNode:new(image1(1,2), image2(3,4))
print(image1)
print(image2)
print(pnode)

print('\nThe Symboltable:')
test.printtable(st._data)
test.printtable(st._data['X'])
test.printtable(st._data['Y'])

stmts = terralib.newlist()
stmts:insert{`[st._data['X']._terrasym]}
stmts:insert{`[st._data['Y']._terrasym]}

bla = symbol(int, 'bla')
terra eval_local_plus([x], [y])
  var [st._data['X']._terrasym]
  var [st._data['Y']._terrasym]
  return [pnode:toQuote()]
end
print(eval_local_plus)
-- test end


lang.PlusNode = PlusNode
-------------------------------------------------------------------------------
-- MultNode = {}
-- function MultNode:new(arg1, arg2)
--   local obj = {_arg1 = arg1, _arg2 = arg2}
--   setmetatable(obj, self)
--   self.__index = self

--   return obj
-- end

-- function MultNode:asString()
--   return self._arg1:asString() .. ' * ' .. self._arg2:asString()
-- end
-- MultNode.__tostring = MultNode.asString

-- function MultNode:toQuote()
--   return `[self._arg1:toQuote()] * [self._arg2:toQuote()]
-- end


-- -- test start
-- print('')
-- image1 = Image:new('X')
-- image2 = Image:new('Y')
-- node = MultNode:new(image1(2,4), image2(8,16))
-- print(image1)
-- print(image2)
-- print(node)

-- terra eval_local_mult([x], [y])
--   return [node:toQuote()]
-- end
-- print(eval_local_mult)
-- -- test end

-- lang.MultNode = MultNode

-- ------------------------------------------------------------------------------
-- -- test Node classes
-- ------------------------------------------------------------------------------
-- -- test1 start
-- print('')
-- image1 = Image:new('X')
-- image2 = Image:new('Y')
-- mnode1 = MultNode:new(image1(2,4), image2(8,16))
-- mnode2 = MultNode:new(image1(3,5), image2(9,17))
-- pnode = PlusNode:new(mnode1, mnode2)
-- print(pnode)

-- terra eval_local_complex1([x], [y])
--   return [pnode:toQuote()]
-- end
-- print(eval_local_complex1)
-- -- test1 end

-- -- test2 start
-- print('')
-- image1 = Image:new('X')
-- image2 = Image:new('Y')
-- pnode1 = PlusNode:new(image1(2,4), image2(8,16))
-- pnode2 = PlusNode:new(image1(3,5), image2(9,17))
-- mnode = MultNode:new(pnode1, pnode2)
-- print(mnode)

-- terra eval_local_complex2([x], [y])
--   return [mnode:toQuote()]
-- end
-- print(eval_local_complex2)
-- -- test2 end

-- print('\nThe Symboltable:')
-- test.printtable(st._data)
-------------------------------------------------------------------------------
-- END of test node classes
-------------------------------------------------------------------------------

return lang


