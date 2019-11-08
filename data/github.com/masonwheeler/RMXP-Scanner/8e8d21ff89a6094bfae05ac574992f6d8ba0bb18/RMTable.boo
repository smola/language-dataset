namespace TURBU.RubyMarshal

import System.IO

def Table(sr as BinaryReader) as object:
	dimensions = sr.ReadInt32()
	x = sr.ReadInt32()
	y = sr.ReadInt32()
	z = sr.ReadInt32()
	size = sr.ReadInt32()
	assert x * y * z == size
	assert dimensions <= 3
	data = array(short, x * y * z)
	for i in range(data.Length):
		break if sr.BaseStream.Position == sr.BaseStream.Length
		data[i] = sr.ReadInt16()
	result = []
	i = 0
	for lZ in range(z):
		zElem = []
		result.Add(zElem)
		for lY in range(y):
			yElem = []
			zElem.Add(yElem)
			for lX in range(x):
				yElem.Add(data[i])
				++i
	return result
