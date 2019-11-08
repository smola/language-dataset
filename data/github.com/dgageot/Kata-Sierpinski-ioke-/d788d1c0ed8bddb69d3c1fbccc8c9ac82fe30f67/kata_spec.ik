use("ispec")

iter = method(chars,height,offY,offX,
	(height == 1) ifTrue(
		chars[offY][offX] = "\u25B2",
	) ifFalse(
		height /= 2
		iter(chars,height,offY+height,offX-height)
		iter(chars,height,offY,offX)
		iter(chars,height,offY+height,offX+height)
	)
)

sierpinski = fn(height,
	chars = (1..height) map([" "]*(2*height-1))

	iter(chars,height,0,height-1)

	chars inject([], sum, x, sum + x + ["\n"]) join
)

describe("sierpinski",
	it("should work on 1 line",
		sierpinski(1) should == "\u25B2\n"
	)
	it("should work on 2 lines",
		sierpinski(2) should == " \u25B2 \n\u25B2 \u25B2\n"
	)
	it("should work on 4 lines",
		sierpinski(4) should == "   \u25B2   \n  \u25B2 \u25B2  \n \u25B2   \u25B2 \n\u25B2 \u25B2 \u25B2 \u25B2\n"
	)
)
