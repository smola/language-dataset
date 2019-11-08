using xml

** (HTML Element) Represents a form '<input>' of type 'hidden'.
const class Hidden : Element {
	
	@NoDoc
	new makeFromFinder	(ElemFinder elemFinder)	: super(elemFinder)  { }
	new makeFromCss		(Str cssSelector) 		: super(cssSelector) { }

	** Returns the 'name' attribute.
	Str? name() {
		getAttr("name")
	}

	** Gets and sets the 'value' attribute. 
	** Returns 'null' if the value has not been set.
	Str? value {
		get { getAttr("value") }
		set { setAttr("value", it) }
	}
	
	** Verify that the hidden element has the given value.
	Void verifyValueEq(Obj expected) {
		verifyEq(value, expected)	
	}

	@NoDoc
	override protected XElem findElem() {
		elem := Attr(super.findElem)
		if (elem.name != "input" || elem["type"]?.lower != "hidden")
			fail("Element is NOT a hidden input: ", false)
		return elem.elem
	}
}
