@TYPE: {
    INT,
    DOUBLE,
    STRING
}

@@IceObject() {

    @show(){
        println(value)
    }

}

@@IceIntegerObject(IceObject) {

    @type: TYPE.INT
    @IceIntegerObject(value) {
        @self.value: value
    }

    @binaryOperate(obj, op) {
        
    }

}

@@IceDoubleObject(IceObject) {

    @type: TYPE.DOUBLE
    @IceDoubleObject(value) {
        @self.value: value
    }

}

@@IceStringObject(IceObject) {

    @type: TYPE.STRING
    @IceStringObject(value) {
        @self.value: value
    }

}