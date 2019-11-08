class YEA_UNIT_TEST_RESPONSE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_ENTITY .
  interfaces IF_HTTP_RESPONSE .

  aliases GET_HEADER_FIELD
    for IF_HTTP_ENTITY~GET_HEADER_FIELD .
  aliases GET_HEADER_FIELDS
    for IF_HTTP_ENTITY~GET_HEADER_FIELDS .
  aliases SET_CDATA
    for IF_HTTP_ENTITY~SET_CDATA .
  aliases SET_CONTENT_TYPE
    for IF_HTTP_ENTITY~SET_CONTENT_TYPE .
  aliases SET_HEADER_FIELD
    for IF_HTTP_ENTITY~SET_HEADER_FIELD .
  aliases SET_HEADER_FIELDS
    for IF_HTTP_ENTITY~SET_HEADER_FIELDS .
  aliases SET_STATUS
    for IF_HTTP_RESPONSE~SET_STATUS .
protected section.
private section.

  aliases GET_CDATA
    for IF_HTTP_ENTITY~GET_CDATA .
  aliases GET_STATUS
    for IF_HTTP_RESPONSE~GET_STATUS .

  data _STATUS type INT4 .
  data _REASON type STRING .
  data _BODY type STRING .
  data _HEADERS type YEA_KEY_VALUES .
  data _CONTENT_TYPE type STRING .
ENDCLASS.



CLASS YEA_UNIT_TEST_RESPONSE IMPLEMENTATION.


  method IF_HTTP_ENTITY~GET_CDATA.
    data = _body.
  endmethod.


  method IF_HTTP_ENTITY~GET_CONTENT_TYPE.
    content_type = me->_content_type.
  endmethod.


  method IF_HTTP_ENTITY~GET_DATA.
  endmethod.


  method if_http_entity~get_header_field.
    try.
      value = me->_headers[ key = name ]-value.
      catch cx_root.
        endtry.
  endmethod.


  method IF_HTTP_ENTITY~GET_HEADER_FIELDS.
    loop at me->_headers assigning field-symbol(<header>).
      append value #( name = <header>-key value = <header>-value ) to fields.
    endloop.
  endmethod.


  method IF_HTTP_ENTITY~SET_CDATA.
    me->_body = data.
  endmethod.


  method IF_HTTP_ENTITY~SET_CONTENT_TYPE.
    me->_content_type = content_type.
  endmethod.


  method IF_HTTP_ENTITY~SET_HEADER_FIELD.
    append value #( key = name value = value ) to me->_headers.
  endmethod.


  method IF_HTTP_ENTITY~SET_HEADER_FIELDS.
    loop at fields assigning field-symbol(<f>).
      me->set_header_field(
        name = <f>-name
        value = <f>-value
      ).
    endloop.
  endmethod.


  method IF_HTTP_RESPONSE~GET_STATUS.
    code = me->_status.
    reason = me->_reason.
  endmethod.


  method IF_HTTP_RESPONSE~SET_STATUS.
    me->_status = code.
    me->_reason = reason.
  endmethod.
ENDCLASS.
