*"* use this source file for your ABAP unit test classes
CLASS ltcl_data_element_structure DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_s_guglhupf,
             number TYPE i,
             text   TYPE string,
           END OF ty_s_guglhupf.
    DATA: data_reader        TYPE REF TO if_sxml_reader,
          data_element       TYPE REF TO zif_yy_data_element,
          expected_structure TYPE ty_s_guglhupf.
    METHODS:
      setup RAISING cx_static_check,
      validate_structure FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_data_element_structure IMPLEMENTATION.

  METHOD setup.
    expected_structure-number = 42.
    expected_structure-text   = 'Gebäck'.
    DATA(json_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    CALL TRANSFORMATION id SOURCE yy = expected_structure RESULT XML json_writer.
    DATA(json_code) = cl_abap_conv_codepage=>create_in( )->convert( source = json_writer->get_output( ) ).
    data_reader = cl_sxml_string_reader=>create( input = json_writer->get_output( ) ).
    data_reader->read_next_node( ).
    data_reader->read_next_node( ).
  ENDMETHOD.

  METHOD validate_structure.

    DATA: my_value         TYPE zif_yy_data_element=>ty_structure,
          my_value_element TYPE zif_yy_data_element=>ty_structure_element,
          first_value      TYPE i,
          second_value     TYPE string.

    data_element = zcl_yy_data_element_structure=>create_instance( ).
    cl_abap_unit_assert=>assert_bound( data_element ).
    CAST zif_yy_data_element_builder( data_element )->build( i_data_reader = data_reader
                                                             i_element_name = 'GUGLHUPF' ).

    data_element->get_value( IMPORTING e_value = my_value ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( my_value ) ).
    READ TABLE my_value INTO my_value_element WITH TABLE KEY index = 1.
    cl_abap_unit_assert=>assert_equals( exp = zif_yy_data_element_type=>number act = my_value_element-element->type ).
    my_value_element-element->get_value( IMPORTING e_value = first_value ).
    cl_abap_unit_assert=>assert_equals( exp = 42 act = first_value ).
    cl_abap_unit_assert=>assert_equals( exp = 'NUMBER' act = my_value_element-element->get_descriptor( )->get_name( ) ).
    READ TABLE my_value INTO my_value_element WITH TABLE KEY index = 2.
    cl_abap_unit_assert=>assert_equals( exp = zif_yy_data_element_type=>string act = my_value_element-element->type ).
    my_value_element-element->get_value( IMPORTING e_value = second_value ).
    cl_abap_unit_assert=>assert_equals( exp = 'Gebäck' act = second_value ).
    cl_abap_unit_assert=>assert_equals( exp = 'TEXT' act = my_value_element-element->get_descriptor( )->get_name( ) ).


    DATA(my_descriptor) = data_element->get_descriptor( ).
    cl_abap_unit_assert=>assert_equals( act = my_descriptor->get_name( ) exp = 'GUGLHUPF' ).
    cl_abap_unit_assert=>assert_equals( act = my_descriptor->get_type( )->get_type_name( ) exp = zif_yy_data_element_type=>structure ).

    CAST zif_yy_data_element_builder( data_element )->build( i_data_reader = data_reader
                                                             i_element_name = 'GUGLHUPF' ).
    data_element->get_value( IMPORTING e_value = my_value ).
    cl_abap_unit_assert=>assert_initial( my_value ).
    cl_abap_unit_assert=>assert_not_bound( data_element->get_descriptor( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_data_element_reference DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: data_reader        TYPE REF TO if_sxml_reader,
          data_element       TYPE REF TO zif_yy_data_element,
          expected_reference TYPE REF TO zif_yy_data_element_type.
    METHODS:
      setup RAISING cx_static_check,
      validate_reference FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_data_element_reference IMPLEMENTATION.

  METHOD setup.
    expected_reference = zcl_yy_data_element_type=>create_instance( i_type_name = zif_yy_data_element_type=>string ).
    DATA(json_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    CALL TRANSFORMATION id SOURCE yy = expected_reference RESULT XML json_writer.
    DATA(json_code) = cl_abap_conv_codepage=>create_in( )->convert( source = json_writer->get_output( ) ).
    data_reader = cl_sxml_string_reader=>create( input = json_writer->get_output( ) ).
    data_reader->read_next_node( ).
    data_reader->read_next_node( ).
  ENDMETHOD.

  METHOD validate_reference.

    data_element = zcl_yy_data_element_structure=>create_instance( ).
    cl_abap_unit_assert=>assert_bound( data_element ).

    TRY.
        CAST zif_yy_data_element_builder( data_element )->build( i_data_reader = data_reader
                                                                 i_element_name = 'GUGLHUPF' ).
        cl_abap_unit_assert=>fail( 'References are unsupported right now!' ).
      CATCH zcx_yy_unsupported_data.
        " Awesome!
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
