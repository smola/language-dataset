*"* use this source file for your ABAP unit test classes
CLASS lcl_about_abapunit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "! We shall contemplate truth by testing reality, via asserts.
      assert_true FOR TESTING,
      "! Ensure the validity of the reference
      assert_bound FOR TESTING,
      "! Ensure that character string fits to simple pattern
      assert_char_cp FOR TESTING,
      "! Ensure that character string does not fit to simple pattern
      assert_char_np FOR TESTING,
      "! Ensure that data objects have distinct values
      assert_differs FOR TESTING,
      "! Ensure equality of two data objects
      assert_equals  FOR TESTING,
      "! Ensure approximate consistency of 2 floating point numbers
      assert_equals_float FOR TESTING,
      "! Ensure that boolean equals ABAP_FALSE
      assert_false  FOR TESTING,
      "! Ensure that data object value is initial
      assert_initial  FOR TESTING,
      "! Ensure invalidity of the reference of a reference variable
      assert_not_bound  FOR TESTING,
      "! Ensure that value of data object is not initial
      assert_not_initial  FOR TESTING,
      "! Ensure that number is in given range
      assert_number_between  FOR TESTING,
      "! Ensure specific value of return code
      assert_subrc  FOR TESTING,
      "! Ensure that data is contained as line within internal table
      assert_table_contains  FOR TESTING,
      "! Ensure that data is not contained as line in internal table
      assert_table_not_contains  FOR TESTING,
      "! Ensure that text matches regular expression
      assert_text_matches FOR TESTING,
      "! Ensure that a constraint is met by data object
      assert_that  FOR TESTING.

    CONSTANTS:
       default_control_flow TYPE aunit_flowctrl VALUE if_aunit_constants=>class.

ENDCLASS.

CLASS lcl_about_abapunit IMPLEMENTATION.

  METHOD assert_bound.
    DATA: book_ref TYPE REF TO cl_book_ppf.

    cl_abap_unit_assert=>assert_bound(
      act              = book_ref
      msg              = |Sometimes you need to make some Objects alive... Just think about the reference|
      quit             = default_control_flow ).

  ENDMETHOD.

  METHOD assert_char_cp.

    cl_abap_unit_assert=>assert_char_cp(
      act              = |abcd|
      exp              = || "this should be replaced properly
      msg              = |Do you really think you match the pattern?|
      quit             = default_control_flow ) .

  ENDMETHOD.

  METHOD assert_char_np.

    cl_abap_unit_assert=>assert_char_np(
      act  = ||
      exp  = || "this should be replaced properly
      msg  = |Wow, could it be possible that this fails on basis of difference|
      quit = default_control_flow ).

  ENDMETHOD.

  METHOD assert_differs.

    cl_abap_unit_assert=>assert_differs(
      act              = ||
      exp              = || "this should be replaced properly
      msg              = |Here should something be different...|
      quit = default_control_flow ).

  ENDMETHOD.

  METHOD assert_equals.

    cl_abap_unit_assert=>assert_equals(
      act =  1
      exp =  || "this should be replaced properly
      msg  = |It´s easy to identify when values should be equal|
      quit = default_control_flow ).

  ENDMETHOD.

  METHOD assert_equals_float.
    DATA: float_value TYPE decfloat16 VALUE '1.9'.

    cl_abap_unit_assert=>assert_equals_float(
      act  =  float_value
      exp  =  CONV decfloat16( 2 ) "this should be replaced properly
      msg  = |Nearly same isn´t same!|
      quit = default_control_flow ).

  ENDMETHOD.

  METHOD assert_false.

    cl_abap_unit_assert=>assert_false(
      act  =  |X|  "this should be replaced properly
      msg  =  |Like the prior koan. Ponder if you will, the power of simple assertions when verifying an object's behavior|
      quit = default_control_flow ).

  ENDMETHOD.

  METHOD assert_initial.

    cl_abap_unit_assert=>assert_initial(
      act  = |X|  "this should be replaced properly
      msg  = |Hey come on... initial means INITIAL|
      quit = default_control_flow ).

  ENDMETHOD.

  METHOD assert_not_bound.
    DATA: book_ref TYPE REF TO cl_book_ppf.

    book_ref = NEW #( ). "Here should something be done

    cl_abap_unit_assert=>assert_not_bound(
      act  =  book_ref
      msg  =  |There are times when something should not be null, and this assertion can prove that|
      quit = default_control_flow ) .

  ENDMETHOD.

  METHOD assert_not_initial.

    cl_abap_unit_assert=>assert_not_initial(
      act  =  || "this should be replaced properly
      msg  =  |Hey come on... NOT initial means NOT INITIAL|
      quit = default_control_flow ).

  ENDMETHOD.

  METHOD assert_number_between.

    cl_abap_unit_assert=>assert_number_between(
      lower  = 1
      upper  = 3
      number = 4 "this should be replaced properly
      msg    = |Ok, you´re kidding...Find the number hat is in between the others|
      quit   = default_control_flow ).

  ENDMETHOD.

  METHOD assert_subrc.

    cl_abap_unit_assert=>assert_subrc(
      exp  = 1
      act  = sy-subrc "this should be replaced properly
      msg  = |Yes, I know this assertin could be improved...Just check if sy-subrc contains the necessary value|
      quit = default_control_flow ).

  ENDMETHOD.

  METHOD assert_table_contains.
    TYPES tyt_integer TYPE STANDARD TABLE OF i WITH NON-UNIQUE KEY table_line.
    DATA(dref) = NEW tyt_integer( ( 1 ) ( 2 ) ( 3 ) ).
    ASSIGN dref->* TO FIELD-SYMBOL(<itab>).

    cl_abap_unit_assert=>assert_table_contains(
      line  = || "this should be replaced properly
      table = <itab>
      msg   = |The line should contain something that´s in the table|
      quit  = default_control_flow ).

  ENDMETHOD.

  METHOD assert_table_not_contains.
    TYPES tyt_integer TYPE STANDARD TABLE OF i WITH NON-UNIQUE KEY table_line.
    DATA(dref) = NEW tyt_integer( ( 1 ) ( 2 ) ( 3 ) ).
    ASSIGN dref->* TO FIELD-SYMBOL(<itab>).

    cl_abap_unit_assert=>assert_table_not_contains(
      line             = 3 "this should be replaced properly
      table            = <itab>
      msg              = |The line should contain something that´s NOT in the table|
      quit = default_control_flow ).

  ENDMETHOD.

  METHOD assert_text_matches.

    cl_abap_unit_assert=>assert_text_matches(
        pattern          = |\\D|
        text             = |123| "this should be replaced properly
        msg              = |Welcome to regular expressions... find out what the pattern means|
        quit = default_control_flow ).

  ENDMETHOD.

  METHOD assert_that.
    DATA(abap_koans_constraint) = NEW lcl_koans_demo_constraint( ).

    cl_abap_unit_assert=>assert_that(
      act              =   || "this should be replaced properly
      exp              =   abap_koans_constraint
      msg              =   |Hey just look at the constraint class...then you will figure out the expeted value|
      quit = default_control_flow ).

  ENDMETHOD.

  METHOD assert_true.

    cl_abap_unit_assert=>assert_true(
      act  = || "this should be replaced properly
      msg  = |The \|\| are an attempt to communicate the need to fill in an answer|
      quit = default_control_flow ).

  ENDMETHOD.

ENDCLASS.
