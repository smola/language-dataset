METHOD _get_stock.

  CHECK is_params-plant IS NOT INITIAL.

  DATA:

     rt_plant TYPE  RANGE OF zaws1_stock_res-plant,
     rl_plant LIKE LINE OF rt_plant,

     rt_storage TYPE RANGE OF zaws1_stock_res-storage,
     rl_storage LIKE LINE OF rt_storage,

     rt_material TYPE RANGE OF zaws1_stock_res-material,
     rl_material LIKE LINE OF rt_material.

  DATA: lt_mard TYPE TABLE OF mard,
        lr_mard TYPE REF TO mard,
        lr_result TYPE REF TO zaws1_stock_res.

  rl_plant-sign = 'I'.
  rl_plant-option = 'EQ'.
  rl_plant-low = is_params-plant.
  APPEND rl_plant TO rt_plant.

  IF is_params-storage IS NOT INITIAL.
    rl_storage-sign = 'I'.
    rl_storage-option = 'EQ'.
    rl_storage-low = is_params-storage.
    APPEND rl_storage TO rt_storage.
  ENDIF.


  IF is_params-material IS NOT INITIAL.

    rl_material-sign = 'I'.
    rl_material-option = 'EQ'.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input              = is_params-material
     IMPORTING
        OUTPUT             = rl_material-low
     EXCEPTIONS
       OTHERS             = 2.
    APPEND rl_material TO rt_material.

  ENDIF.

  SELECT werks lgort matnr
    INTO TABLE lt_mard
    FROM mard
    WHERE werks IN rt_plant
      AND lgort IN rt_storage
      AND matnr IN rt_material.

  LOOP AT lt_mard REFERENCE INTO lr_mard.
    APPEND INITIAL LINE TO rs_result REFERENCE INTO lr_result.

    lr_result->plant    = lr_mard->werks.
    lr_result->storage  = lr_mard->lgort.
    lr_result->material = lr_mard->matnr.
    lr_result->quantity = lr_mard->labst.

    SELECT SINGLE meins
      INTO lr_result->uom
      FROM mara WHERE matnr = lr_mard->matnr.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input  = lr_result->uom
      IMPORTING
        output = lr_result->uom
      EXCEPTIONS
        OTHERS = 2.

    SELECT SINGLE maktx
      INTO lr_result->materialname
      FROM makt
      WHERE matnr = lr_mard->matnr
        and spras = 'EN'.

  ENDLOOP.

ENDMETHOD.