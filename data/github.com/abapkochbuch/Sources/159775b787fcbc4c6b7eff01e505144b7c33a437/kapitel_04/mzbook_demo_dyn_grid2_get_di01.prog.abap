*----------------------------------------------------------------------*
***INCLUDE MZBOOK_DEMO_DYN_GRID2_GET_DI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  GET_DATA  INPUT
*&---------------------------------------------------------------------*
MODULE get_data INPUT.

  CLEAR gs_zbook_ticket.
  SELECT SINGLE * FROM zbook_ticket INTO gs_zbook_ticket
   WHERE tiknr = zbook_ticket-tiknr.
  CHECK sy-subrc = 0.
  if gr_dd_grid is INITIAL.
    CREATE OBJECT gr_dd_grid
      EXPORTING
        container_name = 'CC_DYN'
        cont_log_name  = 'CC_LOG'.
  ENDIF.

  CHECK gr_dd_grid IS  BOUND.
  gr_dd_grid->prepare_data( tiknr = gs_zbook_ticket-tiknr
                            area  = gs_zbook_ticket-area
                            clas  = gs_zbook_ticket-clas ).



ENDMODULE.                 " GET_DATA  INPUT
