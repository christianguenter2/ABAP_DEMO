REPORT z_test_budget_check_po.

START-OF-SELECTION.

  DATA: error  TYPE REF TO zcx_lo_error,
        lt_pos TYPE zcl_mm_budget_check=>tty_pos,
        ls_pos LIKE LINE OF lt_pos.

  ls_pos-menge = '1.000'.
  ls_pos-lfdat = '20161208'.
  ls_pos-preis = '27000'.
  ls_pos-peinh = '1'.
  INSERT ls_pos INTO TABLE lt_pos.

  TRY.
      zcl_mm_budget_check=>check(
        EXPORTING
          i_waers = 'EUR'
          i_aufnr = '000004014305'
          i_kokrs = 'DE01'
          i_sakto = '0006000000'
          it_pos  = lt_pos ).

    CATCH zcx_lo_error INTO error.
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
