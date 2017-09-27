*&---------------------------------------------------------------------*
*& Report  Z_TEST_KLASSIFIZIERUNG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_klassifizierung.

PARAMETERS: p_dokar TYPE draw-dokar,
            p_doknr TYPE draw-doknr,
            p_dokvr TYPE draw-dokvr,
            p_doktl TYPE draw-doktl.

DATA: lv_draw TYPE draw.

SELECT SINGLE * FROM draw
                INTO lv_draw
                WHERE dokar = p_dokar
                AND   doknr = p_doknr
                AND   dokvr = p_dokvr
                AND   doktl = p_doktl.

IF sy-subrc = 0.
  if zcl_ehs_material_functions=>is_old_mat_classific_available( lv_draw ) = abap_true.
    zcl_ehs_material_functions=>delete_old_mat_classification( lv_draw ).
    COMMIT WORK.
    MESSAGE 'Daten wurden geändert' TYPE 'S'.
  ELSE.
    MESSAGE 'Alte Klassifizierung nicht vorhanden' TYPE 'S'.
  endif.
ELSE.
  MESSAGE 'DIS nicht vorhanden' TYPE 'S'.
ENDIF.
