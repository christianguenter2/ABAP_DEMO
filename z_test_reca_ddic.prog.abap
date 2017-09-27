REPORT z_test_reca_ddic.

*DATA: lv_vbeln TYPE vbeln.
*
*lv_vbeln = '000001344'.
*
*cl_reca_ddic_services=>do_conv_exit(
*  EXPORTING
**    id_convexit        = id_convexit    " Konvertierungsroutine
**    if_only_if_defined = ABAP_TRUE    " Bool: Nur übergebenen Exit benutzen, wenn definiert
*    if_output          = abap_true    " Bool: TRUE=OUTPUT, FALSE=INPUT
*  CHANGING
*    cd_field           = lv_vbeln    " Feld, das gepfrüft und konvertiert werden soll
*  EXCEPTIONS
*    error              = 1
*    OTHERS             = 2
*).
*
*WRITE: lv_vbeln.

DATA: lv_doktyp TYPE z_plm_doktyp,
      lv_text   TYPE string.

lv_doktyp = 'IZ1'.

cl_reca_ddic_doma=>get_text_by_value(
  EXPORTING
*    id_name   = id_name    " Domäne (falls leer: Ermittlung über RTTI von ID_VALUE)
    id_value  = lv_doktyp    " Festwert
    id_langu  = 'D'    " Sprache
  IMPORTING
    ed_text   = lv_text    " Text
  EXCEPTIONS
    not_found = 1
    OTHERS    = 2 ).

WRITE: / lv_text.

CLEAR: lv_doktyp.

cl_reca_ddic_doma=>get_value_by_text(
  EXPORTING
    id_name        = 'Z_PLM_DOKTYP'    " Name der Domäne
    id_text        = lv_text    " Text
    id_langu       = 'D'    " Sprache
*    if_ignore_case = ABAP_FALSE    " Groß-/Kleinschreibung ignorieren
  IMPORTING
    ed_value       = lv_doktyp    " Festwert
  EXCEPTIONS
    not_found      = 1
    OTHERS         = 2
).

WRITE: / lv_doktyp.
