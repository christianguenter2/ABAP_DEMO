*&---------------------------------------------------------------------*
*& Report z_test_escape
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_escape.

DATA(string) = `commtter_name=Christian Gänter&committer_email=christianguenter@googlemail.com`.

cl_http_utility=>escape_url(
  EXPORTING
    unescaped = string
*    options   = options    " Reserve für zukünftige Erweiterungen
  RECEIVING
    escaped   = DATA(escaped)
).

cl_demo_output=>write( escaped ).

DATA(fields) = cl_http_utility=>if_http_utility~string_to_fields(
    string             = escaped
    encode             = 0    " 0: unkodiert, 1: kodiert
*    fields             = fields    " Tabelle mit Name/Wert-Paaren
).

cl_demo_output=>display( fields ).
