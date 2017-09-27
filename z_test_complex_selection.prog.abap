REPORT z_test_complex_selection.

DATA: ltr_matnr        TYPE RANGE OF matnr,
      title            TYPE sytitle,
      excluded_options TYPE rsoptions .

title = |Material auswählen| .

excluded_options-bt = abap_true.
excluded_options-cp = abap_true.
excluded_options-ge = abap_true.
excluded_options-gt = abap_true.
excluded_options-le = abap_true.
excluded_options-lt = abap_true.
excluded_options-nb = abap_true.
excluded_options-np = abap_true.
excluded_options-ne = abap_true.

CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
  EXPORTING
    title             = title
    signed            = ' ' " X: Sign Allowed
    just_incl         = abap_true
    excluded_options  = excluded_options    " List of Options Not Allowed
  TABLES
    range             = ltr_matnr
  EXCEPTIONS
    OTHERS            = 5.

cl_demo_output=>display_data( ltr_matnr ).
