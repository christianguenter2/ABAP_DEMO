*&---------------------------------------------------------------------*
*& Report z_test_regex
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_regex.

DATA: regex TYPE string VALUE `\+|\*|\?`,
      value TYPE string VALUE `216825052*.EML`.

IF matches( regex = regex
            val   = value  ).
  cl_demo_output=>write_data( `True` ).
ELSE.
  cl_demo_output=>write_data( `False` ).
ENDIF.

FIND FIRST OCCURRENCE OF REGEX regex
           IN value.
IF sy-subrc = 0.
  cl_demo_output=>write_data( `True` ).
ELSE.
  cl_demo_output=>write_data( `False` ).
ENDIF.

cl_demo_output=>display(  ).
