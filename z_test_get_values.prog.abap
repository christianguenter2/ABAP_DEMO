REPORT z_test_get_values.

DATA: BEGIN OF ls_data,
        varname TYPE string,
        index   TYPE i,
      END OF ls_data,
      l_data TYPE string,
      dref TYPE REF TO data.

GET REFERENCE OF ls_data INTO dref.

CALL FUNCTION 'TPDA_SCRIPT_CALL_SEL_SCREEN'
  EXPORTING
    p_screen   = p_screen
*  IMPORTING
*    p_range_it = p_range_it
*    p_cancel   = p_cancel
  .
