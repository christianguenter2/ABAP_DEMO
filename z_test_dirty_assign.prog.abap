REPORT z_test_dirty_assign.

FORM f_xyz.

  DATA: lv_local_to_this TYPE c,
        your_badi        TYPE REF TO z_test_badi.

  lv_local_to_this = 'X'.

  GET BADI your_badi.

  " in YOUR_METHOD SOME_GLOBAL_VAR is available
  " with the current content of LV_LOCAL_TO_THIS = 'X'
  CALL BADI your_badi->your_method.
ENDFORM.

START-OF-SELECTION.
  PERFORM f_xyz.
  cl_demo_output=>display( `Test` ).
