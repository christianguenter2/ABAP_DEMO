*&---------------------------------------------------------------------*
*& Report  Z_TEST_BARCODE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_barcode.

PARAMETERS: p_value TYPE char20 OBLIGATORY.

START-OF-SELECTION.

  DATA: funcname     TYPE funcname,
        outputparams TYPE sfpoutputparams.

  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = 'Z_TEST_BARCODE'
    IMPORTING
      e_funcname = funcname.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = outputparams    " Formularprozessierung Ausgabeparameter
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.

  CALL FUNCTION funcname
    EXPORTING
      i_value        = p_value
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.

  CALL FUNCTION 'FP_JOB_CLOSE'.
