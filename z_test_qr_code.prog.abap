*&---------------------------------------------------------------------*
*& Report  Z_TEST_QR_CODE
*&
*&--------------------------------------t-------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_qr_code.

PARAMETERS: left  TYPE string OBLIGATORY LOWER CASE,
            right TYPE string LOWER CASE.

INITIALIZATION.

  left  = cl_system_uuid=>create_uuid_c32_static( ).
  right = cl_system_uuid=>create_uuid_c32_static( ).

START-OF-SELECTION.

  DATA: funcname     TYPE funcname,
        outputparams TYPE sfpoutputparams,
        error        TYPE REF TO cx_fp_api.

  TRY .
      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name     = 'Z_TEST_QR_CODE'
        IMPORTING
          e_funcname = funcname.    " Name des dem Formular zugeordneten Funktionsbausteins

    CATCH cx_fp_api INTO error.
      MESSAGE error TYPE 'I' DISPLAY LIKE 'S'.
      RETURN.
  ENDTRY.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = outputparams    " Formularprozessierung Ausgabeparameter
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION '/1BCDWB/SM00000257'
    EXPORTING
      i_string       = left
      i_string2      = right
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'FP_JOB_CLOSE'
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
