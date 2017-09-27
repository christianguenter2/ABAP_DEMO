*&---------------------------------------------------------------------*
*& Report  Z_TEST_ADOBE_LOCALE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_adobe_locale.

PARAMETERS: p_langu TYPE syst-langu DEFAULT 'D' OBLIGATORY,
            p_land TYPE land DEFAULT 'DE' OBLIGATORY.

START-OF-SELECTION.
  DATA: outputparams   TYPE sfpoutputparams.

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

  DATA: name     TYPE fpname,
        funcname TYPE funcname.

  name = 'Z_TEST_PDF_LOCALE'.

  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = name    " Formular
    IMPORTING
      e_funcname = funcname.    " Name des dem Formular zugeordneten Funktionsbausteins


  DATA: docparams TYPE sfpdocparams,
        datum     TYPE datum,
        netwr     TYPE netwr.

  docparams-langu   = p_langu.
  docparams-country = p_land.

  datum = sy-datum.

  netwr = '1500.17'.

  CALL FUNCTION funcname
    EXPORTING
      /1bcdwb/docparams  = docparams
      datum              = datum
      netwr              = netwr
*    IMPORTING
*      /1bcdwb/formoutput = /1bcdwb/formoutput
    EXCEPTIONS
      usage_error        = 1
      system_error       = 2
      internal_error     = 3
      OTHERS             = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'FP_JOB_CLOSE'
*    IMPORTING
*      e_result       = e_result    " Rückgabe Formularprozessierung
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4
    .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
