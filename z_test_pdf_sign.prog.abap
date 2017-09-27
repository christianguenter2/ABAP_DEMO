*&---------------------------------------------------------------------*
*& Report  Z_TEST_PDF_SIGN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_TEST_PDF_SIGN.

START-OF-SELECTION.
  data: OUTPUTPARAMS TYPE  SFPOUTPUTPARAMS,
        form_NAMe TYPE  FPNAME VALUE 'Z_TEST',
        FUNCNAME TYPE  FUNCNAME,
        FORMOUTPUT TYPE  FPFORMOUTPUT.


  call FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = outputparams     " Formularprozessierung Ausgabeparameter
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      others          = 5
    .
  IF sy-subrc <> 0.
   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  call FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name              = form_name    " Formular
    IMPORTING
      e_funcname          = funcname    " Name des dem Formular zugeordneten Funktionsbausteins
    .

  CALL FUNCTION funcname
*   EXPORTING
*     /1BCDWB/DOCPARAMS        =
   IMPORTING
     /1BCDWB/FORMOUTPUT       = formoutput
   EXCEPTIONS
     USAGE_ERROR              = 1
     SYSTEM_ERROR             = 2
     INTERNAL_ERROR           = 3
     OTHERS                   = 4
            .
  IF sy-subrc <> 0.
   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  data: lo_pdf TYPE REF TO if_fp_pdf_object.

  lo_pdf = cl_fp=>get_reference( )->create_pdf_object( connection = 'ADS' ).
  lo_pdf->set_document(
   EXPORTING
*     pdffile =     " PDF Datei
     pdfdata = formoutput-pdf     " PDF Daten
 ).

  lo_pdf->set_signature(
    EXPORTING
*      keyname     =     " Name des privaten Schlüssels
      fieldname   =  `data.#subform[0].Unterschriftsfeld1`   " Name des Feldes, welches das Zertifikat aufnimmt
*      reason      =
*      location    =
*      contactinfo =
  ).

  lo_pdf->execute(
*    EXPORTING
*      iv_synchronous = ABAP_TRUE    " synchroner Aufruf?
*      is_parameters  =     " Parameter
  ).

  lo_pdf->get_document(
*    IMPORTING
*      pdfdata =     " PDF Daten
  ).

  CALL FUNCTION 'FP_JOB_CLOSE'
*    IMPORTING
*      e_result       =     " Rückgabe Formularprozessierung
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      others         = 4
    .
  IF sy-subrc <> 0.
   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
