*&---------------------------------------------------------------------*
*& Report  Z_TEST_UPLOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_upload.

PARAMETERS: p_file TYPE string OBLIGATORY LOWER CASE MEMORY ID file.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING value(r_instance) TYPE REF TO lcl_application,
                   at_value_request.
    METHODS: start.

  PRIVATE SECTION.
    METHODS: _get_picture EXPORTING e_picture TYPE xstring
                                    e_mime_type TYPE string.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD get_instance.
    CREATE OBJECT r_instance.
  ENDMETHOD.                    "get_instance

  METHOD at_value_request.
    DATA: file_table TYPE filetable,
          rc         TYPE syst-subrc.

    cl_gui_frontend_services=>file_open_dialog(
      CHANGING
        file_table              = file_table    " Tabelle, die selektierte Dateien enthält
        rc                      = rc    " Rückgabewert: Anzahl Dateien oder -1 falls Fehler auftritt
      EXCEPTIONS
        OTHERS                  = 5 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE file_table INTO p_file
                          INDEX 1.
  ENDMETHOD.                    "at_value_request

  METHOD start.
    DATA: picture      TYPE xstring,
          mime_type    TYPE string,
          funcname     TYPE funcname,
          outputparams TYPE sfpoutputparams.

    _get_picture(
      IMPORTING
        e_picture   = picture
        e_mime_type = mime_type ).

    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = 'Z_TEST_BILD'
      IMPORTING
        e_funcname = funcname.    " Name des dem Formular zugeordneten Funktionsbausteins


    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = outputparams    " Formularprozessierung Ausgabeparameter
      EXCEPTIONS
        OTHERS          = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION funcname
      EXPORTING
        i_bild         = picture
        i_mime_type    = mime_type
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
        OTHERS = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "start

  METHOD _get_picture.
    DATA: data_tab   TYPE solix_tab,
          filelength TYPE i,
          file       TYPE draw-filep,
          mimetype   TYPE tdwp-mimetype.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = p_file
        filetype                = 'BIN'    " Dateityp (Ascii, Binär)
      IMPORTING
        filelength              = filelength
      CHANGING
        data_tab                = data_tab    " Übergabetabelle für Datei-Inhalt
      EXCEPTIONS
        OTHERS                  = 1 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    cl_bcs_convert=>solix_to_xstring(
      EXPORTING
        it_solix   = data_tab
        iv_size    = filelength
      RECEIVING
        ev_xstring = e_picture ).

    file = p_file.

    CALL FUNCTION 'CV120_GET_MIME_TYPE'
      EXPORTING
        pf_file      = file
      IMPORTING
        pfx_mimetype = mimetype.

    e_mime_type = mimetype.
  ENDMETHOD.                    "get_bild
ENDCLASS.                    "lcl_application IMPLEMENTATION

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_application=>at_value_request( ).

START-OF-SELECTION.
  lcl_application=>get_instance( )->start( ).
