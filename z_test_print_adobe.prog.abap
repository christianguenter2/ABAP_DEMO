*&---------------------------------------------------------------------*
*& Report  Z_TEST_PRINT_ADOBE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_print_adobe.

DATA: page TYPE i.

PARAMETERS: p_prnds LIKE tsp01-rqdest OBLIGATORY DEFAULT 'HIS6',
            p_qpdf  TYPE string OBLIGATORY DEFAULT 'C:\Users\hezellud\Downloads\qpdf-6.0.0-bin-mingw32\qpdf-6.0.0\bin\qpdf.exe' LOWER CASE,
*            p_pdftk TYPE string OBLIGATORY DEFAULT 'C:\Program Files (x86)\PDFtk Server\bin\pdftk.exe' LOWER CASE,
            p_pdftk TYPE string OBLIGATORY DEFAULT 'C:\Program Files (x86)\PDFtk\bin\pdftk.exe' LOWER CASE,
            p_temp TYPE string OBLIGATORY DEFAULT 'C:\Temp\SAP_PDF\' LOWER CASE.

PARAMETERS: p_copies TYPE sfpoutputparams-copies OBLIGATORY DEFAULT 1.
SELECT-OPTIONS: s_pages FOR page.

*----------------------------------------------------------------------*
*       CLASS lcl_pdf_print DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_pdf_print DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      print_pdf
        RETURNING value(r_output) TYPE fpformoutput.

ENDCLASS.                    "lcl_pdf_print DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_pdf_print IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_pdf_print IMPLEMENTATION.

  METHOD print_pdf.

    DATA: outputparams TYPE sfpoutputparams,
          funcname     TYPE funcname,
          docparams    TYPE sfpdocparams,
          result       TYPE sfpjoboutput,
          lt_data      TYPE int4_table.

    outputparams-getpdf   = abap_true.
    outputparams-getxml   = abap_true.
    outputparams-getpdl   = abap_true.
    outputparams-nodialog = abap_true.
    outputparams-noprint  = abap_true.

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

    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = 'Z_TEST_ADOBE'
      IMPORTING
        e_funcname = funcname.

    DO 100 TIMES.
      INSERT sy-index INTO TABLE lt_data.
    ENDDO.

    CALL FUNCTION funcname
      EXPORTING
        /1bcdwb/docparams  = docparams
        it_data            = lt_data
        i_page_to_print    = 2
      IMPORTING
        /1bcdwb/formoutput = r_output.

    CALL FUNCTION 'FP_JOB_CLOSE'
      IMPORTING
        e_result       = result    " Rückgabe Formularprozessierung
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.                    "print_pdf

ENDCLASS.                    "lcl_pdf_print IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_pdf_splitter DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_pdf_splitter DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_splitted_file,
             page TYPE char02,
             file TYPE xstring,
           END OF ty_splitted_file,
         tty_splitted_file TYPE SORTED TABLE OF ty_splitted_file
                                WITH UNIQUE KEY page.

    CLASS-METHODS:
      split_pdf
        IMPORTING
          i_pdf TYPE xstring
          i_pages TYPE i
        RETURNING value(rt_splitted_pages) TYPE tty_splitted_file.


  PRIVATE SECTION.

    CLASS-METHODS:
      _decrypt
        IMPORTING
          i_file1 TYPE csequence
          i_file2 TYPE csequence,

      _burst
        IMPORTING
          i_file  TYPE csequence
          i_splitted_file_pattern TYPE csequence,

     _get_splitted_pages
      IMPORTING
        i_splitted_file_pattern TYPE csequence
        i_pages TYPE i
      RETURNING
        value(rt_splitted_files) TYPE lcl_pdf_splitter=>tty_splitted_file.

ENDCLASS.                    "lcl_pdf_splitter DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_pdf_splitter IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_pdf_splitter IMPLEMENTATION.

  METHOD split_pdf.

    DATA: lt_solix              TYPE solix_tab,
          filename              TYPE string,
          filename2             TYPE string,
          splitted_file_pattern TYPE string,
          filename_doc_data     TYPE string,
          uzeit                 TYPE sy-uzeit.

    lt_solix = cl_bcs_convert=>xstring_to_solix( i_pdf ).

    uzeit = sy-uzeit.

    filename              = p_temp && uzeit && '.pdf'.
    filename2             = p_temp && uzeit && '_decrypted.pdf'.
    filename_doc_data     = p_temp && 'doc_data.txt'.
    splitted_file_pattern = p_temp && uzeit && '_page%02d.pdf'.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  = filename
        filetype                  = 'BIN'    " Dateityp (Asii, Binär, ...)
      CHANGING
        data_tab                  = lt_solix
      EXCEPTIONS
        OTHERS                    = 24 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    _decrypt( i_file1 = filename
              i_file2 = filename2 ).

    _burst( i_file                  = filename2
            i_splitted_file_pattern = splitted_file_pattern ).

    _get_splitted_pages(
      EXPORTING
        i_splitted_file_pattern = splitted_file_pattern
        i_pages                 = i_pages
      RECEIVING
        rt_splitted_files = rt_splitted_pages ).

    DATA: rc TYPE i.

    cl_gui_frontend_services=>file_delete(
        EXPORTING
          filename             = filename
        CHANGING
          rc                   = rc    " Rückgabewert
        EXCEPTIONS
          OTHERS               = 9 ).

    cl_gui_frontend_services=>file_delete(
        EXPORTING
          filename             = filename2
        CHANGING
          rc                   = rc    " Rückgabewert
        EXCEPTIONS
          OTHERS               = 9 ).

    cl_gui_frontend_services=>file_delete(
        EXPORTING
          filename             = filename_doc_data
        CHANGING
          rc                   = rc    " Rückgabewert
        EXCEPTIONS
          OTHERS               = 9 ).

  ENDMETHOD.                    "split_pdf

  METHOD _decrypt.

    DATA: parameter TYPE string.

    parameter = | --decrypt { i_file1 } { i_file2 }|.

    cl_gui_frontend_services=>execute(
      EXPORTING
        application            = p_qpdf
        parameter              = parameter
        synchronous            = 'X'
      EXCEPTIONS
        OTHERS                 = 10 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "_decrypt

  METHOD _burst.

    DATA: parameter TYPE string.

    parameter = |{ i_file } burst output { i_splitted_file_pattern }|.

    cl_gui_frontend_services=>execute(
      EXPORTING
        application            = p_pdftk
        parameter              = parameter
        synchronous            = 'X'
      EXCEPTIONS
        OTHERS                 = 10 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "_burst

  METHOD _get_splitted_pages.

    DATA: temp_filename    TYPE string,
          index            TYPE char02,
          lv_splitted_file LIKE LINE OF rt_splitted_files,
          rc               TYPE i,
          lt_solix TYPE solix_tab.

    DO i_pages TIMES.

      CLEAR: lt_solix, lv_splitted_file.

      index = sy-index.
      SHIFT index RIGHT DELETING TRAILING space.
      OVERLAY index WITH '00'.

      temp_filename = replace( val  = i_splitted_file_pattern
                               sub  = '%02d'
                               with = index ).

      cl_gui_frontend_services=>gui_upload(
        EXPORTING
          filename                = temp_filename
          filetype                = 'BIN'    " Dateityp (Ascii, Binär)
        CHANGING
          data_tab                = lt_solix
        EXCEPTIONS
          OTHERS                  = 19 ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      lv_splitted_file-page = index.
      lv_splitted_file-file = cl_bcs_convert=>solix_to_xstring( lt_solix ).

      INSERT lv_splitted_file INTO TABLE rt_splitted_files.

      cl_gui_frontend_services=>file_delete(
        EXPORTING
          filename             = temp_filename
        CHANGING
          rc                   = rc    " Rückgabewert
        EXCEPTIONS
          OTHERS               = 9 ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDDO.

  ENDMETHOD.                    "_get_splitted_pages

ENDCLASS.                    "lcl_pdf_splitter IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_spooler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_spooler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      send_to_spool
        IMPORTING
          it_splitted_files TYPE lcl_pdf_splitter=>tty_splitted_file.

ENDCLASS.                    "lcl_spooler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_spooler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_spooler IMPLEMENTATION.

  METHOD send_to_spool.

    FIELD-SYMBOLS: <single_page> LIKE LINE OF it_splitted_files.

    DATA: handle   TYPE sy-tabix,
          spoolid  TYPE tsp01-rqident,
          partname TYPE adspart,
          ads_path TYPE text1024,
          dstfile  TYPE text1024.

    CALL FUNCTION 'ADS_GET_PATH'
      IMPORTING
        ads_path = ads_path.

    DATA: pages TYPE i VALUE 0,
          size  TYPE i.

    LOOP AT it_splitted_files ASSIGNING <single_page>
                              WHERE page IN s_pages.

      CALL FUNCTION 'ADS_SR_OPEN'
        EXPORTING
          dest            = p_prnds
          immediate_print = abap_false
          doctype         = 'ADSP'
        IMPORTING
          handle          = handle
          spoolid         = spoolid
          partname        = partname    " Dateiname eines ADS Parts
        EXCEPTIONS
          OTHERS          = 6.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      dstfile = ads_path && '/' && partname && '.pdf'.

      OPEN DATASET dstfile FOR OUTPUT IN BINARY MODE.
      ASSERT sy-subrc = 0.

      TRANSFER <single_page>-file TO dstfile.
      ASSERT sy-subrc = 0.

      CLOSE DATASET dstfile.
      ASSERT sy-subrc = 0.

      size  = xstrlen( <single_page>-file ).

      pages = 1.

      CALL FUNCTION 'ADS_SR_CONFIRM'
        EXPORTING
          handle   = handle    " Handle aus ADS_SR_OPEN
          partname = partname    " Dateiname eines ADS Parts
          size     = size    " File size of part
          pages    = pages    " Pages of part
          no_pdf   = ' '    " Flag: Part wurde ohne PDF Datei erzeugt
        EXCEPTIONS
          OTHERS   = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION 'ADS_SR_CLOSE'
        EXPORTING
          handle = handle    " Handle aus ADS_SR_OPEN
        EXCEPTIONS
          OTHERS = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "send_to_spool

ENDCLASS.                    "lcl_spooler IMPLEMENTATION

START-OF-SELECTION.

  DATA: output            TYPE fpformoutput,
        lt_splitted_files TYPE lcl_pdf_splitter=>tty_splitted_file.

  output = lcl_pdf_print=>print_pdf( ).

  lt_splitted_files = lcl_pdf_splitter=>split_pdf( i_pdf   = output-pdf
                                                   i_pages = output-pages ).

  DO p_copies TIMES.

    lcl_spooler=>send_to_spool( lt_splitted_files ).

  ENDDO.
