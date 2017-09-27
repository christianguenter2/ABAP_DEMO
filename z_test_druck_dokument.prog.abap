*&---------------------------------------------------------------------*
*& Report  Z_TEST_DRUCK_DOKUMENT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_druck_dokument.

PARAMETERS: p_dokar TYPE draw-dokar OBLIGATORY,
            p_doknr TYPE draw-doknr OBLIGATORY,
            p_dokvr TYPE draw-dokvr OBLIGATORY,
            p_doktl TYPE draw-doktl OBLIGATORY.

DATA: lt_content      TYPE zdrao_t,
      xstring         TYPE xstring,
      lv_outputparams TYPE sfpoutputparams,
      funcname        TYPE funcname.

FIELD-SYMBOLS: <content> LIKE LINE OF lt_content.

CALL FUNCTION 'Z_DOKUMENT_TO_TABLE_2'
  EXPORTING
    i_dokar   = p_dokar    " Dokumentart
    i_doknr   = p_doknr    " Dokumentnummer
    i_dokvr   = p_dokvr    " Dokumentversion
    i_doktl   = p_doktl    " Teildokument
    i_content = 'X'    " Content ermitteln
    i_bin     = ' '    " Binäre Ausgabe (1024)
    i_hex     = 'X'    " Hexadezimale Ausgabe (2550)
  TABLES
    t_content = lt_content.    " Originale für Dokumente

LOOP AT lt_content ASSIGNING <content>
                    WHERE filename CP '*JPG'.
*                   WHERE filename CP '*BMP'.
*                    WHERE filename CP '*PDF'.
  xstring = xstring && <content>-orblk.
ENDLOOP.

CHECK xstring IS NOT INITIAL.

CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
  EXPORTING
    i_name     = 'Z_TEST_DOCUMENT'
  IMPORTING
    e_funcname = funcname.

CALL FUNCTION 'FP_JOB_OPEN'
  CHANGING
    ie_outputparams = lv_outputparams    " Formularprozessierung Ausgabeparameter
  EXCEPTIONS
    cancel          = 1
    usage_error     = 2
    system_error    = 3
    internal_error  = 4
    OTHERS          = 5.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE sy-msgty.
ENDIF.

CALL FUNCTION funcname
  EXPORTING
    i_bild         = xstring
  EXCEPTIONS
    usage_error    = 1
    system_error   = 2
    internal_error = 3
    OTHERS         = 4.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE sy-msgty.
ENDIF.

CALL FUNCTION 'FP_JOB_CLOSE'
  EXCEPTIONS
    usage_error    = 1
    system_error   = 2
    internal_error = 3
    OTHERS         = 4.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE sy-msgty.
ENDIF.
