REPORT z_test_2017_02_01.

DATA: lines  TYPE STANDARD TABLE OF tline
                 WITH NON-UNIQUE DEFAULT KEY,
      text   TYPE stxh.

SELECT SINGLE * FROM stxh
       INTO text
       WHERE tdname LIKE '%4204692014%'
       AND   tdid     = '9035'
       AND   tdobject = 'VBBK'.

CALL FUNCTION '/TOPFLOW/ZEIGE_LANGTEXT'
  EXPORTING
    i_id       = text-tdid    " Text-Id des zu lesenden Textes
    i_language = text-tdspras    " Sprachenschlüssel des zu lesenden Textes
    i_name     = text-tdname " Name des zu lesenden Textes
    i_object   = text-tdobject    " Objekt des zu lesenden Textes
    i_title    = 'Test'.    " GUI-Titel-Text für Dynpro

*DATA: ch_text TYPE ldps_txt_tab.
*
*CALL FUNCTION '/TOPFLOW/SIMPLE_TEXT_EDITOR'
*  EXPORTING
*    im_title        = 'Test'
**   im_display_mode = SPACE    " Anzeigemodus 'X' = Ja
**   im_start_column = 10    " Start Spalte fuer Popup Window
**   im_start_row    = 10    " Start Zeile fuer Popup Window
*  CHANGING
*    ch_text         = ch_text.    " Langtexttabelle für CATSXT
*
*cl_demo_output=>display_data( ch_text ).
