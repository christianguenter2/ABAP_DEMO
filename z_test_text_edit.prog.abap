*&---------------------------------------------------------------------*
*& Report  Z_TEST_TEXT_EDIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_text_edit.

TYPES: c255    TYPE c LENGTH 255.
DATA:  lt_text TYPE STANDARD TABLE OF c255.

CALL FUNCTION 'ISU_POPUP_TEXT_EDIT'
*  EXPORTING
*    x_start_x_pos = 5    " X-Position des Popups
*    x_start_y_pos = 5    " Y-Position des Popups
*    x_height      = 5    " Höhe des Popups
*    x_title       = x_title    " Titel
*    x_no_change   = x_no_change    " Keine Änderung erlaubt
  CHANGING
    xy_texttab    = lt_text
  EXCEPTIONS
    general_fault = 1
    OTHERS        = 2
  .
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

APPEND 'Test' TO lt_text.
APPEND 'Test' TO lt_text.
APPEND 'Test' TO lt_text.
APPEND 'Test' TO lt_text.

CALL FUNCTION 'TRIP_EDIT_TEXT_IN_POPUP'
  EXPORTING
    i_morei                = ''    " Reiseregelungsvariante
    i_editor_type          = 'E'    " Verwendung des Texteditors
*   i_editor_key           = i_editor_key    " Text editor line key
*   i_spkzl                = i_spkzl    " Reisespesenart
*   i_kzpmf                = i_kzpmf    " Fahrzeugart
*   i_kmver                = i_kmver    " Reisefahrtstrecke
*   i_tdid                 = 'FITV' " Text-ID
    i_title                = 'Test'   " Titel des Popup
   i_modus                = 'C' " 1. Stelle des PF-Status
*   i_control_type         = 'T' " Table- oder Edit-Control
  TABLES
    t_tab_editor           = lt_text    " Travel Mgmt. General Purpose Text Editor
  EXCEPTIONS
    default_text_not_found = 1
    OTHERS                 = 2.
IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
