*&---------------------------------------------------------------------*
*& Report z_test_2017_02_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_02_01.

DATA: lines  TYPE STANDARD TABLE OF tline
                 WITH NON-UNIQUE DEFAULT KEY,
      header TYPE thead.




SELECT SINGLE FROM stxh
       FIELDS *
       INTO @DATA(text).

CALL FUNCTION 'READ_TEXT'
  EXPORTING
    id                      = TEXT-tdid    " Text-Id des zu lesenden Textes
    language                = TEXT-tdspras    " Sprache des zu lesenden Textes
    name                    = TEXT-tdname    " Name des zu lesenden Textes
    object                  = TEXT-tdobject    " Objekt des zu lesenden Textes
  IMPORTING
    header                  = header    " Textheader des gelesenen Textes
  TABLES
    lines                   = lines     " Textzeilen des gelesenen Textes
  EXCEPTIONS
    id                      = 1
    language                = 2
    name                    = 3
    not_found               = 4
    object                  = 5
    reference_check         = 6
    wrong_access_to_archive = 7
    OTHERS                  = 8.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

CALL FUNCTION 'EDIT_TEXT'
  EXPORTING
*   display       = SPACE    " Kennzeichen: Anzeigemodus
*   editor_title  = SPACE    " Text für Editor-Titelzeile
    header        = header     " Textheader des zu editierenden Textes
*   page          = SPACE
    window        = space
*   save          = 'X'    " Kennzeichen: Sicherung durch Editor
*   line_editor   = SPACE    " Kennzeichen: Zeileneditor oder PC-Editor
*   control       = SPACE    " Steuerparameter für den Editor
*   program       = SPACE    " Programmname für Programmsymbol-Ersetzung
*   local_cat     = SPACE    " Textcatalog lokal
*  IMPORTING
*   function      =     " Kennzeichen: Bearbeitungsstatus
*   newheader     =     " Text-Header (verändert)
*   result        =     " Statusinformation des Editors
  TABLES
    lines         = lines    " Textzeilen des editierenden Textes
  EXCEPTIONS
    id            = 1
    language      = 2
    linesize      = 3
    name          = 4
    object        = 5
    textformat    = 6
    communication = 7
    OTHERS        = 8.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
