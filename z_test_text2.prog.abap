*&---------------------------------------------------------------------*
*& Report  Z_TEST_TEXT2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_text2.

DATA: text_table  TYPE text_lh,
      error_table TYPE text_lh.

CALL FUNCTION 'READ_MULTIPLE_TEXTS'
  EXPORTING
    client                  = sy-mandt    " Mandant
    name                    = '4204692015'    " Textname der zu lesendenTexte
    object                  = 'VBBK'    " Textobjekt der zu suchenden Texte
    id                      = '*'    " Text-Id der zu suchenden Texte
    language                = '*'    " Sprache der zu suchenden Texte
*   name_ranges             = name_ranges    " Tabelle für Textnamen (RANGES)
*   object_ranges           = object_ranges    " Tabelle für Textobjekte (RANGES)
*   id_ranges               = id_ranges    " Tabelle für Textids (RANGES)
*   language_ranges         = language_ranges    " Tabelle für Textsprachen (RANGES)
*   archive_handle          = 0    " ABAP-Systemfeld: Zeilenindex interner Tabellen
*   local_cat               = SPACE    " Textkatalog lokal
*   wildcard_plus           = SPACE    " Textkatalog lokal
  IMPORTING
    text_table              = text_table    " Tabelle mit Text-Header und Inhalten
    error_table             = error_table    " Texte mit Fehlern beim Lesen
  EXCEPTIONS
    wrong_access_to_archive = 1
    OTHERS                  = 2.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

cl_demo_output=>write_data( error_table ).
cl_demo_output=>write_data( text_table ).
cl_demo_output=>display( ).

*call function 'READ_TEXT_TABLE'
**  EXPORTING
**    client_specified        = SPACE    " Mandantenübergreifendes Lesen
**    archive_handle          = 0    " ABAP-Systemfeld: Zeilenindex interner Tabellen
**    local_cat               = SPACE    " Textkatalog lokal
**  IMPORTING
**    text_table              = text_table    " Tabelle mit Text-Header und Inhalten
**    error_table             = error_table    " Texte mit Fehlern beim Lesen
*  TABLES
*    text_headers            = text_headers    " SAPscript: Text-Header
**  EXCEPTIONS
**    wrong_access_to_archive = 1
**    others                  = 2
*  .
*IF sy-subrc <> 0.
** MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*ENDIF.
