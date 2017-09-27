*&---------------------------------------------------------------------*
*& Report  Z_TEST_ICON_CREATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_TEST_ICON_CREATE.

data: icon TYPE icons-text.

call FUNCTION 'ICON_CREATE'
  EXPORTING
    name                  = 'ICON_CREATE'    " Ikonenname ( Name aus INCLUDE <ICON> )
*    text                  = SPACE    " Text zur Ikone ( wird dahinter angezeigt )
*    info                  = SPACE    " Quickinfo (falls SPACE: Standardquickinfo)
*    add_stdinf            = 'X'    " 'X' für Qinfo. ' ' unterdrückt jegl. Quickinfo
  IMPORTING
    result                = icon    " Ikone ( geben Sie hier das Dynprofeld an)
  EXCEPTIONS
    icon_not_found        = 1
    outputfield_too_short = 2
    others                = 3
  .
IF sy-subrc <> 0.
 MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

write: icon.
