*&---------------------------------------------------------------------*
*& Report  Z_TEST_BAPI_ALM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_bapi_alm.

PARAMETERS: number TYPE bapi2080_nothdre-notif_no OBLIGATORY,
            text TYPE bapi2080_nothdri-short_text OBLIGATORY DEFAULT '1234'.

DATA: notifheader        TYPE bapi2080_nothdri,
      notifheader_x      TYPE bapi2080_nothdri_x ,
      notifheader_export TYPE bapi2080_nothdre,
      return             TYPE STANDARD TABLE OF bapiret2.

notifheader-short_text   = text.
notifheader_x-short_text = abap_true.

CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY'
  EXPORTING
    number             = number    " Meldungsnummer
    notifheader        = notifheader
    notifheader_x      = notifheader_x    " BAPI: Kennzeichen für Meldungskopf
  IMPORTING
    notifheader_export = notifheader_export
  TABLES
    return             = return.    " Returnparameter

CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
  EXPORTING
    number = number
  TABLES
    return = return.    " Nachrichtenart: S = Erfolg, E = Fehler, W = Warnung, I = Inf

COMMIT WORK.

cl_demo_output=>display_data( return ).
