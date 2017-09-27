REPORT z_test_user.

DATA: spagpa_tab TYPE STANDARD TABLE OF rfc_spagpa.

spagpa_tab = VALUE #( ( parid = 'XUS'
                        parval = 'TEST' ) ).

CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
  DESTINATION 'TRUSTING@DV1_0120022376'
  EXPORTING
    tcode       = 'SU01'
    skip_screen = 'X'
*   mode_val    = mode_val
*   update_val  = update_val
*  IMPORTING
*   subrc       = subrc
  TABLES
*   using_tab   = using_tab
    spagpa_tab  = spagpa_tab
*   mess_tab    = mess_tab
*  EXCEPTIONS
*   call_transaction_denied = 1
*   tcode_invalid           = 2
*   others      = 3
  .
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

DATA: bt_data  TYPE STANDARD TABLE OF bdcdata,
      l_errors TYPE STANDARD TABLE OF bdcmsgcoll.

*CALL FUNCTION 'RFC_CALL_TRANSACTION_USING'
*  DESTINATION 'TRUSTING@DV1_0120022376'
*  EXPORTING
*    tcode                   = 'SU01D'    " Transaktion
*    mode                    = ' '    " 'N' Keine Anzeige, 'E' Anzeige bei Fehler,'A' Al
**  importing
**   SUBRC                   =     " SY-SUBRC von Call Transaction Using
*  TABLES
*    bt_data                 = bt_data   " BDC-Tabelle für Batchinput
*    l_errors                = l_errors    " Fehler-Liste
*  EXCEPTIONS
*    authority_not_available = 1
*    OTHERS                  = 2.
*IF sy-subrc <> 0.
*  MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*ENDIF.
