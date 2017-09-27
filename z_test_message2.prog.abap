*&---------------------------------------------------------------------*
*& Report z_test_message2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_message2.

*DATA(log) = NEW zcl_log( ).
*
*" Keine Berechtigung
*MESSAGE s099(zhr) INTO DATA(dummy).
*log->add( i_syst = abap_true ).
*
*log->display( ).
