FUNCTION zrs_abap_parallel.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(P_CLASS) TYPE  SYCHAR200
*"     VALUE(P_IN) TYPE  XSTRING
*"  EXPORTING
*"     VALUE(P_OUT) TYPE  XSTRING
*"----------------------------------------------------------------------


  DATA:
    l_ref TYPE REF TO zcl_abap_parallel.


  IF same_system IS INITIAL.
    CALL FUNCTION 'RFC_WITHIN_SAME_SYSTEM'
      IMPORTING
        caller_in_same_system = same_system
      EXCEPTIONS
        OTHERS                = 1.
    CHECK sy-subrc = 0.
  ENDIF.

  CHECK same_system = 'Y'.    "Sicherheitsprüfung

  CREATE OBJECT l_ref TYPE (p_class).

  CLEAR p_out.

  l_ref->do( EXPORTING p_in = p_in IMPORTING p_out = p_out ).

ENDFUNCTION.
