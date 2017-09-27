*&---------------------------------------------------------------------*
*& Report  z_test_bit_byte
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_bit_byte.

CLASS bit_byte DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.
  PRIVATE SECTION.

    METHODS _as_text
      IMPORTING
        i_hex TYPE x.

ENDCLASS.

CLASS bit_byte IMPLEMENTATION.

  METHOD start.

    DATA: hex  TYPE x LENGTH 1.

    DO 8 TIMES.
      CLEAR hex.
      SET BIT sy-index OF hex TO 1.
      cl_demo_output=>write( |{ hex }| ).

      _as_text( hex ).
    ENDDO.

    CLEAR hex.

    DO 8 TIMES.
      SET BIT sy-index OF hex TO 1.
      cl_demo_output=>write( |{ hex }| ).

      _as_text( hex ).
    ENDDO.

    cl_demo_output=>display( ).

  ENDMETHOD.


  METHOD _as_text.

    DATA: codepage TYPE cpcodepage.

    CALL FUNCTION 'SYSTEM_CODEPAGE'
      IMPORTING
        codepage = codepage.

*    cl_bcs_convert=>xstring_to_string(
*      EXPORTING
*        iv_xstr   = CONV #( i_hex )
*        iv_cp     = codepage    " SAP character set identification
*      RECEIVING
*        rv_string = DATA(text) ).
*
*    cl_demo_output=>write( text ).

    DATA: xstring TYPE xstring,
          text    TYPE string.

    xstring = i_hex.

    CALL FUNCTION 'HR_KR_XSTRING_TO_STRING'
      EXPORTING
*        from_codepage = codepage    " Code page of source encoded string
        in_xstring    = xstring
*       out_len       =     " Length of source encoded string
      IMPORTING
        out_string    = text.    " Output string in unicode

    cl_demo_output=>write( text ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW bit_byte( )->start( ).
