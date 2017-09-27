PROGRAM zdemo_dynpro_get_cursor .

DATA: ok_code TYPE sy-ucomm,
      save_ok LIKE ok_code.

DATA: input_output(20) TYPE c,
      fld          TYPE string,
      off              TYPE i,
      val(20)          TYPE c,
      len              TYPE i.

CALL SCREEN 100.

*----------------------------------------------------------------------*
*       CLASS lcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_cursor.
ENDCLASS.                    "lcl_test DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
  METHOD get_cursor.
    GET CURSOR FIELD fld OFFSET off VALUE val LENGTH len.
  ENDMETHOD.                    "get_cursor
ENDCLASS.                    "lcl_test IMPLEMENTATION

*----------------------------------------------------------------------*
*  MODULE init_screen_0100 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE init_screen_0100 OUTPUT.
  SET PF-STATUS 'STATUS_100'.
ENDMODULE.                    "init_screen_0100 OUTPUT

*----------------------------------------------------------------------*
*  MODULE user_command_0100 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'SELE'.
      lcl_test=>get_cursor( ).
  ENDCASE.
ENDMODULE.                    "user_command_0100 INPUT
