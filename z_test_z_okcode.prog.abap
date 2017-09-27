REPORT z_test_z_okcode.

DATA: ok_code TYPE sy-ucomm.

CALL SCREEN 0100.

*----------------------------------------------------------------------*
*  MODULE USER_COMMAND_0100 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  IF sy-subrc = 0.

  ENDIF.
ENDMODULE.                    "USER_COMMAND_0100 INPUT
