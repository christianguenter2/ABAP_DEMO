*&---------------------------------------------------------------------*
*& Report  Z_TEST_QUERY
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_query.

DATA: ok_code      TYPE sy-ucomm,
      gs_drop_down TYPE string,
      gs_subscreen TYPE sy-dynnr VALUE '1010'.

SELECTION-SCREEN BEGIN OF SCREEN 1010 AS SUBSCREEN .
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_test TYPE abap_bool.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 1010.

SELECTION-SCREEN BEGIN OF SCREEN 1020 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS: p_test2 TYPE abap_bool.
PARAMETERS: p_test3 TYPE abap_bool.
PARAMETERS: p_test4 TYPE abap_bool.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF SCREEN 1020.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: set_values,
                   selections.

ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD set_values.
    DATA: values TYPE vrm_values,
          value LIKE LINE OF values.

    value-key = 'key1'.
    value-text = 'Text1'.
    INSERT value INTO TABLE values.

    value-key = 'key2'.
    value-text = 'Text2'.
    INSERT value INTO TABLE values.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'GS_DROP_DOWN'    " Name of Value Set
        values          = values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "set_values

  METHOD selections.
    IF gs_drop_down CP '*1*'.
      gs_subscreen = '1010'.
    ELSE.
      gs_subscreen = '1020'.
    ENDIF.
  ENDMETHOD.                    "selections
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'DEFAULT'.
  lcl_application=>set_values( ).
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  IF ok_code = 'ENTE'.
    lcl_application=>selections( ).
  ENDIF.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0100 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT_COMMAND_0100  INPUT
