*&---------------------------------------------------------------------*
*& Report  Z_TEST_REPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_TEST_REPORT.

TABLES: usr02.

PARAMETERS: p_file type string MODIF ID m1.
parameter p_test TYPE string  MODIF ID m3 MEMORY ID DV1.
PARAMETER p_rad1 TYPE abap_bool RADIOBUTTON GROUP r1.
PARAMETER p_rad2 TYPE abap_bool RADIOBUTTON GROUP r1.
SELECT-OPTIONS: s_test for usr02-bname  MODIF ID m2.

CLASS lcl_application DEFINITION INHERITING FROM cl_aunit_assert.
  PUBLIC SECTION.
    INTERFACES: zif_bc_test_command.
    class-METHODS: start.

ENDCLASS.

CLASS lcl_application IMPLEMENTATION.
  METHOD start.

  ENDMETHOD.
ENDCLASS.

AT SELECTION-SCREEN on VALUE-REQUEST FOR p_file.
  zcl_plm_gui_helper=>value_request_file(
    CHANGING
      c_file = p_file ).

START-OF-SELECTION.
  lcl_application=>start( ).
