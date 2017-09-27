*&---------------------------------------------------------------------*
*& Report  Z_TEST_GRID_FOCUS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_grid_focus.

DATA: ok_code TYPE syst-ucomm.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: class_constructor,
                   status_0100,
                   user_command_0100,
                   status_0101,
                   user_command_0101,
                   exit.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_data,
             matnr TYPE mara-matnr,
             btgew TYPE mara-brgew,
           END OF ty_data.
    CLASS-DATA: container1 TYPE REF TO cl_gui_custom_container,
                container2 TYPE REF TO cl_gui_custom_container,
                container3 TYPE REF TO cl_gui_custom_container,
                alv1 TYPE REF TO cl_gui_alv_grid,
                alv2 TYPE REF TO cl_salv_table,
                alv3 TYPE REF TO cl_salv_table,
                lt_data TYPE STANDARD TABLE OF mara.
    CLASS-DATA: focus TYPE i VALUE 1.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD class_constructor.
    DO 3 TIMES.
      INSERT INITIAL LINE INTO TABLE lt_data.
    ENDDO.
  ENDMETHOD.                    "class_constructor


  METHOD status_0100.
    data: row  TYPE LVC_S_ROW,
          col  TYPE LVC_S_COL,
          roid TYPE LVC_S_ROID,
          lt_fieldcatalog TYPE LVC_T_FCAT.

    SET PF-STATUS 'DEFAULT'.

    IF container1 IS NOT BOUND.
      CREATE OBJECT container1
        EXPORTING
          container_name              = 'CUST_CONTAINER_1'    " Name of the Screen CustCtrl Name to Link Container To
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.

      create OBJECT alv1
        EXPORTING
          i_parent          = container1    " Parent-Container
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          others            = 5.

      alv1->set_table_for_first_display(
        EXPORTING
*          i_buffer_active               =     " Pufferung aktiv
*          i_bypassing_buffer            =     " Puffer ausschalten
*          i_consistency_check           =     " Starte Konsistenzverprobung für Schnittstellefehlererkennung
          i_structure_name              = 'MARA'    " Strukturname der internen Ausgabetabelle
*          is_variant                    =     " Anzeigevariante
*          i_save                        =     " Anzeigevariante sichern
*          i_default                     = 'X'    " Defaultanzeigevariante
*          is_layout                     =     " Layout
*          is_print                      =     " Drucksteuerung
*          it_special_groups             =     " Feldgruppen
*          it_toolbar_excluding          =     " excludierte Toolbarstandardfunktionen
*          it_hyperlink                  =     " Hyperlinks
*          it_alv_graphics               =     " Tabelle von der Struktur DTC_S_TC
*          it_except_qinfo               =     " Tabelle für die Exception Quickinfo
*          ir_salv_adapter               =     " Interface ALV Adapter
        CHANGING
          it_outtab                     = lt_data    " Ausgabetabelle
          it_fieldcatalog               = lt_fieldcatalog
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          others                        = 4 ).
    ENDIF.

    IF container2 IS NOT BOUND.
      CREATE OBJECT container2
        EXPORTING
          container_name              = 'CUST_CONTAINER_2'    " Name of the Screen CustCtrl Name to Link Container To
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.

      cl_salv_table=>factory(
        EXPORTING
*          list_display   = IF_SALV_C_BOOL_SAP=>FALSE    " ALV Displayed in List Mode
          r_container    = container2    " Abstract Container for GUI Controls
*          container_name = 'CUST_CONTAINER_1'
        IMPORTING
          r_salv_table   = alv2    " Basis Class Simple ALV Tables
        CHANGING
          t_table        = lt_data ).

      alv2->display( ).
    ENDIF.

    CASE focus.
      WHEN '1'.
        cl_gui_alv_grid=>set_focus(
          EXPORTING
            control           = container1    " Control
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3 ).

        row-index = 2.
        col-fieldname = 'ERNAM'.
*        roid-row_id = 2.

        alv1->set_current_cell_via_id(
          EXPORTING
            is_row_id    = row    " Zeile
            is_column_id = col    " Spalte
            is_row_no    = roid    " Numerische Zeilen ID
        ).
      WHEN '2'.
        cl_gui_alv_grid=>set_focus(
          EXPORTING
            control           = container2   " Control
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3 ).
      WHEN '3'.
        cl_gui_alv_grid=>set_focus(
          EXPORTING
            control           = container3    " Control
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3 ).
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "status_0100

  METHOD user_command_0100.
    CASE ok_code.
      WHEN 'FOCUS'.
        IF focus = 1.
          focus = 2.
        ELSEIF focus = 2.
          focus = 3.
        ELSE.
          focus = 1.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "USER_COMMAND_0100

  METHOD status_0101.
    IF container3 IS NOT BOUND.
      CREATE OBJECT container3
        EXPORTING
          container_name              = 'CUST_CONTAINER_3'    " Name of the Screen CustCtrl Name to Link Container To
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.

      cl_salv_table=>factory(
        EXPORTING
*          list_display   = IF_SALV_C_BOOL_SAP=>FALSE    " ALV Displayed in List Mode
          r_container    = container3    " Abstract Container for GUI Controls
*          container_name = 'CUST_CONTAINER_1'
        IMPORTING
          r_salv_table   = alv3    " Basis Class Simple ALV Tables
        CHANGING
          t_table        = lt_data ).

      alv3->display( ).
    ENDIF.
  ENDMETHOD.                    "status_0101

  METHOD user_command_0101.

  ENDMETHOD.                    "user_command_0101

  METHOD exit.
    CASE ok_code.
      WHEN 'BACK' OR 'CANC' OR 'E' OR 'ENDE' OR'ECAN'.
        LEAVE TO SCREEN 0.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "exit
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  lcl_application=>status_0100( ).
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  lcl_application=>user_command_0100( ).
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  lcl_application=>exit( ).
ENDMODULE.                 " EXIT  INPUT

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TABSTRIP'
CONSTANTS: BEGIN OF c_tabstrip,
             tab1 LIKE sy-ucomm VALUE 'TABSTRIP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TABSTRIP_FC2',
           END OF c_tabstrip.
*&SPWIZARD: DATA FOR TABSTRIP 'TABSTRIP'
CONTROLS:  tabstrip TYPE TABSTRIP.
DATA:      BEGIN OF g_tabstrip,
             subscreen   LIKE sy-dynnr,
             prog        LIKE sy-repid VALUE 'Z_TEST_GRID_FOCUS',
             pressed_tab LIKE sy-ucomm VALUE c_tabstrip-tab1,
           END OF g_tabstrip.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TABSTRIP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE tabstrip_active_tab_set OUTPUT.
  tabstrip-activetab = g_tabstrip-pressed_tab.
  CASE g_tabstrip-pressed_tab.
    WHEN c_tabstrip-tab1.
      g_tabstrip-subscreen = '0101'.
    WHEN c_tabstrip-tab2.
      g_tabstrip-subscreen = '0102'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TABSTRIP_ACTIVE_TAB_SET OUTPUT

*&SPWIZARD: INPUT MODULE FOR TS 'TABSTRIP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tabstrip_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tabstrip-tab1.
      g_tabstrip-pressed_tab = c_tabstrip-tab1.
    WHEN c_tabstrip-tab2.
      g_tabstrip-pressed_tab = c_tabstrip-tab2.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TABSTRIP_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  lcl_application=>status_0101( ).
ENDMODULE.                 " STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  lcl_application=>user_command_0101( ).
ENDMODULE.                 " USER_COMMAND_0110  INPUT
