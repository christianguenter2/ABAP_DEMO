*&---------------------------------------------------------------------*
*& Report  Z_TEST_WDA_ROADMAP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_wda_roadmap.

DATA: ok_code TYPE sy-ucomm,
      g_step TYPE string,
      g_step_count TYPE int4.

START-OF-SELECTION.
  CALL SCREEN 0100.

*----------------------------------------------------------------------*
*       CLASS lcl_screen DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_screen DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: val_step,
                   pbo_0100,
                   controls,
                   user_command_0100.
  PRIVATE SECTION.
    CLASS-DATA: cust_container TYPE REF TO cl_gui_custom_container.
    CLASS-METHODS: _roadmap.
ENDCLASS.                    "lcl_screen DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_screen IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_screen IMPLEMENTATION.
  METHOD val_step.
    DATA: values TYPE vrm_values,
          value  TYPE vrm_value.

    DO g_step_count TIMES.
      value-key = 'STEP' && sy-index.
      INSERT value INTO TABLE values.
    ENDDO.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'G_STEP'    " Name of Value Set
        values          = values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
  ENDMETHOD.                    "class_constructor

  METHOD pbo_0100.
    SET PF-STATUS 'STANDARD' OF PROGRAM 'Z_TEST_PM_WDA_SAP_GUI'.
  ENDMETHOD.                                                "pbo_0100

  METHOD controls.
    IF cust_container IS BOUND.
      cust_container->finalize( ).
      cust_container->free(
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3 ).
    ENDIF.

    CREATE OBJECT cust_container
      EXPORTING
        container_name              = 'CUST_CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

  ENDMETHOD.                    "controls

  METHOD user_command_0100.
    CASE ok_code.
      WHEN '&F03' OR '&F15' OR '&F12'.
        SET SCREEN 0.
      WHEN OTHERS.
        _roadmap( ).
    ENDCASE.
  ENDMETHOD.                    "user_command_0100

  METHOD _roadmap.
    DATA: parameters TYPE tihttpnvp,
          parameter LIKE LINE OF parameters.

    controls( ).

    parameter-name  = 'WDTHEMEROOT'.
    parameter-value = 'SAP_CORBU'.
    INSERT parameter INTO TABLE parameters.

    parameter-name  = 'STEP'.
    parameter-value = g_step.
    INSERT parameter INTO TABLE parameters.

    parameter-name  = 'STEP_COUNT'.
    parameter-value = g_step_count.
    INSERT parameter INTO TABLE parameters.

    CALL FUNCTION 'Z_BC_WEDYNPRO_IN_SAP_GUI'
      EXPORTING
        i_application_name = 'z_test_roadmap'    " Web Dynpro: Name of Application
        i_container        = cust_container    " Abstract Container for GUI Controls
        it_parameters      = parameters.

  ENDMETHOD.                    "_roadmap
ENDCLASS.                    "lcl_screen IMPLEMENTATION


*----------------------------------------------------------------------*
*  MODULE pbo_0100 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  lcl_screen=>pbo_0100( ).
ENDMODULE.                    "pbo_0100 OUTPUT

*----------------------------------------------------------------------*
*  MODULE user_command_0100 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  lcl_screen=>user_command_0100( ).
ENDMODULE.                    "user_command_0100 INPUT
*&---------------------------------------------------------------------*
*&      Module  VAL_STEP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE val_step INPUT.
  lcl_screen=>val_step( ).
ENDMODULE.                 " VAL_STEP  INPUT
