*&---------------------------------------------------------------------*
*& Report  Z_TEST_PM_WDA_SAP_GUI
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_pm_wda_sap_gui.

DATA: ok_code TYPE sy-ucomm,
      g_wda_application TYPE wdy_wb_appl_name,
      g_theme TYPE char255.

*----------------------------------------------------------------------*
*       CLASS lcl_screen DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_screen DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: class_constructor,
                   set_dropdown_values,
                   status_0100,
                   user_command_0100.

  PRIVATE SECTION.
    CLASS-METHODS: wda_parameters,
                   inplace_wda,
                   create_container IMPORTING i_container_name TYPE csequence
                                    CHANGING  c_container TYPE REF TO cl_gui_custom_container,
                   create_alv_grid  CHANGING c_grid TYPE REF TO cl_gui_alv_grid,
                   get_webdynpro_applications RETURNING value(rt_applications) TYPE stringtab,
                   get_wda_properties IMPORTING i_application_name   TYPE wdy_application-application_name
                                      RETURNING value(rt_properties) TYPE wdy_app_property_table,
                   data_changed_handler FOR EVENT data_changed OF cl_gui_alv_grid
                                        IMPORTING sender er_data_changed.
    CLASS-DATA: cust_container TYPE REF TO cl_gui_custom_container,
                cust_container_parameter TYPE REF TO cl_gui_custom_container,
                alv_grid TYPE REF TO cl_gui_alv_grid,
                lt_parameters TYPE wdy_app_property_table.
ENDCLASS.                    "lcl_screen DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_screen IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_screen IMPLEMENTATION.
  METHOD class_constructor.

  ENDMETHOD.                    "class_constructor

  METHOD set_dropdown_values.
    DATA: values TYPE vrm_values,
          value  TYPE vrm_value,
          lt_services TYPE stringtab.
    FIELD-SYMBOLS: <service> LIKE LINE OF lt_services.

    IF g_theme IS INITIAL.
      g_theme = 'sap_corbu'.
    ENDIF.

    lt_services = get_webdynpro_applications( ).

    LOOP AT lt_services ASSIGNING <service>.
      value-key = <service>.
      INSERT value INTO TABLE values.
    ENDLOOP.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'G_WDA_APPLICATION'    " Name of Value Set
        values          = values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 DISPLAY LIKE sy-msgty.
    ENDIF.
  ENDMETHOD.                    "set_dropdown_values

  METHOD create_alv_grid.
    DATA: lt_fcat TYPE lvc_t_fcat,
          lt_tb_exl TYPE ui_functions,
          lv_tb_exl LIKE LINE OF lt_tb_exl .

    FIELD-SYMBOLS: <fcat> LIKE LINE OF lt_fcat.

    IF c_grid IS BOUND.
      c_grid->finalize( ).
      c_grid->free(
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3 ).
    ENDIF.

    CREATE OBJECT c_grid
      EXPORTING
        i_parent          = cust_container_parameter    " Parent Container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'WDY_APP_PROPERTY'    " Internal Output Table Structure Name
      CHANGING
        ct_fieldcat            = lt_fcat    " Field Catalog with Field Descriptions
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    READ TABLE lt_fcat ASSIGNING <fcat>
                       WITH KEY fieldname = 'VALUE'.
    IF sy-subrc = 0.
      <fcat>-edit      = abap_true.
      <fcat>-scrtext_s = 'Value'.
      <fcat>-scrtext_m = 'Value'.
      <fcat>-scrtext_l = 'Value'.
      <fcat>-coltext   = 'Value'.
    ENDIF.

    READ TABLE lt_fcat ASSIGNING <fcat>
                       WITH KEY fieldname = 'NAME'.
    IF sy-subrc = 0.
      <fcat>-scrtext_s = 'Name'.
      <fcat>-scrtext_m = 'Name'.
      <fcat>-scrtext_l = 'Name'.
      <fcat>-coltext   = 'Name'.
    ENDIF.

    READ TABLE lt_fcat ASSIGNING <fcat>
                       WITH KEY fieldname = 'APPLICATION_NAME'.
    IF sy-subrc = 0.
      DELETE TABLE lt_fcat FROM <fcat>.
    ENDIF.

    lv_tb_exl = cl_gui_alv_grid=>mc_fc_excl_all.
    INSERT lv_tb_exl INTO TABLE lt_tb_exl.

    c_grid->register_edit_event(
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter
      EXCEPTIONS
        error      = 1
        OTHERS     = 2 ).

    SET HANDLER data_changed_handler FOR c_grid.

    c_grid->set_table_for_first_display(
      EXPORTING
        it_toolbar_excluding          = lt_tb_exl    " Excluded Toolbar Standard Functions
      CHANGING
        it_outtab                     = lt_parameters    " Output Table
        it_fieldcatalog               = lt_fcat    " Field Catalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4 ).
  ENDMETHOD.                    "create_alv_grid

  METHOD get_webdynpro_applications.
    SELECT application_name FROM wdy_application
                            INTO TABLE rt_applications
                            WHERE application_name LIKE 'Z%'.
  ENDMETHOD.                    "get_webdynpro_applications

  METHOD get_wda_properties.
    SELECT * FROM wdy_app_property
             INTO TABLE rt_properties
             WHERE application_name = i_application_name.
  ENDMETHOD.                    "get_wda_properties

  METHOD data_changed_handler.
    FIELD-SYMBOLS: <parameters> LIKE lt_parameters,
                   <parameter> LIKE LINE OF lt_parameters,
                   <parameter2> LIKE LINE OF lt_parameters.

    ASSIGN er_data_changed->mp_mod_rows->* TO <parameters>.

    CHECK sy-subrc = 0.

    LOOP AT <parameters> ASSIGNING <parameter>.
      READ TABLE lt_parameters ASSIGNING <parameter2>
                               WITH KEY name = <parameter>-name.
      CHECK sy-subrc = 0.

      <parameter2>-value = <parameter>-value.
    ENDLOOP.

    inplace_wda( ).
  ENDMETHOD.                    "data_changed_handler

  METHOD status_0100.
    SET PF-STATUS 'STANDARD'.

    IF g_wda_application <> 'z_pm_list_01'.
      LOOP AT SCREEN.
        IF screen-name = 'G_QMART'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.                    "status_0100

  METHOD user_command_0100.
    CASE ok_code.
      WHEN  '&F03' OR '&F15' OR '&F12'.
        SET SCREEN 0.
      WHEN 'WDA'.
        wda_parameters( ).
        inplace_wda( ).
      WHEN 'REFR'.
        inplace_wda( ).
    ENDCASE.
  ENDMETHOD.                    "USER_COMMAND_0100

  METHOD wda_parameters.
    lt_parameters = get_wda_properties( i_application_name = g_wda_application ).

    create_container( EXPORTING i_container_name = 'CUST_CONTAINER_PARAMETER'
                      CHANGING  c_container      = cust_container_parameter ).


    create_alv_grid( CHANGING c_grid = alv_grid ).
  ENDMETHOD.                    "wda_parameters

  METHOD inplace_wda.
    DATA: parameters TYPE tihttpnvp,
          parameter TYPE LINE OF tihttpnvp.

    FIELD-SYMBOLS: <par> LIKE LINE OF lt_parameters.

    CHECK g_wda_application IS NOT INITIAL.

    parameter-name  = 'WDTHEMEROOT'.
    parameter-value = g_theme.
    INSERT parameter INTO TABLE parameters.

    LOOP AT lt_parameters ASSIGNING <par>.
      parameter-name  = to_upper( <par>-name ).
      parameter-value = <par>-value.
      INSERT parameter INTO TABLE parameters.
    ENDLOOP.

    create_container( EXPORTING i_container_name = 'CUST_CONTAINER'
                      CHANGING  c_container      = cust_container ).

    CALL FUNCTION 'Z_BC_WEDYNPRO_IN_SAP_GUI'
      EXPORTING
        i_application_name = g_wda_application
        i_container        = cust_container
        it_parameters      = parameters.
  ENDMETHOD.                    "inplace_wda

  METHOD create_container.
    IF c_container IS BOUND.
      c_container->finalize( ).
      c_container->free(
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3 ).
    ENDIF.

    CREATE OBJECT c_container
      EXPORTING
        container_name              = i_container_name    " Name of the Screen CustCtrl Name to Link Container To
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDMETHOD.                    "create_container
ENDCLASS.                    "lcl_screen IMPLEMENTATION

START-OF-SELECTION.
  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  lcl_screen=>status_0100( ).
  lcl_screen=>set_dropdown_values( ).
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  lcl_screen=>user_command_0100( ).
ENDMODULE.                 " USER_COMMAND_0100  INPUT
