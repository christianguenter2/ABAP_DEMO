REPORT z_test_alv_no_merge.

*----------------------------------------------------------------------*
*       CLASS lcl_alv_no_Merge DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_no_merge DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      start,
      pbo,
      pai,
      class_constructor.

  PRIVATE SECTION.
    CLASS-DATA: mt_mara TYPE mara_tab.

ENDCLASS.                    "lcl_alv_no_Merge DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_alv_no_Merge IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_no_merge IMPLEMENTATION.

  METHOD class_constructor.

    SELECT *
           FROM mara
           INTO TABLE mt_mara
           UP TO 100 ROWS.

  ENDMETHOD.                    "class_constructor

  METHOD
  start.

    CALL SCREEN 0100.

  ENDMETHOD.                    "start

  METHOD pbo.

    DATA: lo_alv           TYPE REF TO cl_gui_alv_grid,
          lt_fieldcatalog  TYPE lvc_t_fcat,
          layout           TYPE lvc_s_layo,
          variant          TYPE disvariant,
          dummy_alv        TYPE REF TO cl_salv_table,
          custom_container TYPE REF TO cl_gui_custom_container.


    FIELD-SYMBOLS: <fieldcat> LIKE LINE OF lt_fieldcatalog.

    CREATE OBJECT custom_container
      EXPORTING
        container_name              = 'CONTAINER'    " Name of the dynpro CustCtrl name to link this container to
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT lo_alv
      EXPORTING
        i_parent          = custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*      EXPORTING
*        i_structure_name       = 'MARA'
*      CHANGING
*        ct_fieldcat            = lt_fieldcatalog
*      EXCEPTIONS
*        inconsistent_interface = 1
*        program_error          = 2
*        OTHERS                 = 3.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.

    cl_salv_table=>factory(
      IMPORTING
        r_salv_table   = dummy_alv
      CHANGING
        t_table        = mt_mara ).

    zcl_bc_alv_utilities=>do_alv_default_config(
      EXPORTING
        i_alv   = dummy_alv ).

    dummy_alv->get_columns( )->get_column( `MATNR` )->set_long_text( `Hallo Welt` ).

    cl_salv_controller_metadata=>get_lvc_fieldcatalog(
      EXPORTING
        r_columns      = dummy_alv->get_columns( )
        r_aggregations = dummy_alv->get_aggregations( )
      RECEIVING
        t_fieldcatalog = lt_fieldcatalog ).

    LOOP AT lt_fieldcatalog ASSIGNING <fieldcat>.


    ENDLOOP.

    variant-report   = cl_abap_syst=>get_current_program( ).
    variant-username = sy-uname.

    DATA: lt_f4 TYPE lvc_t_f4,
          lv_f4 LIKE LINE OF lt_f4.

    lv_f4-fieldname  = 'MAT_NO'.
    lv_f4-register   = abap_true.
    lv_f4-getbefore  = abap_true.
    lv_f4-chngeafter = abap_true.
    INSERT lv_f4 INTO TABLE lt_f4.

    lo_alv->register_f4_for_fields( lt_f4 ).

    lo_alv->set_table_for_first_display(
      EXPORTING
        is_layout                     = layout
        is_variant                    = variant
        i_default                     = 'X'
        i_save                        = 'A'
      CHANGING
        it_outtab                     = mt_mara
        it_fieldcatalog               = lt_fieldcatalog    " Feldkatalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "pbo

  METHOD pai.

  ENDMETHOD.                    "pai

ENDCLASS.                    "lcl_alv_no_Merge IMPLEMENTATION

START-OF-SELECTION.
  lcl_alv_no_merge=>start( ).

*----------------------------------------------------------------------*
*  MODULE pbo_0100 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  lcl_alv_no_merge=>pbo( ).
ENDMODULE.                    "pbo_0100 OUTPUT

*----------------------------------------------------------------------*
*  MODULE pai_0100 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  lcl_alv_no_merge=>pai( ).
ENDMODULE.                    "pai_0100 INPUT
