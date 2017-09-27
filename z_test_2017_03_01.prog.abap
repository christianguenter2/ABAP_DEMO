*&---------------------------------------------------------------------*
*& Report z_test_2017_03_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_03_01.

TABLES: t100.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_sprsl FOR t100-sprsl,
                s_arbgb FOR t100-arbgb,
                s_msgnr FOR t100-msgnr,
                s_text  FOR t100-text.
PARAMETERS: p_rows TYPE i DEFAULT 50 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

CLASS view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_2017_03_01_view.

    ALIASES:
      m_current_program FOR zif_2017_03_01_view~m_current_program.

    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO view.

    METHODS:
      constructor,
      pbo_0100,
      pai_0100.

  PRIVATE SECTION.
    CLASS-DATA _instance TYPE REF TO view.

ENDCLASS.

CLASS view IMPLEMENTATION.

  METHOD get_instance.

    IF _instance IS NOT BOUND.

      _instance = NEW view( ).

    ENDIF.

    r_instance = _instance.

  ENDMETHOD.

  METHOD constructor.

    m_current_program = cl_abap_syst=>get_current_program( ).

  ENDMETHOD.

  METHOD zif_2017_03_01_view~display.

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD pbo_0100.

    TRY.
        zcl_2017_03_01_controller=>get_instance( )->pbo_0100( ).

      CATCH zcx_2017_03_01 INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD pai_0100.

    TRY.
        zcl_2017_03_01_controller=>get_instance( )->pai_0100( ).

      CATCH zcx_2017_03_01 INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      zcl_2017_03_01_controller=>get_instance( io_view       = view=>get_instance( )
                                               io_selections = NEW zcl_2017_03_01_selections( view=>get_instance( )->m_current_program )
                              )->start( ).

    CATCH zcx_2017_03_01 INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

MODULE pbo_0100 OUTPUT.

  view=>get_instance( )->pbo_0100( ).

ENDMODULE.

MODULE pai_0100 INPUT.

  view=>get_instance( )->pai_0100( ).

ENDMODULE.
