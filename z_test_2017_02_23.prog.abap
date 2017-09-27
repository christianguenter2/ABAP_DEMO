*&---------------------------------------------------------------------*
*& Report z_test_2017_02_23
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_02_23.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-DATA: okcode TYPE sy-ucomm.

    CLASS-METHODS:
      get
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    METHODS:
      start,
      pai_0100,
      pbo_0100.

  PRIVATE SECTION.

    CLASS-DATA:
      _instance TYPE REF TO controller.

    DATA:
      mt_t100 TYPE STANDARD TABLE OF t100,
      m_alv   TYPE REF TO cl_salv_table.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD get.

    IF _instance IS NOT BOUND.

      _instance = NEW controller( ).

    ENDIF.

    r_instance = _instance.

  ENDMETHOD.

  METHOD start.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @mt_t100
           UP TO 100 ROWS.

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD pbo_0100.

    SET PF-STATUS 'STATUS_0100'.

    IF m_alv IS NOT BOUND.

      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = cl_gui_container=>screen0
            IMPORTING
              r_salv_table   = m_alv
            CHANGING
              t_table        = mt_t100 ).

          m_alv->display( ).

        CATCH cx_salv_error INTO DATA(error).    "
          MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = okcode.

    CLEAR controller=>okcode.

    CASE save_ok_code.
      WHEN 'BACK'
      OR   'CANC'
      OR   'EXIT'.

        SET SCREEN 0.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  controller=>get( )->start( ).
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  controller=>get( )->pbo_0100( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  controller=>get( )->pai_0100( ).
ENDMODULE.
