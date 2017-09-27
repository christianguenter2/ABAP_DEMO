*&---------------------------------------------------------------------*
*& Report z_test_2017_08_15
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_08_15.

CLASS lcx_error DEFINITION FINAL
                INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    CLASS-METHODS:
      raise_db_not_found
        RAISING
          lcx_error.

    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL
          text     TYPE csequence OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA:
      _text TYPE string.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid   = textid
                        previous = previous ).

    _text = text.

  ENDMETHOD.

  METHOD raise_db_not_found.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        text = TEXT-001.

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN _text IS NOT INITIAL THEN _text
                     ELSE super->get_text( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      run.

  PRIVATE SECTION.
    DATA: t100_tab TYPE STANDARD TABLE OF t100.

    METHODS:
      _select
        RAISING
          lcx_error,

      _process,

      _display.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD run.

    TRY.
        _select( ).
        _process( ).
        _display( ).

      CATCH lcx_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

    IF sy-subrc <> 0.
      lcx_error=>raise_db_not_found( ).
    ENDIF.

  ENDMETHOD.

  METHOD _process.

    LOOP AT t100_tab INTO DATA(t100)
                     GROUP BY ( msgnr = t100-msgnr )
                     WITHOUT MEMBERS
                     ASSIGNING FIELD-SYMBOL(<t100>).

      DELETE t100_tab WHERE msgnr = <t100>-msgnr.

    ENDLOOP.

  ENDMETHOD.

  METHOD _display.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(alv)
          CHANGING
            t_table      = t100_tab ).

        alv->display( ).

      CATCH cx_salv_msg INTO DATA(error).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->run( ).
