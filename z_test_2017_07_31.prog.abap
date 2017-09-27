*&---------------------------------------------------------------------*
*& Report z_test_2017_07_31
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_07_31.

PARAMETERS: times TYPE i OBLIGATORY DEFAULT 100,
            size  TYPE i OBLIGATORY DEFAULT 100.

CLASS test_performance DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      run.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_data,
             uname TYPE sy-uname,
           END OF ty_data,
           tty_data TYPE STANDARD TABLE OF ty_data
                         WITH NON-UNIQUE DEFAULT KEY.

    DATA: mt_data TYPE test_performance=>tty_data,
          t1      TYPE i,
          t2      TYPE i.

    METHODS:
      _get_data_returning
        RETURNING
          VALUE(rt_data) TYPE tty_data,

      _get_data_exporting
        EXPORTING
          et_data TYPE test_performance=>tty_data,

      _start,

      _stop
        RETURNING
          VALUE(r_elapsed_time) TYPE mengv13.

ENDCLASS.

CLASS test_performance IMPLEMENTATION.

  METHOD run.

    DATA: elapsed_time TYPE i.

    DO times TIMES.

      WRITE: / |Run { sy-index }|.

      _start( ).
      mt_data = _get_data_returning( ).
      elapsed_time = _stop( ).

      WRITE: |Returning: { elapsed_time }|.

      _start( ).
      _get_data_exporting(
        IMPORTING
          et_data = mt_data ).
      elapsed_time = _stop( ).

      WRITE: |Exporting: { elapsed_time }|.

    ENDDO.

  ENDMETHOD.

  METHOD _get_data_returning.

    rt_data = VALUE #( FOR i = 0 WHILE i < size
                       ( uname = sy-index ) ).

  ENDMETHOD.

  METHOD _get_data_exporting.

    et_data = VALUE #( FOR i = 0 WHILE i < size
                       ( uname = sy-index ) ).

  ENDMETHOD.

  METHOD _start.

    CLEAR: mt_data.
    GET RUN TIME FIELD t1.

  ENDMETHOD.

  METHOD _stop.

    GET RUN TIME FIELD t2.
    r_elapsed_time = t2 - t1.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_performance( )->run( ).
