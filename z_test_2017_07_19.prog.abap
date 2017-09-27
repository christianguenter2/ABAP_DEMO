*&---------------------------------------------------------------------*
*& Report z_test_2017_07_19
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_07_19.

INTERFACE if_callback.
  METHODS:
    execute
      IMPORTING
        it_data TYPE INDEX TABLE.
ENDINTERFACE.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

  PRIVATE SECTION.
    METHODS:
      _process
        IMPORTING
          io_callback TYPE REF TO if_callback,

      _process2
        IMPORTING
          io_callback TYPE REF TO if_callback.

ENDCLASS.

CLASS callback DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_callback.

    METHODS:
      finish.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    DATA(lo_callback) = NEW callback( ).

    _process( lo_callback ).
    _process2( lo_callback ).

    lo_callback->finish( ).

  ENDMETHOD.

  METHOD _process.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @DATA(t100_tab)
           UP TO 100 ROWS.

    io_callback->execute( it_data = t100_tab ).

  ENDMETHOD.

  METHOD _process2.

    SELECT FROM t100a
           FIELDS *
           INTO TABLE @DATA(t100a_tab)
           UP TO 100 ROWS.

    io_callback->execute( it_data = t100a_tab ).

  ENDMETHOD.

ENDCLASS.

CLASS callback IMPLEMENTATION.

  METHOD if_callback~execute.

    cl_demo_output=>write( it_data ).

  ENDMETHOD.

  METHOD finish.

    cl_demo_output=>display( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->start( ).
