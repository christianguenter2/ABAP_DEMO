*&---------------------------------------------------------------------*
*& Report z_test_parallel
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_parallel.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    DATA: in_tab        TYPE cl_abap_parallel=>t_in_tab,
          out_tab       TYPE cl_abap_parallel=>t_out_tab,
          block         TYPE i,
          xstring       LIKE LINE OF in_tab,
          t100_tab_temp TYPE STANDARD TABLE OF t100,
          result        TYPE STANDARD TABLE OF t100.

    DO 10 TIMES.
      block = sy-index.
      EXPORT block = block TO DATA BUFFER xstring.
      INSERT xstring INTO TABLE in_tab.
    ENDDO.

    GET RUN TIME FIELD DATA(start).

    NEW zcl_test_abap_parallel( )->run(
      EXPORTING
        p_in_tab  = in_tab
      IMPORTING
        p_out_tab = out_tab ).

    GET RUN TIME FIELD DATA(stop).

    LOOP AT out_tab ASSIGNING FIELD-SYMBOL(<out>).

      CLEAR: t100_tab_temp.
      IMPORT t100_tab = t100_tab_temp FROM DATA BUFFER <out>-result.
      INSERT LINES OF t100_tab_temp INTO TABLE result.

    ENDLOOP.

    cl_demo_output=>write( |finished after { stop - start }| ).
    cl_demo_output=>write( |lines: {  lines( result ) }| ).
    cl_demo_output=>display( result ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->start( ).
