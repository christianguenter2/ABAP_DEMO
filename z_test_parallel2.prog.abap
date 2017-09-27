*&---------------------------------------------------------------------*
*& Report z_test_parallel2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_parallel2.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    DATA: in_tab        TYPE cl_abap_parallel=>t_in_tab,
          in            LIKE LINE OF in_tab,
          out_tab       TYPE cl_abap_parallel=>t_out_tab,
          t100_tab      TYPE STANDARD TABLE OF t100
                        WITH NON-UNIQUE DEFAULT KEY,
          temp_t100_tab LIKE t100_tab.

    SELECT DISTINCT arbgb
           FROM t100
           INTO TABLE @DATA(arbgb_tab).

    LOOP AT arbgb_tab ASSIGNING FIELD-SYMBOL(<arbgb>).

      EXPORT arbgb = <arbgb>-arbgb TO DATA BUFFER in.
      INSERT in INTO TABLE in_tab.

    ENDLOOP.

    NEW zcl_test_abap_parallel_2( )->run(
      EXPORTING
        p_in_tab  = in_tab
      IMPORTING
        p_out_tab = out_tab ).

    LOOP AT out_tab ASSIGNING FIELD-SYMBOL(<out>).

      IMPORT t100_tab = temp_t100_tab FROM DATA BUFFER <out>-result.
      INSERT LINES OF temp_t100_tab INTO TABLE t100_tab.

    ENDLOOP.

    cl_demo_output=>display( lines( t100_tab ) ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->start( ).
