CLASS zcl_test_abap_parallel DEFINITION
  PUBLIC
  INHERITING FROM zcl_abap_parallel
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      do REDEFINITION.

ENDCLASS.



CLASS ZCL_TEST_ABAP_PARALLEL IMPLEMENTATION.


  METHOD do.

    CONSTANTS co_block_size TYPE i VALUE 100.

    DATA: block    TYPE i,
          lower    TYPE i,
          rows     TYPE i,
          t100_tab TYPE STANDARD TABLE OF t100,
          x type i.

    IMPORT block = block FROM DATA BUFFER p_in.

    lower = block * co_block_size - co_block_size.
    rows  = block * co_block_size.

    SELECT * FROM t100
             INTO TABLE t100_tab
             UP TO rows ROWS.

    DO lower TIMES.

      DELETE t100_tab INDEX 1.

    ENDDO.

    DO 100000000 TIMES.

      x = 13 + 29.

    ENDDO.

    EXPORT t100_tab = t100_tab TO DATA BUFFER p_out.

  ENDMETHOD.
ENDCLASS.
