*&---------------------------------------------------------------------*
*& Report z_test_dynamic_loop
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_dynamic_loop.

SELECT FROM t100
       FIELDS *
       INTO TABLE @DATA(lt_t100)
       UP TO 100 ROWS.

SELECT FROM t100
       FIELDS *
       WHERE arbgb LIKE 'Z%'
       INTO TABLE @DATA(lt_t1002)
       UP TO 100 ROWS.

DO 2 TIMES.

  CASE sy-index.
    WHEN 1.

      DATA(tab_ref) = REF #( lt_t100 ).

    WHEN 2.

      tab_ref = REF #( lt_t1002 ).

  ENDCASE.

  LOOP AT tab_ref->* ASSIGNING FIELD-SYMBOL(<t100>).

    cl_demo_output=>write( <t100> ).

  ENDLOOP.

ENDDO.

cl_demo_output=>display( ).
