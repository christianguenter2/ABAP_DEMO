*&---------------------------------------------------------------------*
*& Report z_test_750_3
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_750_3.

DATA(rnd) = cl_abap_random_int=>create(
                    seed = CONV #( sy-uzeit )
                    min  = 0
                    max  = 100 ).

DATA(random_numbers) = VALUE int_tab1(
                         FOR n = 0
                         WHILE n < 10
                         ( rnd->get_next( ) ) ).

IF sy-subrc = 0.

ENDIF.
