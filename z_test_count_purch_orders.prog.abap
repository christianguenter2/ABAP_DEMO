*&---------------------------------------------------------------------*
*& Report  Z_TEST_COUNT_PURCH_ORDERS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_count_purch_orders.

TYPES: BEGIN OF ty_data,
         lifnr TYPE lifnr,
         count TYPE i,
       END OF ty_data,
       tty_data TYPE STANDARD TABLE OF ty_data.

DATA: lt_data TYPE tty_data.

SELECT lifnr COUNT(*)
       FROM ekpo
       INNER JOIN ekko ON ekpo~ebeln = ekko~ebeln
       INTO TABLE lt_data
       GROUP BY lifnr.

SORT lt_data BY count DESCENDING.

cl_demo_output=>display_data( lt_data ).
