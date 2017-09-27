*&---------------------------------------------------------------------*
*& Report z_test_750_2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_750_2.
TYPES:
  BEGIN OF ty_carr_name,
    ai TYPE string, " Air India
    ek TYPE string, " Emirates
    lh TYPE string, " Lufthansa
  END OF ty_carr_name.
" AI & EK do not exist in SCARR !!!
SELECT * FROM scarr INTO TABLE @DATA(carriers).
" Using VALUE with table expressions

DATA(carrier_names) = VALUE ty_carr_name(
    ai = VALUE #( carriers[ carrid = 'AI' ]-carrname OPTIONAL )
    ek = VALUE #( carriers[ carrid = 'EK' ]-carrname OPTIONAL )
    lh = VALUE #( carriers[ carrid = 'LH' ]-carrname OPTIONAL )
).

DATA(html_output)
= cl_demo_output=>new(
  )->begin_section( `Using VALUE with table expressions`
     )->write_data( carrier_names
        )->end_section( ).
" Using VALUE with table reductions
carrier_names = VALUE #(
  ai = REDUCE #( INIT x TYPE string
                 FOR <wa> IN carriers WHERE ( carrid = 'AI' ) NEXT x = <wa>-carrname )
  ek = REDUCE #( INIT x TYPE string
                 FOR <wa> IN carriers WHERE ( carrid = 'EK' ) NEXT x = <wa>-carrname )
  lh = REDUCE #( INIT x TYPE string
                 FOR <wa> IN carriers WHERE ( carrid = 'LH' ) NEXT x = <wa>-carrname )
).
html_output->begin_section( `Using VALUE with table reductions`
)->write_data( carrier_names )->end_section( )->display( ).
