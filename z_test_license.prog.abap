*&---------------------------------------------------------------------*
*& Report  Z_TEST_LICENSE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_license.

CONSTANTS: c_orders             TYPE rsuvm_unit VALUE '5101',
           c_sales_orders       TYPE rsuvm_unit VALUE '5102',
           c_sched_agreemnt     TYPE rsuvm_unit VALUE '5104',
           c_sched_agreemnt_ext TYPE rsuvm_unit VALUE '5105',
           c_returns            TYPE rsuvm_unit VALUE '5107',
           c_delivery_uncharged TYPE rsuvm_unit VALUE '5108',
           c_gutschrift         TYPE rsuvm_unit VALUE '5109',
           c_debit_memo         TYPE rsuvm_unit VALUE '5110',
           c_charged_unit       TYPE rsuvm_unit VALUE '5212',
           c_charge_free_unit   TYPE rsuvm_unit VALUE '5213'.
*           c_unknown            TYPE rsuvm_unit VALUE '5214'.

TYPES: BEGIN OF ty_data,
         slim_unit   TYPE rsuvm_unit,
         usertyp     TYPE tutypa-usertyp,
         charge_unit TYPE rsuvm_unit,
       END OF ty_data.

DATA: lt_slim_unit TYPE STANDARD TABLE OF rsuvm_unit WITH DEFAULT KEY,
      lt_usertyp   TYPE STANDARD TABLE OF tutypa-usertyp WITH DEFAULT
      KEY,
      charge_unit  TYPE rsuvm_unit,
      lt_data      TYPE STANDARD TABLE OF ty_data,
      lv_data      LIKE LINE OF lt_data.

FIELD-SYMBOLS: <slim_unit> LIKE LINE OF lt_slim_unit,
               <usertyp>   LIKE LINE OF lt_usertyp.

INSERT c_orders INTO TABLE lt_slim_unit.
INSERT c_sales_orders INTO TABLE lt_slim_unit.
INSERT c_sched_agreemnt INTO TABLE lt_slim_unit.
INSERT c_sched_agreemnt_ext INTO TABLE lt_slim_unit.
INSERT c_returns INTO TABLE lt_slim_unit.
INSERT c_delivery_uncharged INTO TABLE lt_slim_unit.
INSERT c_gutschrift INTO TABLE lt_slim_unit.
INSERT c_debit_memo INTO TABLE lt_slim_unit.
INSERT c_charged_unit INTO TABLE lt_slim_unit.
INSERT c_charge_free_unit INTO TABLE lt_slim_unit.
*INSERT c_unknown INTO TABLE lt_slim_unit.

SELECT usertyp FROM tutypa
               INTO TABLE lt_usertyp.

LOOP AT lt_slim_unit ASSIGNING <slim_unit>.
  LOOP AT lt_usertyp ASSIGNING <usertyp>.
    CLEAR lv_data.

    CALL FUNCTION 'SLIM_UNIT_EVALUATE_CHARGE'
      EXPORTING
        slim_unit        = <slim_unit>
        slim_usertype    = <usertyp>
      IMPORTING
        slim_charge_unit = charge_unit.

    lv_data-slim_unit   = <slim_unit>.
    lv_data-usertyp     = <usertyp>.
    lv_data-charge_unit = charge_unit.
    INSERT lv_data INTO TABLE lt_data.
  ENDLOOP.
ENDLOOP.


DATA: lo_alv TYPE REF TO cl_salv_table.

cl_salv_table=>factory(
  IMPORTING
    r_salv_table   = lo_alv
  CHANGING
    t_table        = lt_data ).

lo_alv->get_functions( )->set_all( ).

lo_alv->get_columns(
)->get_column( 'SLIM_UNIT' )->set_output_length( 10 ).

lo_alv->get_columns(
)->get_column( 'SLIM_UNIT' )->set_medium_text('Nutzertyp' ).

lo_alv->get_columns(
)->get_column( 'USERTYP' )->set_output_length( 10 ).

lo_alv->get_columns(
)->get_column( 'USERTYP' )->set_medium_text('Nutzertyp' ).

lo_alv->get_columns(
)->get_column( 'CHARGE_UNIT' )->set_output_length( 15 ).

lo_alv->get_columns(
)->get_column( 'CHARGE_UNIT' )->set_medium_text('umgeschl. Wert' ).

lo_alv->display( ).
