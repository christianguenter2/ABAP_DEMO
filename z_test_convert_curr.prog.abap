*&---------------------------------------------------------------------*
*& Report  Z_TEST_CONVERT_CURR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_convert_curr.

DATA: from_amount   TYPE bseg-dmbtr VALUE '1900000',
      from_currency TYPE bkpf-waers VALUE 'EUR',
      from_factor   TYPE tcurr-ffact VALUE '1',
      rate          TYPE tcurr-ukurs VALUE '1',
      to_currency   TYPE bkpf-waers VALUE 'EUR',
      to_factor     TYPE tcurr-tfact VALUE '1000',
      to_amount     TYPE bseg-wrbtr.

*CALL FUNCTION 'CONVERT_CURRENCY_BY_RATE'
*  EXPORTING
*    from_amount   = from_amount
*    from_currency = from_currency
*    from_factor   = from_factor
*    rate          = rate
*    to_currency   = to_currency
*    to_factor     = to_factor
*  IMPORTING
*    to_amount     = to_amount
*  EXCEPTIONS
*    no_rate_found = 1
*    OTHERS        = 2.

CALL FUNCTION 'UNIT_CONVERSION_WITH_FACTOR'
  EXPORTING
    add_const        = 0
    denominator      = '1000'    " Nenner
    input            = '1900000'    " Eingabewert
    numerator        = '1'    " Zähler
  IMPORTING
    output           = to_amount
  EXCEPTIONS
    division_by_zero = 1
    overflow         = 2
    type_invalid     = 3
    OTHERS           = 4.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

cl_demo_output=>display_data( to_amount ).
