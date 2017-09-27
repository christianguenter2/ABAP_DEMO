CLASS zcl_2017_03_01_model DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      get_data
        IMPORTING
          !io_selections TYPE REF TO zcl_2017_03_01_selections
        EXPORTING
          !et_data       TYPE ANY TABLE
        RAISING
          zcx_2017_03_01 .

ENDCLASS.



CLASS zcl_2017_03_01_model IMPLEMENTATION.

  METHOD get_data.

    DATA: ltr_sprsl TYPE RANGE OF t100-sprsl,
          ltr_arbgb TYPE RANGE OF t100-arbgb,
          ltr_msgnr TYPE RANGE OF t100-msgnr,
          ltr_text  TYPE RANGE OF t100-text,
          rows      TYPE i.

    io_selections->get(
      EXPORTING i_name           = 'S_SPRSL'
      IMPORTING et_select_option = ltr_sprsl ).

    io_selections->get(
      EXPORTING i_name           = 'S_ARBGB'
      IMPORTING et_select_option = ltr_arbgb ).

    io_selections->get(
      EXPORTING i_name           = 'S_MSGNR'
      IMPORTING et_select_option = ltr_msgnr ).

    io_selections->get(
      EXPORTING i_name           = 'S_TEXT'
      IMPORTING et_select_option = ltr_text ).

    io_selections->get(
      EXPORTING i_name      = 'P_ROWS'
      IMPORTING e_parameter = rows ).

    SELECT FROM t100
           FIELDS *
           WHERE sprsl IN @ltr_sprsl
           AND   arbgb IN @ltr_arbgb
           AND   msgnr IN @ltr_msgnr
           AND   text  IN @ltr_text
           INTO TABLE @et_data
           UP TO @rows ROWS.

  ENDMETHOD.

ENDCLASS.
