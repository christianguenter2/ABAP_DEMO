*&---------------------------------------------------------------------*
*& Report z_test_751
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_751.

CLASS lcx_error DEFINITION CREATE PUBLIC
                INHERITING FROM cx_static_check.

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    CLASS-METHODS:
      raise_syst_error
        RAISING lcx_error.

ENDCLASS.

CLASS nwabap_751 DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start
        RAISING
          lcx_error.

  PRIVATE SECTION.
    TYPES: BEGIN OF ENUM tsize STRUCTURE size,
             s,
             m,
             l,
             xl,
           END OF ENUM tsize STRUCTURE size..

    METHODS:
      _lines.


ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD raise_syst_error.

    RAISE EXCEPTION TYPE lcx_error
      MESSAGE ID sy-msgid
      TYPE sy-msgty
      NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDMETHOD.

ENDCLASS.

CLASS nwabap_751 IMPLEMENTATION.

  METHOD start.

    _lines( ).

    DATA: t_shirt TYPE tsize.

    t_shirt = size-s.

    " Enter a valid value
    MESSAGE e002(00) INTO DATA(dummy).
    lcx_error=>raise_syst_error( ).

  ENDMETHOD.


  METHOD _lines.

    TYPES: BEGIN OF ENUM tsign STRUCTURE sign,
             plus,
             minus,
           END OF ENUM tsign STRUCTURE sign,
           BEGIN OF ty_text,
             text TYPE string,
             sign TYPE tsign,
           END OF ty_text,
           tty_text TYPE HASHED TABLE OF ty_text
                    WITH UNIQUE KEY text
                    WITH NON-UNIQUE SORTED KEY sign
                         COMPONENTS sign.

    DATA(texts) = VALUE tty_text( ( text = `Test7` sign = sign-plus  )
                                  ( text = `Test1` sign = sign-plus  )
                                  ( text = `Test4` sign = sign-minus )
                                  ( text = `Test3` sign = sign-minus )
                                  ( text = `Test2` sign = sign-plus  )
                                  ( text = `Test6` sign = sign-minus ) ).

    DATA(texts2) = FILTER #( texts USING KEY sign
                                   WHERE sign = sign-minus ).

    DATA(lines) = lines( FILTER #( texts USING KEY sign
                                         WHERE sign = sign-minus ) ).

    cl_demo_output=>write( texts2 ).
    cl_demo_output=>display( lines ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW nwabap_751( )->start( ).

    CATCH lcx_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
