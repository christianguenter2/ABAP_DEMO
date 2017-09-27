CLASS zcl_test_string DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        i_string TYPE string,

      split
        RETURNING VALUE(r_result) TYPE stringtab.

  PRIVATE SECTION.
    DATA:
      _string TYPE string.

ENDCLASS.



CLASS ZCL_TEST_STRING IMPLEMENTATION.


  METHOD constructor.

    me->_string = i_string.

  ENDMETHOD.


  METHOD split.

    DO strlen( _string ) TIMES.

      DATA(save_index) = sy-index - 1.

      INSERT substring( val = _string
                        off = save_index
                        len = 1 )
             INTO TABLE r_result.

    ENDDO.

  ENDMETHOD.
ENDCLASS.
