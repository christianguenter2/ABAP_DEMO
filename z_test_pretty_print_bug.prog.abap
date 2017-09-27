REPORT z_test_pretty_print_bug.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_data,
             i TYPE i,
             s TYPE string,
           END OF ty_data,
           tty_data TYPE HASHED TABLE OF ty_data
                         WITH UNIQUE KEY i.

    METHODS: constructor,
             start.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD start.

  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_application IMPLEMENTATION
