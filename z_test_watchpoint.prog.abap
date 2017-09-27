*&---------------------------------------------------------------------*
*& Report Z_TEST_WATCHPOINT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_watchpoint.

TYPES: BEGIN OF ty_data,
         s   TYPE string,
         i   TYPE i,
         tab TYPE stringtab,
       END OF ty_data,
       tty_data TYPE STANDARD TABLE OF ty_data
                     WITH NON-UNIQUE DEFAULT KEY.

DATA(table) =  VALUE tty_data( ( s = 'Test' i = 1 tab = VALUE #( ( `Test` ) ) )
                               ( s = 'Test' i = 1 )
                               ( s = 'Test' i = 2 tab = VALUE #( ( `0815` ) ) )
                               ( s = 'Test3' i = 1 )
                               ( s = 'Test' i = 1 ) ).

LOOP AT table ASSIGNING FIELD-SYMBOL(<fs>).

  IF sy-subrc = 0.

  ENDIF.

ENDLOOP.
