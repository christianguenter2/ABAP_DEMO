REPORT z_test_debugger_step.

TYPES: BEGIN OF ty_data,
         s TYPE string,
         i TYPE i,
       END OF ty_data,
       tty_data TYPE STANDARD TABLE OF ty_data
                WITH NON-UNIQUE DEFAULT KEY.


DATA(test) = VALUE tty_data( ( i = 1 s = 'Hallo Welt' )
                             ( i = 2 s = 'Dies ist ein Test' )
                             ( i = 3 s = '12345' ) ).

DATA(test2) = VALUE tty_data( FOR wa IN test ( i = wa-i * 2
                                               s = wa-s && ' added!' ) ).

IF sy-subrc = 0.

ENDIF.
