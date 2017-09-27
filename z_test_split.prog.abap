REPORT z_test_split.

DATA(test) = 'Hallo Welt'.
DATA: chars TYPE STANDARD TABLE OF char01.

DO strlen( test ) TIMES.
  DATA(index) = sy-index - 1.
  DATA(char)  = test+index(1).
  INSERT char INTO TABLE chars.
ENDDO.

cl_demo_output=>display( chars ).
