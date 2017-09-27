*&---------------------------------------------------------------------*
*& Report z_test_insert
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_insert.

DATA:
  itab TYPE STANDARD TABLE OF string
            WITH NON-UNIQUE EMPTY KEY
            WITH NON-UNIQUE SORTED KEY secondary_key
                 COMPONENTS table_line.

INSERT `Test` INTO TABLE itab.
cl_demo_output=>write( |{ sy-subrc }| ).

INSERT `Test` INTO TABLE itab.
cl_demo_output=>write( |{ sy-subrc }| ).

cl_demo_output=>display( itab ).
