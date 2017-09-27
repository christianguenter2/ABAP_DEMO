*&---------------------------------------------------------------------*
*& Report z_test_abap_in_eclipse
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_abap_in_eclipse.

DATA: y LIKE sy-langu.

DATA(table) = VALUE stringtab( ( `Test` ) ( `Test123` ) ).

DATA(x) = 123.

LOOP AT table ASSIGNING FIELD-SYMBOL(<line>).
ENDLOOP.

cl_demo_output=>display( table ).
