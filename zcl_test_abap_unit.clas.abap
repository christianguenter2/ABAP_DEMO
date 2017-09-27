class ZCL_TEST_ABAP_UNIT definition
  public
  final
  create public .

public section.

  class-methods TEST
    returning
      value(R_TEXT) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TEST_ABAP_UNIT IMPLEMENTATION.


method TEST.
  r_text = 'Test'.
endmethod.
ENDCLASS.
