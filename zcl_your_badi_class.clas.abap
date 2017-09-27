class ZCL_YOUR_BADI_CLASS definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces ZIF_TEST_BADI .

  class-data SOME_GLOBAL_VARIABLE type ref to DATA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_YOUR_BADI_CLASS IMPLEMENTATION.


METHOD zif_test_badi~your_method.
  ASSIGN some_global_variable->* TO FIELD-SYMBOL(<c>).
  CLEAR <c>.
ENDMETHOD.
ENDCLASS.
