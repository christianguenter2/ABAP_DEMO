class ZCL_TEST_CRUD definition
  public
  final
  create public .

public section.

  class-methods READ
    returning
      value(R_INSTANCE) type ref to ZCL_TEST_CRUD .
  class-methods CREATE
    returning
      value(R_INSTANCE) type ref to ZCL_TEST_CRUD .
  methods UPDATE .
  methods DELETE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TEST_CRUD IMPLEMENTATION.


method CREATE.
endmethod.


method DELETE.
endmethod.


METHOD read.
ENDMETHOD.


method UPDATE.
endmethod.
ENDCLASS.
