class ZCL_HTTP_CLIENT_TEST definition
  public
  final
  create public .

*"* public components of class ZCL_HTTP_CLIENT_TEST
*"* do not include other source files here!!!
public section.

  interfaces IF_HTTP_CLIENT .
protected section.
*"* protected components of class ZCL_HTTP_CLIENT_TEST
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_HTTP_CLIENT_TEST
*"* do not include other source files here!!!

  methods CLEAR_SERVICE .
ENDCLASS.



CLASS ZCL_HTTP_CLIENT_TEST IMPLEMENTATION.


method CLEAR_SERVICE.
  FIELD-SYMBOLS: <service> TYPE string.
  ASSIGN ('me->M_TARGET_SERVICE') TO <service>.
  IF <service> IS ASSIGNED.
    CLEAR <service>.
  ENDIF.
endmethod.
ENDCLASS.
