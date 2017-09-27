*"* use this source file for your ABAP unit test

DEFINE def_meth.
  data: name TYPE string.
  METHODS: &1,
           &2 FOR TESTING.
END-OF-DEFINITION.

DEFINE def_test_class.
*----------------------------------------------------------------------*
*       CLASS &1 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class &1 definition
                    for testing
                    inheriting from cl_aunit_assert
                    risk level harmless
                    duration short.
  private section.
    data: &2 TYPE REF TO &3.

    def_meth setup
             &4.
endclass.                    "&1 DEFINITION
END-OF-DEFINITION.

def_test_class LCL_ABAP_UNIT
               m_ref zcl_test_abap_unit
               test_1.

*----------------------------------------------------------------------*
*       CLASS lcl_abap_unit IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_unit IMPLEMENTATION.
  METHOD setup.
    create OBJECT m_ref.
  ENDMETHOD.                    "setup

  METHOD test_1.
    assert_equals(
      EXPORTING
        exp                  = 'Test'
        act                  = m_ref->test( )
    ).
  ENDMETHOD.
ENDCLASS.                    "lcl_abap_unit IMPLEMENTATION
