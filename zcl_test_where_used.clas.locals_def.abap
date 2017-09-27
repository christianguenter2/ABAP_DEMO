*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

INTERFACE lif_test.

  METHODS: test.

ENDINTERFACE.

CLASS lcl_test_abstract DEFINITION ABSTRACT.

  PUBLIC SECTION.
    INTERFACES: lif_test ALL METHODS ABSTRACT.

ENDCLASS.

CLASS lcl_test_1 DEFINITION INHERITING FROM lcl_test_abstract.

  PUBLIC SECTION.
    METHODS: lif_test~test REDEFINITION.

ENDCLASS.
