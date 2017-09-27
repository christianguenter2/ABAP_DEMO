*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section






CLASS lcl_template DEFINITION  CREATE PROTECTED ABSTRACT .

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        i_type            TYPE string
      RETURNING
        VALUE(r_instance) TYPE REF TO lcl_template .

    METHODS:
      run
        RETURNING VALUE(r_result) TYPE string.

  PROTECTED SECTION.
    METHODS:
      _step_2 ABSTRACT.

    DATA: result TYPE string.

  PRIVATE SECTION.

    METHODS:
      _step_1,
      _step_3.

ENDCLASS.

CLASS lcl_template_one DEFINITION CREATE PUBLIC INHERITING FROM lcl_template.

  PROTECTED SECTION.
    METHODS: _step_2 REDEFINITION.

ENDCLASS.

CLASS lcl_template_two DEFINITION CREATE PUBLIC INHERITING FROM lcl_template.

  PROTECTED SECTION.
    METHODS: _step_2 REDEFINITION.

ENDCLASS.

CLASS lcl_template_three DEFINITION CREATE PUBLIC INHERITING FROM lcl_template.

  PROTECTED SECTION.
    METHODS: _step_2 REDEFINITION.

ENDCLASS.

CLASS lcl_template_four DEFINITION CREATE PUBLIC INHERITING FROM lcl_template.

  PROTECTED SECTION.
    METHODS: _step_2 REDEFINITION.

ENDCLASS.

CLASS lcl_template_five DEFINITION CREATE PUBLIC INHERITING FROM lcl_template.

  PROTECTED SECTION.
    METHODS: _step_2 REDEFINITION.

ENDCLASS.

CLASS lcl_template_six DEFINITION CREATE PUBLIC INHERITING FROM lcl_template.


  PROTECTED SECTION.
    METHODS: _step_2 REDEFINITION.

ENDCLASS.

CLASS lcl_template_seven DEFINITION CREATE PUBLIC INHERITING FROM lcl_template.

  PROTECTED SECTION.
    METHODS: _step_2 REDEFINITION.

ENDCLASS.
