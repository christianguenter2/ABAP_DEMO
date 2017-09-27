*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations



CLASS lcl_template IMPLEMENTATION.

  METHOD create.

    DATA(classname) = to_upper( |lcl_template_{ i_type }| ).

    CREATE OBJECT r_instance
      TYPE (classname).

  ENDMETHOD.

  METHOD run.

    _step_1( ).
    _step_2( ).
    _step_3( ).

    r_result = result.

  ENDMETHOD.

  METHOD _step_1.

  ENDMETHOD.

  METHOD _step_3.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_template_one IMPLEMENTATION.

  METHOD _step_2.

    result = `one`.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_template_two IMPLEMENTATION.

  METHOD _step_2.

    result = `two`.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_template_three IMPLEMENTATION.

  METHOD _step_2.

    result = `three`.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_template_four IMPLEMENTATION.

  METHOD _step_2.

    result = `four`.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_template_five IMPLEMENTATION.

  METHOD _step_2.

    result = `five`.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_template_six IMPLEMENTATION.

  METHOD _step_2.

    result = `six`.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_template_seven IMPLEMENTATION.

  METHOD _step_2.

    result = `seven`.

  ENDMETHOD.

ENDCLASS.
