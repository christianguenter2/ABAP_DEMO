*&---------------------------------------------------------------------*
*& Report z_test_serialize
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_serialize.

CLASS lcl_serial_object DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_serializable_object.

    DATA: m_data TYPE string READ-ONLY.

    METHODS:
      constructor
        IMPORTING
          i_data TYPE string.

ENDCLASS.

CLASS lcl_serial_object IMPLEMENTATION.

  METHOD constructor.

    me->m_data = i_data.

  ENDMETHOD.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    DATA: instance TYPE REF TO lcl_serial_object.

    DATA(obj) = NEW lcl_serial_object( |This is a test| ).

    CALL TRANSFORMATION id SOURCE instance = obj
                           RESULT XML DATA(xml).

    CALL TRANSFORMATION id SOURCE XML xml
                           RESULT instance = instance.

    cl_demo_output=>display( instance->m_data ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->start( ).
