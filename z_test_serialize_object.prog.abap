*&---------------------------------------------------------------------*
*& Report z_test_serialize_object
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_serialize_object.

CLASS test_serialize_object DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_serializable_object.
    METHODS:
      constructor
        IMPORTING
          i_text TYPE string.

    DATA: _text TYPE string READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS test_application DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

ENDCLASS.

CLASS test_serialize_object IMPLEMENTATION.

  METHOD constructor.

    _text = i_text.

  ENDMETHOD.

ENDCLASS.

CLASS test_application IMPLEMENTATION.

  METHOD start.

    DATA: object2 TYPE REF TO test_serialize_object.
    DATA(object) = NEW test_serialize_object(  `Hallo Welt!` ).

    CALL TRANSFORMATION id
         SOURCE object = object
         RESULT XML DATA(xml).

    CLEAR object.
    FREE object.

    CALL TRANSFORMATION id
         SOURCE XML xml
         RESULT object = object2.

    cl_demo_output=>display( object2->_text ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_application( )->start( ).
