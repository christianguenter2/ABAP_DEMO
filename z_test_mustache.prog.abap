*&---------------------------------------------------------------------*
*& Report z_test_mustache
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_mustache.

INCLUDE zmustache.
INCLUDE zmustache_ut.

CLASS test_template DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,
      start
        RAISING
          lcx_mustache_error.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_hobby,
             name TYPE string,
           END OF ty_hobby,
           tty_hobby TYPE STANDARD TABLE OF ty_hobby
                     WITH NON-UNIQUE DEFAULT KEY,

           BEGIN OF ty_programming_language,
             name TYPE string,
           END OF ty_programming_language,
           tty_programming_language TYPE STANDARD TABLE OF ty_programming_language
                                    WITH NON-UNIQUE DEFAULT KEY,

           BEGIN OF ty_person_record,
             BEGIN OF person,
               name                  TYPE string,
               surname               TYPE string,
               hobbies               TYPE tty_hobby,
               programming_languages TYPE tty_programming_language,
             END OF person,
           END OF ty_person_record,

           tty_person TYPE STANDARD TABLE OF ty_person_record
                      WITH NON-UNIQUE DEFAULT KEY.

    DATA: m_template TYPE string.

    METHODS:
      _get_persons
        RETURNING
          VALUE(r_persons) TYPE test_template=>tty_person,

      _render_template
        IMPORTING
          it_persons      TYPE test_template=>tty_person
        RETURNING
          VALUE(r_result) TYPE string
        RAISING
          lcx_mustache_error,

      _display
        IMPORTING
          i_result TYPE string.

ENDCLASS.

CLASS test_template IMPLEMENTATION.

  METHOD start.

    DATA(persons) = _get_persons( ).

    DATA(result) = _render_template( persons ).

    _display( result ).

  ENDMETHOD.

  METHOD _get_persons.

    DATA: p1 TYPE test_template=>ty_person_record,
          p2 TYPE test_template=>ty_person_record.

    p1 = VALUE ty_person_record(
                        person-name    = `Christian`
                        person-surname = `Günter`
                        person-hobbies = VALUE #( ( name = `Fussball` )
                                                  ( name = `Programmieren` ) )
                        person-programming_languages = VALUE #( ( name = `ABAP` )
                                                                ( name = `JavaScript` ) ) ).

    p2 = VALUE ty_person_record(
                        person-name    = `Julia`
                        person-surname = `Hartung`
                        person-hobbies = VALUE #( ( name = `Musik` )
                                                  ( name = `Fasnet` ) )
                        person-programming_languages = VALUE #( ( name = `ABAP` )
                                                                ( name = `JavaScript` ) ) ).

    r_persons  = VALUE tty_person( ( p1 ) ( p2 ) ).

  ENDMETHOD.

  METHOD _render_template.

    r_result = lcl_mustache=>create( m_template )->render( it_persons ).

  ENDMETHOD.

  METHOD _display.

    cl_demo_output=>display_html( i_result ).

  ENDMETHOD.

  METHOD constructor.

    m_template = '{{#person}}'
              && '<h1><b>{{name}}</b> {{surname}}<h1>'
              && '<h3>Hobbies</h3>'
              && '{{#hobbies}}'
              && '<li>{{name}}</li>'
              && '{{/hobbies}}'
              && '<h3>Programmiersprachen</h3>'
              && '{{#programming_languages}}'
              && '<li>{{name}}</li>'
              && '{{/programming_languages}}'
              && '{{/person}}'.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  TRY.
      NEW test_template( )->start( ).

    CATCH lcx_mustache_error INTO DATA(error).  "
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
