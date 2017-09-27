*&---------------------------------------------------------------------*
*& Report z_test_2017_03_03
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_03_03.

PARAMETERS: p_in1  TYPE i,
            p_in2  TYPE i,
            p_add  RADIOBUTTON GROUP r1 DEFAULT 'X',
            p_sub  RADIOBUTTON GROUP r1,
            p_mult RADIOBUTTON GROUP r1,
            p_div  RADIOBUTTON GROUP r1.

CLASS lcx_error DEFINITION CREATE PUBLIC
                INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        textid   LIKE textid OPTIONAL
        previous LIKE previous OPTIONAL
        text     TYPE csequence,

      get_text REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA m_text TYPE string.

ENDCLASS.

CLASS calculator DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: ty_result TYPE p DECIMALS 2 length 3.
    CLASS-METHODS
      class_constructor.

    METHODS:
      constructor,

      start.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: result TYPE ty_result.

    METHODS _calculate
      RAISING
        lcx_error.
    METHODS _display.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).

    m_text = text.

  ENDMETHOD.

  METHOD get_text.

    result = m_text.

  ENDMETHOD.

ENDCLASS.

CLASS calculator IMPLEMENTATION.

  METHOD class_constructor.

    cl_demo_output=>write( |CLASS_Constructor wurde ausgeführt!|  ).

  ENDMETHOD.

  METHOD constructor.

    cl_demo_output=>write( |Constructor wurde ausgeführt!|  ).

  ENDMETHOD.

  METHOD start.

    TRY.
        _calculate( ).
        _display( ).

      CATCH lcx_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _calculate.

    result = COND #( WHEN p_add  = abap_true THEN p_in1 + p_in2
                     WHEN p_sub  = abap_true THEN p_in1 - p_in2
                     WHEN p_mult = abap_true THEN p_in1 * p_in2
                     WHEN p_div  = abap_true THEN p_in1 / p_in2
                     ELSE THROW lcx_error( text = |Operation nicht unterstützt!| ) ).

  ENDMETHOD.

  METHOD _display.

    cl_demo_output=>display( result ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW calculator( )->start( ).
