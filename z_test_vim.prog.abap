REPORT z_test_vim.

CLASS lcx_test DEFINITION CREATE PUBLIC
               INHERITING FROM cx_no_check.

  PUBLIC SECTION.
    METHODS:

      get_text REDEFINITION,

      constructor
        IMPORTING
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL.

    DATA: x TYPE string.

ENDCLASS.

CLASS lcx_test IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid   = textid
                        previous = previous ).

    x = |Dies ist ein Test|.

  ENDMETHOD.

  METHOD get_text.

    result = x.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_test_application DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.
    METHODS _do_something
      IMPORTING
        i_t000_tab TYPE any.

ENDCLASS.

CLASS lcl_test_application IMPLEMENTATION.

  METHOD start.

    SELECT *
           FROM t000
           INTO TABLE @DATA(t000_tab)
           UP TO 100 ROWS.

    TRY.
        _do_something( t000_tab ).
      CATCH lcx_test INTO DATA(error).
        IF sy-subrc = 0.

        ENDIF.
    ENDTRY.

    cl_demo_output=>display( t000_tab ).

  ENDMETHOD.


  METHOD _do_something.

    RAISE EXCEPTION TYPE lcx_test.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_test_application( )->start( ).
