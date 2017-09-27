*&---------------------------------------------------------------------*
*& Report z_test_index_tabix
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_index_tabix.

CLASS test_index DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.
    METHODS:
      _tabix,
      _index.

ENDCLASS.

CLASS test_index IMPLEMENTATION.

  METHOD start.

    _index( ).
    _tabix( ).

    cl_demo_output=>display(  ).

  ENDMETHOD.


  METHOD _index.

    DATA(i) = 0.

    WHILE i < 5.

      cl_demo_output=>write( |{ i } -> { sy-index }| ).

      i = i + 1.

    ENDWHILE.

    DO 3 TIMES.

      cl_demo_output=>write( |{ sy-index }| ).

    ENDDO.

  ENDMETHOD.


  METHOD _tabix.

    DATA(table) = VALUE stringtab( ( `Test` ) ( `naother Test` ) ( `Hello world` ) ).

    LOOP AT table ASSIGNING FIELD-SYMBOL(<line>).

      cl_demo_output=>write( |index: { sy-index } tabix: { sy-tabix }| ).

    ENDLOOP.

    TYPES: BEGIN OF ty_data,
             i TYPE i,
             s TYPE string,
           END OF ty_data.

    DATA: standard_table TYPE STANDARD TABLE OF ty_data
                              WITH NON-UNIQUE DEFAULT KEY.

    DATA: sorted_table TYPE SORTED TABLE OF ty_data
                            WITH UNIQUE KEY i
                            WITH UNIQUE HASHED KEY secondary_key COMPONENTS s i.

    DATA: hashed_table TYPE HASHED TABLE OF ty_data
                            WITH UNIQUE KEY i
                            WITH NON-UNIQUE SORTED KEY secondary_key COMPONENTS s.

    INSERT VALUE #( i = 815 s = `Test` ) INTO TABLE standard_table.
    INSERT VALUE #( i = 12345 s = `Hallo Welt` ) INTO TABLE standard_table.
    INSERT VALUE #( i = 766 s = `Test` ) INTO TABLE standard_table.

    sorted_table = standard_table.
    hashed_table = standard_table.

    cl_demo_output=>write( |Standard table| ).

    LOOP AT standard_table ASSIGNING FIELD-SYMBOL(<data>).

      cl_demo_output=>write( |index: { sy-index } tabix: { sy-tabix }| ).

    ENDLOOP.

    cl_demo_output=>write( |Sorted table| ).

    LOOP AT sorted_table ASSIGNING <data>.

      cl_demo_output=>write( |index: { sy-index } tabix: { sy-tabix }| ).

    ENDLOOP.

    cl_demo_output=>write( |Sorted table with secondary key| ).

    LOOP AT sorted_table ASSIGNING <data> USING KEY secondary_key.

      cl_demo_output=>write( |index: { sy-index } tabix: { sy-tabix }| ).

    ENDLOOP.

    cl_demo_output=>write( |Hashed table| ).

    LOOP AT hashed_table ASSIGNING <data>.

      cl_demo_output=>write( |index: { sy-index } tabix: { sy-tabix }| ).

    ENDLOOP.

    cl_demo_output=>write( |Hashed table with secondary key| ).


    LOOP AT hashed_table ASSIGNING <data> USING KEY secondary_key
                         WHERE s = `Test`.

      DATA(save_tabix) = sy-tabix.

      cl_demo_output=>write( |i: { <data>-i } s: { <data>-s }| ).
      cl_demo_output=>write( |index: { sy-index } tabix: { save_tabix }| ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_index( )->start( ).
