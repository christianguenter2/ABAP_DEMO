*&---------------------------------------------------------------------*
*& Report z_test_db_trace
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_db_trace.

PARAMETERS: sel TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X',
            ins TYPE abap_bool RADIOBUTTON GROUP r1,
            upd TYPE abap_bool RADIOBUTTON GROUP r1,
            del TYPE abap_bool RADIOBUTTON GROUP r1.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: run.

  PRIVATE SECTION.
    METHODS:
      _delete,
      _update,
      _select,
      _insert.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD run.

    CASE abap_true.
      WHEN sel.
        _select( ).
      WHEN ins.
        _insert( ).
      WHEN upd.
        _update( ).
      WHEN del.
        _delete( ).
    ENDCASE.

  ENDMETHOD.

  METHOD _select.

    SELECT * FROM ztest_db
             INTO TABLE @DATA(ztest_db_tab).

  ENDMETHOD.

  METHOD _insert.

    DATA(ls_ztest_db) = VALUE ztest_db( user_name = sy-uname
                                        date_created = sy-datum
                                        time_created = sy-uzeit ).

    INSERT ztest_db FROM ls_ztest_db.

  ENDMETHOD.

  METHOD _update.

    UPDATE ztest_db SET text = 'Test'
                    WHERE user_name = sy-uname.

  ENDMETHOD.

  METHOD _delete.

    DELETE FROM ztest_db.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->run( ).
