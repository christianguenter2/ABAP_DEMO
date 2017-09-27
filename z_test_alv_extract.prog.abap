*&---------------------------------------------------------------------*
*& Report z_test_alv_extract
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_alv_extract.

CLASS indx_database_cache DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      read
        EXPORTING
          table             TYPE STANDARD TABLE
        RETURNING
          VALUE(returncode) TYPE syst-subrc,

      write
        IMPORTING
          table             TYPE STANDARD TABLE
        RETURNING
          VALUE(returncode) TYPE syst-subrc,

      constructor
        IMPORTING
          i_report TYPE sy-repid
          i_handle TYPE ltex-handle.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: _report TYPE sy-repid,
          _handle TYPE ltex-handle.

ENDCLASS.

CLASS indx_database_cache IMPLEMENTATION.

  METHOD constructor.

    _report = i_report.
    _handle = i_handle.

  ENDMETHOD.

  METHOD read.

    CALL FUNCTION 'ALV_CACHE_READ'
      EXPORTING
        i_report  = _report
        i_handle  = _handle
      TABLES
        et_outtab = table     " Datentabelle
      EXCEPTIONS
        no_cache  = 1
        OTHERS    = 2.

    returncode = sy-subrc.

  ENDMETHOD.

  METHOD write.

    CALL FUNCTION 'ALV_CACHE_READ'
      EXPORTING
        i_report  = cl_abap_syst=>get_current_program( )
        i_handle  = _handle
      TABLES
        et_outtab = table     " Datentabelle
      EXCEPTIONS
        no_cache  = 1
        OTHERS    = 2.

    returncode = sy-subrc.

  ENDMETHOD.

ENDCLASS.

CLASS test_alv_extract DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS test_alv_extract IMPLEMENTATION.

  METHOD start.

    CONSTANTS: handle TYPE ltex-handle VALUE `Test`.

    DATA: table TYPE STANDARD TABLE OF t100
                     WITH NON-UNIQUE DEFAULT KEY.

    DATA(cache) = NEW indx_database_cache(
        i_report = cl_abap_syst=>get_current_program( )
        i_handle = handle ).

    DATA(rcode) = cache->read( IMPORTING table = table ).

    IF rcode <> 0.

      SELECT *
             FROM t100
             INTO TABLE @table
             UP TO 100 ROWS.

      cache->write( table ).

    ENDIF.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)
          CHANGING
            t_table        = table ).

        alv->display( ).

      CATCH cx_salv_msg INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_alv_extract( )->start( ).
