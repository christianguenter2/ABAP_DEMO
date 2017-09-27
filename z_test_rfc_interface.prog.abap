REPORT z_test_rfc_interface.

*----------------------------------------------------------------------*
*       CLASS lcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start.
ENDCLASS.                        "lcl_test DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
  METHOD start.
    DATA: airlineid       TYPE bapisflkey-airlineid,
          connectionid    TYPE bapisflkey-connectid,
          flightdate      TYPE bapisflkey-flightdate,
          flight_data     TYPE bapisfldat,
          additional_info TYPE bapisfladd,
          availibility    TYPE bapisflava.

    CALL FUNCTION 'BAPI_FLIGHT_GETDETAIL'
      EXPORTING
        airlineid    = airlineid
        connectionid = connectionid
        flightdate   = flightdate.

  ENDMETHOD.                       "start
ENDCLASS.                        "lcl_test IMPLEMENTATION
