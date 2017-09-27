REPORT z_test_http_location.

DATA: host     TYPE string,
      port     TYPE string,
      protocol TYPE string.

cl_http_server=>if_http_server~get_location(
  EXPORTING
    protocol            = 'https'
*    application         =
*    for_domain          =
*    server              =     " HTTP Framework (iHTTP) HTTP Server
*    use_ticket_protocol = ABAP_TRUE
  IMPORTING
    host                = host
    port                = port
    out_protocol        = protocol
).

cl_demo_output=>new( )->write_data( host
                     )->write_data( port
                     )->write_data( protocol
                     )->display( ).
