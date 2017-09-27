REPORT z_test_cmd_ei_api.

CLASS test_cmd_ei_api DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      create_instance
        RETURNING
          value(r_instance) TYPE REF TO test_cmd_ei_api.

    METHODS:
      run.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS test_cmd_ei_api IMPLEMENTATION.

  METHOD create_instance.

    CREATE OBJECT r_instance.

  ENDMETHOD.

  METHOD run.

    DATA: customers            TYPE cmds_ei_main,
          customer             LIKE LINE OF customers-customers,
          ls_message_defective TYPE cvis_message,
          ls_message_correct   TYPE cvis_message.

    customer-header-object_instance-kunnr = '7090'.
*    customer-company_data-company-

    INSERT customer INTO TABLE customers-customers.

    cmd_ei_api=>maintain_bapi(
      EXPORTING
        is_master_data       = customers
      IMPORTING
        es_message_correct   = ls_message_correct
        es_message_defective = ls_message_defective ).

    cl_demo_output=>write_data( ls_message_correct ).
    cl_demo_output=>write_data( ls_message_defective ).
    cl_demo_output=>display(  ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  test_cmd_ei_api=>create_instance( )->run( ).
