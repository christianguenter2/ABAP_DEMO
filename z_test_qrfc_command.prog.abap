*&---------------------------------------------------------------------*
*& Report  Z_TEST_QRFC_COMMAND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_qrfc_command.

*----------------------------------------------------------------------*
*       INTERFACE lif_command
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_command.
  METHODS: execute RETURNING value(rt_return) TYPE bapiret2_tab.
ENDINTERFACE.                    "lif_command

*----------------------------------------------------------------------*
*       CLASS lcl_test_command DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test_command DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: new RETURNING value(r_instance) TYPE REF TO lcl_test_command.
    METHODS: start.
ENDCLASS.                    "lcl_test_command DEFINITION

*----------------------------------------------------------------------*
*       CLASS qrfc DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS qrfc DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: new IMPORTING i_command TYPE REF TO lif_command
                       RETURNING value(r_instance) TYPE REF TO qrfc.
    METHODS: constructor IMPORTING i_command TYPE REF TO lif_command,
             process RETURNING value(rt_return) TYPE bapiret2_tab.

  PRIVATE SECTION.
    DATA: command TYPE REF TO lif_command.
ENDCLASS.                    "qrfc DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_315_command DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_315_command DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_command.
    CLASS-METHODS: new RETURNING value(r_instance) TYPE REF TO lif_command.
ENDCLASS.                    "lcl_315_command DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_another_command DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_another_command DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_command.
    CLASS-METHODS: new RETURNING value(r_instance) TYPE REF TO lif_command.
ENDCLASS.                    "lcl_another_command DEFINITION

*----------------------------------------------------------------------*
*       CLASS command_with_params DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS command_with_params DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_command.
    METHODS: constructor IMPORTING i_param TYPE i.

  PRIVATE SECTION.
    DATA: param TYPE i.
ENDCLASS.                    "command_with_params DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test_command IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test_command IMPLEMENTATION.
  METHOD new.
    CREATE OBJECT r_instance.
  ENDMETHOD.                    "new

  METHOD start.
    DATA: qrfc                TYPE REF TO qrfc,
          another_qrfc        TYPE REF TO qrfc,
          qrfc_with_params    TYPE REF TO qrfc,
          command_with_params TYPE REF TO lif_command,
          lt_return           TYPE bapiret2_tab.

    qrfc         = qrfc=>new( lcl_315_command=>new( ) ).
    another_qrfc = qrfc=>new( lcl_another_command=>new( ) ).

    CREATE OBJECT command_with_params TYPE command_with_params
      EXPORTING
        i_param = 0815.
    qrfc_with_params = qrfc=>new( command_with_params ).

    lt_return = qrfc->process( ).
    cl_demo_output=>write_data( lt_return ).

    lt_return = another_qrfc->process( ).
    cl_demo_output=>write_data( lt_return ).

    lt_return = qrfc_with_params->process( ).
    cl_demo_output=>write_data( lt_return ).

    cl_demo_output=>display( ).
  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_test_command IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS qrfc IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS qrfc IMPLEMENTATION.
  METHOD new.
    CREATE OBJECT r_instance
      EXPORTING
        i_command = i_command.
  ENDMETHOD.                    "new

  METHOD constructor.
    command = i_command.
  ENDMETHOD.                    "constructor

  METHOD process.
    /spe/cl_qrfc_services=>set_update_handling( ).

    rt_return = command->execute( ).

    /spe/cl_qrfc_services=>end_queue_processing(
      EXPORTING
        it_message        = rt_return[]
      EXCEPTIONS
        no_queue_detected = 0 ).
  ENDMETHOD.                    "process
ENDCLASS.                    "qrfc IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_315_command IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_315_command IMPLEMENTATION.
  METHOD new.
    CREATE OBJECT r_instance TYPE lcl_315_command.
  ENDMETHOD.                    "new

  METHOD lif_command~execute.
    DATA: return LIKE LINE OF rt_return.

    cl_demo_output=>write_data( `315 Command` ).

    return-id         = 'SD'.
    return-number     = '024'.
    return-message_v1 = 'Return 315 Command'.
    INSERT return INTO TABLE rt_return.
  ENDMETHOD.                    "lif_command~execute
ENDCLASS.                    "lcl_315_command IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_another_command IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_another_command IMPLEMENTATION.
  METHOD new.
    CREATE OBJECT r_instance TYPE lcl_another_command.
  ENDMETHOD.                    "new

  METHOD lif_command~execute.
    DATA: return LIKE LINE OF rt_return.

    cl_demo_output=>write_data( `Another Command` ).

    return-id         = 'SD'.
    return-number     = '024'.
    return-message_v1 = 'Return another Command'.
    INSERT return INTO TABLE rt_return.
  ENDMETHOD.                    "lif_command~execute
ENDCLASS.                    "lcl_another_command IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS command_with_params IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS command_with_params  IMPLEMENTATION.
  METHOD constructor.
    param = i_param.
  ENDMETHOD.                    "constructor

  METHOD lif_command~execute.
    DATA: return LIKE LINE OF rt_return.

    cl_demo_output=>write_data( `Command with params` ).

    return-id         = 'SD'.
    return-number     = '024'.
    return-message_v1 = 'Command with params'.
    return-message_v2 = param.
    INSERT return INTO TABLE rt_return.
  ENDMETHOD.                    "lif_command~execute
ENDCLASS.                    "command_with_params IMPLEMENTATION


START-OF-SELECTION.
  lcl_test_command=>new( )->start( ).
