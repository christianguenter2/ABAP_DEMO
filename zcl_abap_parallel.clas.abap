CLASS zcl_abap_parallel DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      t_in_tab TYPE STANDARD TABLE OF xstring WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      BEGIN OF t_out,
             result  TYPE xstring,
             index   TYPE int4,
             time    TYPE int4,
             message TYPE string,
      END   OF t_out .
    TYPES:
      t_out_tab TYPE STANDARD TABLE OF t_out WITH NON-UNIQUE DEFAULT KEY .

    METHODS constructor
      IMPORTING
        !p_num_tasks           TYPE i DEFAULT 10
        !p_timeout             TYPE i DEFAULT 200
        !p_percentage          TYPE i DEFAULT 50
        value(p_num_processes) TYPE i DEFAULT 20 .
    METHODS run
      IMPORTING
        !p_in_tab  TYPE t_in_tab
      EXPORTING
        !p_out_tab TYPE t_out_tab .
    METHODS do
          ABSTRACT
      IMPORTING
        !p_in  TYPE xstring
      EXPORTING
        !p_out TYPE xstring .
    CLASS-METHODS end_task
      IMPORTING
        !p_task TYPE clike .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_task,
             name  TYPE sychar32,
             start TYPE syuzeit,
      END   OF t_task .

    CLASS-DATA:
                tasks         TYPE HASHED TABLE OF t_task WITH UNIQUE KEY name .
    CLASS-DATA  out_tab       TYPE t_out_tab .
    DATA        time_out      TYPE i .
    DATA        num_processes TYPE i .

    CLASS-METHODS get_number_of_processes
      IMPORTING
        !p_percentage   TYPE i
        !p_num_tasks    TYPE i
      RETURNING
        value(p_result) TYPE i .
    CLASS-METHODS get_number_of_free_processes
      RETURNING
        value(p_result) TYPE i .
ENDCLASS.



CLASS zcl_abap_parallel IMPLEMENTATION.


  METHOD constructor.
    time_out  = p_timeout.
    IF p_num_tasks IS SUPPLIED.
      p_num_processes = p_num_tasks.
    ENDIF.
    num_processes = get_number_of_processes( p_num_tasks = p_num_processes p_percentage = p_percentage ).

  ENDMETHOD.


  METHOD end_task.
    DATA:
          l_out       LIKE LINE OF out_tab,
          l_task_name TYPE sychar32,
          l_message   TYPE sychar80 ##needed.
    FIELD-SYMBOLS:
      <l_task>   LIKE LINE OF tasks[].

    RECEIVE RESULTS FROM FUNCTION 'RS_ABAP_PARALLEL'
      IMPORTING
        p_out                 = l_out-result
      EXCEPTIONS
        communication_failure = 1 MESSAGE l_message
        system_failure        = 2 MESSAGE l_message.

    l_out-message = l_message.
    READ TABLE tasks[] WITH TABLE KEY name = p_task ASSIGNING <l_task> .
    GET TIME.
    l_out-time  = sy-uzeit - <l_task>-start.
    l_out-index = p_task.
    l_task_name = p_task.
    DELETE tasks[] WHERE name = l_task_name.
    APPEND l_out TO out_tab.

  ENDMETHOD.


  METHOD get_number_of_free_processes.
    CONSTANTS:
      c_opcode_wp_get_info TYPE syhex01 VALUE 25.

    CALL 'ThWpInfo' ID 'OPCODE'     FIELD c_opcode_wp_get_info
                    ID 'FREE_DIAWP' FIELD p_result.

  ENDMETHOD.


  METHOD get_number_of_processes.
    CONSTANTS:
      c_opcode_wp_get_info TYPE syhex01 VALUE 25.
    DATA:
          l_max_procs        TYPE i,
          l_num_dia_wps      TYPE i,
          l_num_free_dia_wps TYPE i.

    CALL 'ThWpInfo' ID 'OPCODE'     FIELD c_opcode_wp_get_info
                    ID 'DIAWP'      FIELD l_num_dia_wps
                    ID 'FREE_DIAWP' FIELD l_num_free_dia_wps.

    l_max_procs = l_num_dia_wps.

    p_result = p_percentage * l_max_procs / 100.

    IF p_result < 1.
      p_result = 1.
    ENDIF.

    IF p_result > p_num_tasks.
      p_result = p_num_tasks.
    ENDIF.

  ENDMETHOD.


  METHOD run.

    DATA:
          l_descr_ref_class TYPE REF TO cl_abap_classdescr,
          l_class           TYPE abap_abstypename.

    FIELD-SYMBOLS: <l_in>   LIKE LINE OF p_in_tab,
                   <l_task> LIKE LINE OF tasks.


    l_descr_ref_class ?= cl_abap_typedescr=>describe_by_object_ref( me ).
    l_class = l_descr_ref_class->absolute_name.
    CLEAR tasks.

    LOOP AT p_in_tab ASSIGNING <l_in>.

      DATA:
            l_task    LIKE LINE OF tasks,
            l_message TYPE sychar80 ##needed.

      l_task-start = sy-uzeit.
      l_task-name  = sy-tabix.

      CALL FUNCTION 'ZRS_ABAP_PARALLEL'
        STARTING NEW TASK l_task-name
        "DESTINATION IN GROUP DEFAULT
        CALLING end_task ON END OF TASK
        EXPORTING
          p_class               = l_class
          p_in                  = <l_in>
        EXCEPTIONS
          communication_failure = 1 MESSAGE l_message
          system_failure        = 1 MESSAGE l_message.

      IF sy-subrc = 0.
        INSERT l_task INTO TABLE tasks.
      ENDIF.

      IF lines( tasks ) >= num_processes OR get_number_of_free_processes( ) < 2.
        WAIT UNTIL NOT ( lines( tasks ) >= num_processes OR get_number_of_free_processes( ) < 2 ) UP TO time_out SECONDS.
      ENDIF.

    ENDLOOP.

    WAIT UNTIL lines( tasks ) = 0 UP TO time_out SECONDS.

    LOOP AT tasks ASSIGNING <l_task>.
      DATA l_out LIKE LINE OF p_out_tab.
      GET TIME.
      l_out-time    = sy-uzeit - <l_task>-start.
      l_out-message = 'Timeout'(001).
      APPEND l_out TO out_tab.
    ENDLOOP.

    SORT out_tab BY index.
    p_out_tab = out_tab.
    CLEAR out_tab.

  ENDMETHOD.
ENDCLASS.
