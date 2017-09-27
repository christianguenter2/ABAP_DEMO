REPORT z_test_job_list.

CLASS lcl_test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_job,
             job    TYPE string,
             status TYPE char01,
           END OF ty_job,
           tty_job TYPE HASHED TABLE OF ty_job
                   WITH UNIQUE KEY job.

    METHODS: job_get_list RETURNING VALUE(r_jobs) TYPE tbtcjob_tt.
    METHODS: job_get_list2 RETURNING VALUE(r_jobs) TYPE tty_job.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_test IMPLEMENTATION.

  METHOD job_get_list.

  ENDMETHOD.

  METHOD job_get_list2.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(test) = NEW lcl_test( ).

  DATA(jobs) = test->job_get_list2(  ).

  IF lines( FILTER lcl_test=>tty_job( jobs WHERE JOB = `A` ) > 0.

  ENDIF.
*  DATA(filtered_jobs) = VALUE tbtcjob_tt( FOR job IN test->job_get_list( )
*                                          WHERE ( STATUS = 'A' )
*                                                ( job ) ).

*  IF lines( VALUE tbtcjob_tt( FOR job IN test->job_get_list( ) WHERE ( status = 'A' ) ( job ) ) ) > 0.
*
*  ENDIF.
