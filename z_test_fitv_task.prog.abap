REPORT z_test_fitv_task.

zcl_fitv_travel_exp=>get_workitems_for_user_wda(
  EXPORTING
    iv_username  = sy-uname
  RECEIVING
    rt_workitems = DATA(workitems) ).

cl_demo_output=>display( workitems ).
