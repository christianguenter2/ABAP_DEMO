REPORT z_test_select_.

SELECT * FROM t100 INTO TABLE @DATA(lt_data)
                   UP TO 10 ROWS.

LOOP AT lt_data INTO DATA(data).

ENDLOOP.

BREAK-POINT 'Test'.

cl_demo_output=>display( lt_data ).
