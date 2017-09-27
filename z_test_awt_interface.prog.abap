REPORT z_test_awt_interface.

DATA: lt_awt TYPE STANDARD TABLE OF zhyb_awt_file
                  WITH NON-UNIQUE DEFAULT KEY.

SELECT *
       FROM zhyb_awt_file
       INTO TABLE lt_awt.

cl_demo_output=>display_data( lt_awt ).
