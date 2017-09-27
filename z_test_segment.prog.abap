*&---------------------------------------------------------------------*
*& Report z_test_segment
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_segment.

DATA(result) = segment( val 	= 'DR-Exec-Directors'
                        index = 3
                        sep 	= '-' ).

DATA(result2) = match( val	 = 'DR-Exec-Directors'
                       regex = `([^-]*$)` ).

ASSERT result2 = result.

cl_demo_output=>display( result ).
