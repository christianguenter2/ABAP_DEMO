REPORT z_test_xsitd.

PARAMETER: p_vbeln TYPE vbeln OBLIGATORY.


DATA: xsitd TYPE xsitd.

SELECT SINGLE xsitd
       FROM vlbltd
       INTO xsitd
       WHERE tdlnr = '0001027255'
       AND   expkz = ( SELECT expkz FROM likp WHERE vbeln = p_vbeln ).

cl_demo_output=>display_data( xsitd ).
