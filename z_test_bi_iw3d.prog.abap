report Z_TEST_BI_IW3D
       no standard page heading line-size 255.

include bdcrecx1.

parameters: p_aufnr TYPE aufnr.
***    DO NOT CHANGE - the generated data section - DO NOT CHANGE    ***
*
*   If it is nessesary to change the data section use the rules:
*   1.) Each definition of a field exists of two lines
*   2.) The first line shows exactly the comment
*       '* data element: ' followed with the data element
*       which describes the field.
*       If you don't have a data element use the
*       comment without a data element name
*   3.) The second line shows the fieldname of the
*       structure, the fieldname must consist of
*       a fieldname and optional the character '_' and
*       three numbers and the field length in brackets
*   4.) Each field must be type C.
*
*** Generated data section with specific formatting - DO NOT CHANGE  ***
start-of-selection.

perform bdc_dynpro      using 'SAPLCOIH' '0101'.
perform bdc_field       using 'BDC_CURSOR'
                              'CAUFVD-AUFNR'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'CAUFVD-AUFNR'
                              p_aufnr.
perform bdc_dynpro      using 'SAPLCOIH' '3000'.
perform bdc_field       using 'BDC_OKCODE'
                              '=IHDR'.
perform bdc_dynpro      using 'SAPLIPRT' '0110'.
perform bdc_field       using 'BDC_OKCODE'
                              '=GO'.
perform bdc_field       using 'WWORKPAPER-SELECTED(01)'
                              ' '.
perform bdc_field       using 'WWORKPAPER-SELECTED(02)'
                              'X'.
perform bdc_dynpro      using 'SAPLCOIH' '3000'.
perform bdc_field       using 'BDC_OKCODE'
                              '/EEBAC'.
perform bdc_transaction using 'IW32'.
