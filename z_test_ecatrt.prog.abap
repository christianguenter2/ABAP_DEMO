report Z_TEST_ECATRT
       no standard page heading line-size 255.

include bdcrecx1.

parameters: dataset(132) lower case.
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
data: begin of record,
* data element:
        RB_ECATT_SCRIPT_001(001),
* data element: ETOBJ_NAME
        NAME_002(030),
* data element: ETOBJ_VER
        VERSION_003(008),
      end of record.

*** End generated data section ***

start-of-selection.

perform open_dataset using dataset.
perform open_group.

do.

read dataset dataset into record.
if sy-subrc <> 0. exit. endif.

perform bdc_dynpro      using 'SAPLECATT_MAIN' '0100'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ECOB_SHOW'.
perform bdc_field       using 'RB_ECATT_SCRIPT'
                              record-RB_ECATT_SCRIPT_001.
perform bdc_field       using 'ECATT_VER-NAME'
                              record-NAME_002.
perform bdc_field       using 'ECATT_VER-VERSION'
                              record-VERSION_003.
perform bdc_dynpro      using 'SAPLECATT_MAIN' '0202'.
perform bdc_field       using 'BDC_OKCODE'
                              '/EWB_BACK'.
perform bdc_dynpro      using 'SAPLECATT_MAIN' '0100'.
perform bdc_field       using 'BDC_OKCODE'
                              '/ETOC_BACK_E'.
perform bdc_field       using 'BDC_CURSOR'
                              'ECATT_VER-NAME'.
perform bdc_transaction using 'SECATT'.

enddo.

perform close_group.
perform close_dataset using dataset.
