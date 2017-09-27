REPORT z_test_select.

SELECT SINGLE pernr, subty
       FROM pa0001
       INTO ( @DATA(pernr), @DATA(subty) )
       WHERE pernr = '00012667'.
