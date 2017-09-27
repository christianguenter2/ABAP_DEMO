*&---------------------------------------------------------------------*
*& Report z_test_iban_formatting
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_iban_formatting.

DATA(iv_iban) = |DE89370400440532013000|.

*DATA(formatted_iban) = replace( val   = iv_iban
*                                regex = `(\D{2}\d{2})(\d{4})(\d{4})(\d{4})(\d{4})(\d{2,})`
*                                with  = `$1 $2 $3 $4 $5 $6` ).

*DATA(formatted_iban) = replace( val   = iv_iban
*                                regex = `(\D{2}\d{2})(\d{2,4})+`
*                                with  = `$0 $1 $2 ` ).

DATA(formatted_iban2)
     = REDUCE string(
         LET iban        = condense( iv_iban )
             iban_len    = strlen( iban )
             segment_len = 4 IN
         INIT fiban = ``
         FOR n = 0 THEN n + segment_len WHILE n < iban_len
         NEXT fiban =
              fiban
              && |{ substring(
                    val = iban
                    off = n
                    len = nmin( val1 = segment_len
                                val2 = iban_len - n ) )
                        } | ).

*                                          COND #( WHEN iban_len - n >=  segment_len
*                                                  THEN segment_len
*                                                  ELSE iban_len - n ) ) } | ).

*DATA(formatted_iban)
*      = REDUCE string(
*          LET iban = CONV char128( condense( iv_iban ) ) IN
*          INIT fiban = ``
*          FOR n = 0 THEN n + 4 WHILE n < strlen( iban )
*          NEXT fiban = fiban && |{ iban+n(4) } |
*        ).
*clike
cl_demo_output=>write( formatted_iban2 ).
cl_demo_output=>display( ).

*  DATA: iv_iban TYPE char128 VALUE 'DE89370400440532013000' . "
*  DATA: rv_formatted_iban TYPE string .
*
*  rv_formatted_iban
*      = REDUCE #(
*          LET iban = iv_iban IN
*          INIT fiban = `` " Formatierte IBAN
*          FOR n = 0 THEN n + 4 WHILE n < strlen( iban )
*          NEXT fiban = fiban && |{ iban+n(4) } |
*        ).
*
*  DATA(ob_demo_output) = cl_demo_output=>new( ).
*
*  ob_demo_output->write( rv_formatted_iban ) .
*
*  ob_demo_output->display( ).
