REPORT z_test_condition_access.

DATA: tax_rate TYPE kbetr_kond,
      lt_mwskz TYPE STANDARD TABLE OF a003-mwskz,
      lt_matkl TYPE STANDARD TABLE OF matkl.

tax_rate = '200'.

SELECT mwskz FROM a003
       INNER JOIN konp
             ON a003~knumh = konp~knumh
       INTO TABLE lt_mwskz
       WHERE kbetr			= tax_rate
       AND   a003~kschl = 'MWVS'
       AND   aland			= 'FR'.

IF sy-subrc = 0.
  SELECT matkl FROM a920
         INNER JOIN konp
               ON a920~knumh = konp~knumh
         INTO TABLE lt_matkl
         FOR ALL ENTRIES IN lt_mwskz
         WHERE mwsk1			= lt_mwskz-table_line
         AND   a920~kschl = 'NAVS'
         AND   lland			= 'FR'
         AND   datab      >= sy-datum
         AND   datbi      <= sy-datum.
ENDIF.

cl_demo_output=>write_data( lt_mwskz ).
cl_demo_output=>write_data( lt_matkl ).
cl_demo_output=>display( ).
