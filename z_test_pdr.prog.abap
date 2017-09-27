REPORT z_test_pdr.

DATA: lt_dost TYPE STANDARD TABLE OF dost
                   WITH NON-UNIQUE DEFAULT KEY,
      lo_alv TYPE REF TO cl_salv_table.


SELECT dost~mandt
       dost~dokar
       dost~doknr
       dost~dokvr
       dost~doktl
       dost~stlnr
       dost~stlal
       dost~annam
       dost~andat
       dost~aenam
       dost~aedat
         FROM dost
         INNER JOIN stpo ON dost~stlnr = stpo~stlnr
         INTO TABLE lt_dost
         WHERE dost~dokar = 'PDR'
         AND   stpo~dokar = 'PPA'.

cl_salv_table=>factory(
  IMPORTING
    r_salv_table   = lo_alv    " Basisklasse einfache ALV Tabellen
  CHANGING
    t_table        = lt_dost ).

zcl_bc_alv_utilities=>do_alv_default_config( lo_alv ).
lo_alv->display( ).
