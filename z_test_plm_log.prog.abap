*&---------------------------------------------------------------------*
*& Report  Z_TEST_PLM_LOG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_plm_log.

PARAMETERS: dokar TYPE draw-dokar OBLIGATORY,
            doknr TYPE draw-doknr OBLIGATORY,
            dokvr TYPE draw-dokvr OBLIGATORY,
            doktl TYPE draw-doktl OBLIGATORY.

DATA: draw_key TYPE cvdidrawkey.

SELECT SINGLE
       dokar
       doknr
       dokvr
       doktl FROM draw
       INTO draw_key
       WHERE dokar = dokar
       AND   doknr = doknr
       AND   dokvr = dokvr
       AND   doktl = doktl.

zcl_ehs_material_plm_log=>create_log( draw_key ).

zcl_ehs_material_plm_log=>go_ws_log->add( id_msgty     = 'S'
                                          id_msgid     = 'ZEHSMAT'
                                          id_msgno     = '044' ).

zcl_ehs_material_plm_log=>save( ).

COMMIT WORK AND WAIT.
