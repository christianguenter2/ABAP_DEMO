*&---------------------------------------------------------------------*
*& Report  Z_TEST_SPOOL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_spool.

PARAMETERS: p_spool  TYPE tsp01-rqident    OBLIGATORY DEFAULT '703905',
            p_device TYPE tsp03-padest     OBLIGATORY DEFAULT 'HIS6',
            p_start  TYPE tsp02-pjstrtpage OBLIGATORY DEFAULT 1,
            p_end    TYPE tsp02-pjendpage  OBLIGATORY DEFAULT 1,
            p_copies TYPE tsp02-pjcopies   OBLIGATORY DEFAULT 1.

CALL FUNCTION 'RSPO_OUTPUT_SPOOL_REQUEST'
  EXPORTING
    copies                   = p_copies
    device                   = p_device
*    division                 = '*'    " Abteilung (auf Titelseite)
    endpage                  = p_end
*    prio                     = 0    " Priorität
*    receiver                 = '*'    " Empfänger (auf Titelseite)
*    reqest_title             = '*'    " Titel     (auf Titelseite)
    spool_request_id         = p_spool
    startpage                = p_start
*    telelan                  = '*'    " Landeskennzeichen für Telefaxnummer
*    telenum                  = '*'    " Telefaxnummer
*    posname                  = posname    " Telefaxnummer
*    acttime                  = acttime    " Telefaxnummer
*  TABLES
*    attributes               = attributes    " Freie Attribute
  EXCEPTIONS
    archive_dest_invalid     = 1
    archive_dest_not_found   = 2
    archive_dest_no_right    = 3
    cannot_archive           = 4
    change_archdest_no_right = 5
    change_copies_no_right   = 6
    change_dest_no_right     = 7
    change_devtype_no_right  = 8
    change_prio_no_right     = 9
    change_telenum_no_right  = 10
    change_title_no_right    = 11
    dest_invalid             = 12
    dest_not_found           = 13
    dest_no_right            = 14
    internal_problem         = 15
    invalid_fax_attribute    = 16
    invalid_parameters       = 17
    non_owner_no_right       = 18
    no_layout                = 19
    no_spool_request         = 20
    out_again_no_right       = 21
    spooler_problem          = 22
    OTHERS                   = 23.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
