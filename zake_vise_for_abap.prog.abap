*&---------------------------------------------------------------------*
*& Report  ZAKE_VISE_FOR_ABAP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zake_vise_for_abap.


CONSTANTS cl_svn TYPE seoclsname VALUE 'ZCL_ZAKE_SVN'.
CONSTANTS cl_tortoise_svn TYPE seoclsname VALUE 'ZCL_ZAKE_TORTOISE_SVN'.

DATA package TYPE devclass.
DATA zake    TYPE REF TO zake.

DATA objects TYPE scts_tadir.
DATA object  LIKE LINE OF objects.

DATA files TYPE string_table.
DATA file  LIKE LINE OF files.

DATA zake_build                TYPE string.
DATA zake_nuggetname           TYPE string.

DATA comment_str               TYPE string.
DATA loclpath_str              TYPE string.
DATA svnpath_str               TYPE string.
DATA username_str              TYPE string.
DATA password_str              TYPE string.
DATA class                     TYPE seoclsname.

DATA: ex TYPE REF TO zcx_saplink,
      message TYPE string.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE a.
PARAMETERS:
*  checkout TYPE flag RADIOBUTTON GROUP act,
*  update   TYPE flag RADIOBUTTON GROUP act,
  install  TYPE flag RADIOBUTTON GROUP act,
  export   TYPE flag RADIOBUTTON GROUP act,
  build    TYPE flag RADIOBUTTON GROUP act DEFAULT 'X'.
*  checkin  TYPE flag RADIOBUTTON GROUP act.
SELECTION-SCREEN END OF BLOCK a.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE b.
PARAMETERS:
  svn      TYPE flag RADIOBUTTON GROUP cl,
  tortoise TYPE flag RADIOBUTTON GROUP cl.
SELECTION-SCREEN END OF BLOCK b.

SELECTION-SCREEN BEGIN OF BLOCK c WITH FRAME TITLE c.
PARAMETERS:
  loclpath TYPE char512 DEFAULT 'C:\Users\Uwe\Documents\workspace\Vise_for_ABAP' LOWER CASE OBLIGATORY,
  zakenugg TYPE char512 DEFAULT 'C:\Users\Uwe\Documents\workspace\Vise_for_ABAP\zake_build\vise_daily.nugg' LOWER CASE OBLIGATORY,
  svnpath  TYPE char512 DEFAULT 'https://code.sdn.sap.com/svn/...' LOWER CASE OBLIGATORY,
  comment  TYPE char512 DEFAULT '' LOWER CASE,
  username TYPE char512 LOWER CASE,
  password TYPE char512 LOWER CASE,
  testrun  TYPE flag    DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK c.

INITIALIZATION.
  a = 'Action'.
  b = 'Version Controll Program'.
  c = 'Parameters'.

START-OF-SELECTION.

  svnpath_str               = svnpath.
  loclpath_str              = loclpath.
  zake_nuggetname           = zakenugg.
  comment_str               = comment.

  " SELECT * INTO TABLE objects FROM tadir WHERE devclass = 'ZABAP2XLSX'.
  " DELETE zake_objects WHERE object = 'DEVC'.

  TRY.
      IF svn = 'X'.
        class = cl_svn.
      ELSE.
        class = cl_tortoise_svn.
      ENDIF.

      CREATE OBJECT zake
        TYPE
        (class)
        EXPORTING
          i_svnpath   = svnpath_str
          i_localpath = loclpath_str.
      zake->set_testrun( testrun ).
      zake->set_package( 'Z_VISE_FOR_ABAP' ).
      " Build Object list for Export
      " Classes
      SELECT object obj_name INTO CORRESPONDING FIELDS OF TABLE objects FROM tadir WHERE devclass = 'Z_VISE_FOR_ABAP'.

*      IF checkout = 'X'.
*        zake->checkout( ).
*      ELSEIF update = 'X'.
*        zake->update( ).
*      ELSE
      IF install = 'X'.
        zake->install_slinkees_from_lm( testrun ).
        IF testrun IS INITIAL.
          zake->activate_package_objects( ).
        ENDIF.
        " zake->install_objects( zake_objects ).
      ELSEIF export = 'X'.

        zake->set_checkin_objects( objects ).
        zake->download_slinkees_to_lm = abap_true.
        zake->download_nugget_to_lm   = space.
        zake->download_zip_to_lm_flag = space.
        zake->create_slinkees( zake_nuggetname ).
      ELSEIF build = 'X'.
        " Build a complete package for download
        zake->set_checkin_objects( objects ).
        " We don't want that for the complete Package Slinkees are created
        " in the ZAKE folder
        zake->download_slinkees_to_lm = space.
        zake->download_nugget_to_lm   = abap_true.
        zake->create_slinkees( zake_nuggetname ).
*      ELSEIF checkin = 'X'.
*        zake->set_package( 'ZA2X' ).
*        zake->set_checkin_objects( objects ).
*        zake->create_slinkees( zake_nuggetname ).
*        IF testrun IS INITIAL.
*          zake->checkin( comment_str ).
*        ENDIF.
      ENDIF.
    CATCH zcx_saplink INTO ex.
      message = ex->msg.
      WRITE: / 'An Error occured: ', message.
  ENDTRY.
