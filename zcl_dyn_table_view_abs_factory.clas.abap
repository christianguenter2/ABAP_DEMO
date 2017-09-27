*----------------------------------------------------------------------*
*       CLASS ZCL_DYN_TABLE_VIEW_ABS_FACTORY DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_DYN_TABLE_VIEW_ABS_FACTORY definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_DYN_TABLE_VIEW
      abstract methods DISPLAY_TABLE .

  aliases DISPLAY_TABLE
    for ZIF_DYN_TABLE_VIEW~DISPLAY_TABLE .

  types:
    bEGIN OF ty_class,
           clsname     type seorelkey-clsname, "Objekttypname
           refclsname  type seorelkey-refclsname, "Objekttypname
           description TYPE SEODESCR,
         END   OF ty_class .
  types:
    tty_class TYPE SORTED TABLE OF ty_class
                        WITH UNIQUE KEY clsname refclsname .

  class-methods GET_ALL_SUBLASSES
    returning
      value(RT_CLASS) type TTY_CLASS .
  class-methods GET_INSTANCE
    importing
      !I_VIEW type CSEQUENCE
    returning
      value(R_INSTANCE) type ref to ZIF_DYN_TABLE_VIEW .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DYN_TABLE_VIEW_ABS_FACTORY IMPLEMENTATION.


METHOD get_all_sublasses.
  DATA: intkey   TYPE  seoclskey VALUE 'ZIF_DYN_TABLE_VIEW',
        impkeys  TYPE  seor_implementing_keys,
        inhkeys  TYPE  seor_inheritance_keys,
        clskey   TYPE  seoclskey,
        lv_class LIKE LINE OF rt_class,
        lo_class TYPE REF TO cl_oo_class.

  FIELD-SYMBOLS: <impkey> LIKE LINE OF impkeys,
                 <inhkey> LIKE LINE OF inhkeys.

  CALL FUNCTION 'SEO_INTERFACE_IMPLEM_GET_ALL'
    EXPORTING
      intkey       = intkey   " Keystruktur einer Klasse
    IMPORTING
      impkeys      = impkeys   " Keystruktur einer Klasse
    EXCEPTIONS
      not_existing = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  LOOP AT impkeys ASSIGNING <impkey>.
    clskey = <impkey>-clsname.
    CALL FUNCTION 'SEO_CLASS_GET_ALL_SUBS'
      EXPORTING
        clskey             = clskey    " Schlüssel Klasse/Interface
*       version            = '0' " Versionskennzeichen (aktiv/inaktiv)
      IMPORTING
        inhkeys            = inhkeys    " Versionskennzeichen (aktiv/inaktiv)
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT inhkeys ASSIGNING <inhkey>.
      MOVE-CORRESPONDING <inhkey> TO lv_class.
      lo_class ?= cl_oo_class=>get_instance( clsname = <inhkey>-clsname  ).
      lv_class-description = lo_class->class-descript.
      INSERT lv_class INTO TABLE rt_class.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.


  METHOD get_instance.
    CREATE OBJECT r_instance TYPE (i_view).
  ENDMETHOD.                    "get_instance
ENDCLASS.
