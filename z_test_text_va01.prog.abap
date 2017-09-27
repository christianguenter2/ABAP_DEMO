*&---------------------------------------------------------------------*
*& Report  Z_TEST_TEXT_VA01
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_text_va01.

DATA:
      catalog TYPE STANDARD TABLE OF tcatalog,
      mem_id  TYPE char20,
   BEGIN OF text,
        header TYPE thead,
        lines  TYPE STANDARD TABLE OF tline,
   END OF text.

FIELD-SYMBOLS: <catalog> TYPE tcatalog.

* build text header and catalog entry
APPEND INITIAL LINE TO catalog ASSIGNING <catalog>.
<catalog>-tdobject = text-header-tdobject = 'VBBK'.
<catalog>-tdid     = text-header-tdid = '9035'.
<catalog>-tdname   = text-header-tdname = 'XXXXXXXXXX'.
<catalog>-tdspras  = text-header-tdspras = sy-langu.
<catalog>-id       = '000001'.

* export catalog
mem_id = 'SAPLSTXD' && <catalog>-id.
EXPORT catalog = catalog TO MEMORY ID mem_id.

FIELD-SYMBOLS: <textline> LIKE LINE OF text-lines.

* build text body
APPEND INITIAL LINE TO text-lines ASSIGNING <textline>.
<textline>-tdformat = '*'.
<textline>-tdline   = 'this is a test'.

* write text to text memory
CALL FUNCTION 'SAVE_TEXT'
  EXPORTING
    header    = text-header
    local_cat = 'X'
  TABLES
    lines     = text-lines.

CALL FUNCTION 'COMMIT_TEXT'
  EXPORTING
    object    = text-header-tdobject
    name      = text-header-tdname
    id        = text-header-tdid
    language  = text-header-tdspras
    local_cat = 'X'.

* open VA01 and hope, that there is a long text existing
CALL TRANSACTION 'VA01'.
