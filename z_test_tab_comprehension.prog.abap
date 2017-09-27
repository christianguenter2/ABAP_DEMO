REPORT z_test_tab_comprehension.

TYPES t_int TYPE TABLE OF i WITH NON-UNIQUE DEFAULT KEY.

DATA(lt_data) = VALUE t_int( ( 1 ) ( 2 ) ( 3 ) ).

DATA(lt_from2) = VALUE t_int(
      FOR wa IN lt_data FROM 2
      ( wa ) ).

DATA(lt_tmp) = VALUE t_int(
      FOR wa IN lt_data FROM 2
      WHERE ( table_line >= 1 )
      ( wa ) ).

DATA(lt_great) = VALUE t_int(
      FOR wa IN lt_data FROM 2
      WHERE ( table_line >= lt_data[ 1 ] )
      ( wa ) ).

LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<data>)
                FROM 2
                WHERE table_line >= lt_data[ 1 ].
  WRITE: <data>.
ENDLOOP.

WRITE: /.

PERFORM print USING `lt_data ` lt_data.
PERFORM print USING `lt_from2` lt_from2.
PERFORM print USING `lt_tmp  ` lt_tmp.
PERFORM print USING `lt_great` lt_great.

FORM print USING name TYPE string list TYPE t_int.
  WRITE: name.
  LOOP AT list ASSIGNING FIELD-SYMBOL(<fs>).
    WRITE: <fs>.
  ENDLOOP.
  WRITE /.
ENDFORM.
