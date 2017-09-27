REPORT z_test_greeting.

cl_demo_output=>display(
  |\n\n\n{ REDUCE string( LET t = replace( val =  REDUCE string(
  INIT l = replace( val = replace( val =  replace( val = replace(
  val = REDUCE string( LET x = cl_abap_random_int=>create( seed =
  CONV i( sy-uzeit ) min = 1 max = 1999 ) IN INIT h = repeat( val
  = ` ` occ = 2000 ) FOR k = 1 UNTIL k > 500 NEXT h = replace( val
  = h off = x->get_next( ) len = 1 with = COND string( WHEN k / 2
  = ( k - 1 ) / 2 THEN `*` ELSE `+` ) ) ) sub = `**` with = `* `
  occ = 0 ) sub = `*+` with = `* ` occ = 0 ) sub = `+*` with = `+ `
  occ = 0 ) sub = `++` with = `+ ` occ = 0 ) FOR j = 0 UNTIL j > 2
  NEXT l = replace( val = l off = 800 + j * 100 + 30 len = 40 with
  = repeat( val = ` ` occ = 40 ) ) ) off = 930 len = 40 with = |{


                      `Seasons Greetings!`


  WIDTH = 40 ALIGN = CENTER }| ) IN INIT s = `` FOR i = 0 UNTIL i >
  19 NEXT s = s && substring( val = t off = i * 100  len = 100 ) &&
  |\n| ) }| ).
