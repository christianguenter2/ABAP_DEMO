*&---------------------------------------------------------------------*
*& Report  ZSTO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zsto.

TABLES: stpo, draw.

WRITE: / 'draw-res4'.
SELECT DISTINCT res4 FROM draw INTO draw-res4 ORDER BY res4.
  WRITE: / draw-res4.
ENDSELECT.

WRITE: / 'stpo-sortf'.

SELECT DISTINCT sortf FROM stpo INTO stpo-sortf WHERE stlty  = 'D'
ORDER BY sortf.
  WRITE: / stpo-sortf.
ENDSELECT.
