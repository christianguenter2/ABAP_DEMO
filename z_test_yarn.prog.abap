*&---------------------------------------------------------------------*
*& Report z_test_yarn
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_yarn.

* Local Type Declaration
TYPES: BEGIN OF lty_werks_name,
         plant TYPE werks_d,
         name  TYPE name1,
       END OF lty_werks_name.

* Local Data Declaration
DATA: lt_plant      TYPE STANDARD TABLE OF t001w,
      ls_plant      TYPE t001w,
      lv_regio      TYPE land1 VALUE 'TX',
      lt_plant_name TYPE STANDARD TABLE OF lty_werks_name,
      ls_plant_name TYPE lty_werks_name,
      lv_count      TYPE i.

* Select Plant Info
SELECT * FROM t001w
INTO TABLE lt_plant.
* Sy-subrc check not required

* Loop through and save Plant and Name only
LOOP AT lt_plant INTO ls_plant WHERE regio EQ lv_regio.
  ls_plant_name-plant = ls_plant-werks.
  ls_plant_name-name = ls_plant-name1.
  APPEND ls_plant_name TO lt_plant_name.
ENDLOOP.

* Get the Number of Plants in the Region
DESCRIBE TABLE lt_plant_name LINES lv_count.

* Write a plain output
WRITE: 'Region' , lv_regio, 'has', lv_count, 'Plant(s)'.
