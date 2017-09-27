REPORT z_test_sort2.

DATA: lt_atta TYPE gos_t_atta,
      lv_atta LIKE LINE OF lt_atta.

lv_atta-cr_date = '20160811'.
lv_atta-cr_time = '145909'.
INSERT lv_atta INTO TABLE lt_atta.

lv_atta-cr_date = '20160815'.
lv_atta-cr_time = '132819'.
INSERT lv_atta INTO TABLE lt_atta.

lv_atta-cr_date = '20160816'.
lv_atta-cr_time =	'132310'.
INSERT lv_atta INTO TABLE lt_atta.

lv_atta-cr_date = '20160816'.
lv_atta-cr_time =	'132310'.
INSERT lv_atta INTO TABLE lt_atta.

SORT lt_atta ASCENDING BY cr_date cr_time.
SORT lt_atta DESCENDING BY cr_date cr_time.

cl_demo_output=>display_data( lt_atta ).
