REPORT z_test_db_feature.

cl_abap_dbfeatures=>use_features(
  EXPORTING
    requested_features          = VALUE #( (  cl_abap_dbfeatures=>views_with_parameters ) )
  RECEIVING
    supports_features           = DATA(supports_feature) ).

cl_demo_output=>display_data( supports_feature ).
