class ZCL_SM_ABSTRACT_EVENT definition
  public
  create public .

public section.

  data MD_NAME type STRING read-only .
  data MD_CODE type STRING read-only .

  methods CONSTRUCTOR
    importing
      !ID_NAME type STRING
      !ID_CODE type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SM_ABSTRACT_EVENT IMPLEMENTATION.


method CONSTRUCTOR.

  md_name = id_name.
  md_code = id_code.

endmethod.
ENDCLASS.
