CLASS zcx_2017_03_01 DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      raise_syst
        RAISING
          zcx_2017_03_01,

      raise_text
        IMPORTING
          i_text TYPE csequence
        RAISING
          zcx_2017_03_01,

      raise_previous
        IMPORTING
          io_previous_error TYPE REF TO cx_root
        RAISING
          zcx_2017_03_01.

    METHODS:
      constructor
        IMPORTING
          !textid   LIKE textid OPTIONAL
          !previous LIKE previous OPTIONAL
          !msg      TYPE symsg OPTIONAL
          !text     TYPE csequence OPTIONAL,

      get_text
        REDEFINITION .

  PRIVATE SECTION.

    DATA: m_msg  TYPE symsg,
          m_text TYPE string.

ENDCLASS.

CLASS zcx_2017_03_01 IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( textid = textid previous = previous ).

    m_msg = msg.
    m_text = text.

  ENDMETHOD.

  METHOD get_text.

    IF m_text IS NOT INITIAL.

      result = m_text.
      RETURN.

    ENDIF.

    IF m_msg IS NOT INITIAL.

      MESSAGE ID m_msg-msgid TYPE m_msg-msgty NUMBER m_msg-msgno
              WITH m_msg-msgv1 m_msg-msgv2 m_msg-msgv3 m_msg-msgv4
              INTO result.
      RETURN.

    ENDIF.

    result = super->get_text( ).

  ENDMETHOD.

  METHOD raise_previous.

    RAISE EXCEPTION TYPE zcx_2017_03_01
      EXPORTING
        previous = io_previous_error.

  ENDMETHOD.

  METHOD raise_syst.

    RAISE EXCEPTION TYPE zcx_2017_03_01
      EXPORTING
        msg = VALUE symsg( msgty = sy-msgty
                           msgid = sy-msgid
                           msgno = sy-msgno
                           msgv1 = sy-msgv1
                           msgv2 = sy-msgv2
                           msgv3 = sy-msgv3
                           msgv4 = sy-msgv4 ).

  ENDMETHOD.

  METHOD raise_text.

    RAISE EXCEPTION TYPE zcx_2017_03_01
      EXPORTING
        text = i_text.

  ENDMETHOD.

ENDCLASS.
