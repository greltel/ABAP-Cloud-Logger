class ZCX_CLOUD_LOGGER_MESSAGE definition
  public
  inheriting from CX_NO_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .
  interfaces IF_T100_DYN_MSG .
  interfaces IF_ABAP_BEHV_MESSAGE .

  aliases MSGTY
    for IF_T100_DYN_MSG~MSGTY .
  aliases MSGV1
    for IF_T100_DYN_MSG~MSGV1 .
  aliases MSGV2
    for IF_T100_DYN_MSG~MSGV2 .
  aliases MSGV3
    for IF_T100_DYN_MSG~MSGV3 .
  aliases MSGV4
    for IF_T100_DYN_MSG~MSGV4 .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGTY type SYMSGTY optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional .
    "! Generates a new message for behavior within RAP
    "! @parameter class    | Message class
    "! @parameter number   | Message number
    "! @parameter severity | Severity
    "! @parameter v1       | Placeholder 1
    "! @parameter v2       | Placeholder 2
    "! @parameter v3       | Placeholder 3
    "! @parameter v4       | Placeholder 4
    "! @parameter result   | Instance for message
  class-methods NEW_MESSAGE
    importing
      !CLASS type SYMSGID
      !NUMBER type SYMSGNO
      !SEVERITY type IF_ABAP_BEHV_MESSAGE=>T_SEVERITY
      !V1 type SIMPLE optional
      !V2 type SIMPLE optional
      !V3 type SIMPLE optional
      !V4 type SIMPLE optional
    returning
      value(RESULT) type ref to IF_ABAP_BEHV_MESSAGE .
    "! Generates a new message from SYMSG
    "! @parameter message | Message in Format
    "! @parameter result  | Instance for message
  class-methods NEW_MESSAGE_FROM_SYMSG
    importing
      !MESSAGE type SYMSG
    returning
      value(RESULT) type ref to IF_ABAP_BEHV_MESSAGE .
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_CLOUD_LOGGER_MESSAGE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGTY = MSGTY .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD NEW_MESSAGE.
    result = NEW zcx_cloud_logger_message(
        textid = VALUE #( msgid = class
                          msgno = number
                          attr1 = COND #( WHEN v1 IS NOT INITIAL THEN 'IF_T100_DYN_MSG~MSGV1' )
                          attr2 = COND #( WHEN v2 IS NOT INITIAL THEN 'IF_T100_DYN_MSG~MSGV2' )
                          attr3 = COND #( WHEN v3 IS NOT INITIAL THEN 'IF_T100_DYN_MSG~MSGV3' )
                          attr4 = COND #( WHEN v4 IS NOT INITIAL THEN 'IF_T100_DYN_MSG~MSGV4' ) )
        msgty  = SWITCH #( severity
                           WHEN if_abap_behv_message=>severity-error   THEN 'E'
                           WHEN if_abap_behv_message=>severity-warning THEN 'W'
                           WHEN if_abap_behv_message=>severity-success THEN 'S'
                           ELSE                                             'I' )
        msgv1  = |{ v1 }|
        msgv2  = |{ v2 }|
        msgv3  = |{ v3 }|
        msgv4  = |{ v4 }| ).

    result->m_severity = severity.
  ENDMETHOD.


  METHOD new_message_from_symsg.
    RETURN new_message( class    = message-msgid
                        number   = message-msgno
                        severity = SWITCH #( message-msgty
                                             WHEN 'A' THEN if_abap_behv_message=>severity-error
                                             WHEN 'X' THEN if_abap_behv_message=>severity-error
                                             WHEN 'E' THEN if_abap_behv_message=>severity-error
                                             WHEN 'W' THEN if_abap_behv_message=>severity-warning
                                             WHEN 'I' THEN if_abap_behv_message=>severity-information
                                             WHEN 'S' THEN if_abap_behv_message=>severity-success
                                             ELSE          if_abap_behv_message=>severity-none )
                        v1       = message-msgv1
                        v2       = message-msgv2
                        v3       = message-msgv3
                        v4       = message-msgv4 ).
  ENDMETHOD.
ENDCLASS.
