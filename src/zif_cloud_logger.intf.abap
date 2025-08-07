interface ZIF_CLOUD_LOGGER
  public .


  types:
    tt_bapiret2 TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY .
  types:
    tt_rap_messages  TYPE STANDARD TABLE OF REF TO if_abap_behv_message WITH EMPTY KEY .
  types FLAT_MESSAGE type STRING .
  types:
    tt_flat_messages TYPE STANDARD TABLE OF flat_message WITH EMPTY KEY .
  types:
    BEGIN OF t_log_messages,
      symsg     TYPE symsg,
      message   TYPE bapiret2-message,
      type      TYPE symsgty,
      user_name TYPE syuname,
      date      TYPE datum,
      time      TYPE uzeit,
      item      TYPE REF TO if_bali_item_setter,
    END OF t_log_messages .
  types:
    tt_log_messages     TYPE STANDARD TABLE OF t_log_messages    WITH DEFAULT KEY .
  types:
    BEGIN OF t_logger_instance,
      log_object    TYPE        cl_bali_header_setter=>ty_object,
      log_subobject TYPE        cl_bali_header_setter=>ty_subobject,
      extnumber     TYPE        cl_bali_header_setter=>ty_external_id,
      logger        TYPE REF TO ZIF_CLOUD_LOGGER,
    END OF t_logger_instance .
  types:
    tt_logger_instances TYPE STANDARD TABLE OF t_logger_instance WITH KEY log_object log_subobject extnumber .

  constants:
    BEGIN OF c_message_type,
      error           TYPE symsgty VALUE 'E',
      success         TYPE symsgty VALUE 'S',
      warning         TYPE symsgty VALUE 'W',
      abandon         TYPE symsgty VALUE 'A',
      terminate       TYPE symsgty VALUE 'X',
      error_pattern   TYPE c       LENGTH 3 VALUE 'AEX',
      warning_pattern TYPE c       LENGTH 4 VALUE 'AEXW',
    END OF c_message_type .
  constants:
    BEGIN OF c_default_message_attributes,
      type TYPE symsgty VALUE c_message_type-warning,
      id   TYPE symsgid VALUE 'CL',
      no   TYPE symsgno VALUE '000',
    END OF c_default_message_attributes .
  constants:
    BEGIN OF c_select_options,
      option_between              TYPE ddoption VALUE 'BT',
      option_contains_pattern     TYPE ddoption VALUE 'CP',
      option_equal                TYPE ddoption VALUE 'EQ',
      option_greater              TYPE ddoption VALUE 'GT',
      option_greater_equal        TYPE ddoption VALUE 'GE',
      option_less                 TYPE ddoption VALUE 'LT',
      option_less_equal           TYPE ddoption VALUE 'LE',
      option_not_between          TYPE ddoption VALUE 'NB',
      option_not_contains_pattern TYPE ddoption VALUE 'NP',
      option_not_equal            TYPE ddoption VALUE 'NE',
      sign_exclude                TYPE ddsign   VALUE 'E',
      sign_include                TYPE ddsign   VALUE 'I',
    END OF c_select_options .

  methods MERGE_LOGS
    importing
      !IM_EXTERNAL_LOG type ref to ZIF_CLOUD_LOGGER .
  methods LOG_STRING_ADD
    importing
      !IV_STRING type STRING
      !IV_MSGTY type SYMSGTY default C_DEFAULT_MESSAGE_ATTRIBUTES-TYPE .
  methods LOG_MESSAGE_ADD
    importing
      !IV_MSGTY type SYMSGTY default C_DEFAULT_MESSAGE_ATTRIBUTES-TYPE
      !IV_MSGID type SYMSGID default C_DEFAULT_MESSAGE_ATTRIBUTES-ID
      !IV_MSGNO type SYMSGNO default C_DEFAULT_MESSAGE_ATTRIBUTES-NO
      !IV_MSGV1 type SYMSGV
      !IV_MSGV2 type SYMSGV
      !IV_MSGV3 type SYMSGV
      !IV_MSGV4 type SYMSGV .
  methods LOG_SYST_ADD .
  methods LOG_EXCEPTION_ADD
    importing
      !IV_SEVERITY type SYMSGTY
      !IV_EXCEPTION type ref to CX_ROOT .
  methods LOG_BAPIRET2_TABLE_ADD
    importing
      !IT_BAPIRET2_T type TT_BAPIRET2 .
  methods LOG_BAPIRET2_STRUCTURE_ADD
    importing
      !IS_BAPIRET2 type BAPIRET2 .
  methods SAVE_APPLICATION_LOG
    importing
      !IM_USE_2ND_DB_CONNECTION type ABAP_BOOLEAN default ABAP_FALSE
      !IM_ASSIGN_TO_CURRENT_APPL_JOB type ABAP_BOOLEAN default ABAP_FALSE .
  methods GET_MESSAGES
    returning
      value(RESULT) type TT_LOG_MESSAGES .
  methods GET_MESSAGES_FLAT
    returning
      value(RESULT) type TT_FLAT_MESSAGES .
  methods GET_MESSAGES_AS_BAPIRET2
    returning
      value(RE_BAPIRET2) type TT_BAPIRET2 .
  methods GET_MESSAGES_RAP
    returning
      value(RESULT) type TT_RAP_MESSAGES .
  methods GET_HANDLE
    returning
      value(RE_HANDLE) type BALLOGHNDL .
  methods GET_LOG_HANDLE
    returning
      value(RESULT) type ref to IF_BALI_LOG .
  methods GET_MESSAGE_COUNT
    returning
      value(RE_COUNT) type INT4 .
  methods RESET_APPL_LOG .
  methods LOG_IS_EMPTY
    returning
      value(RESULT) type ABAP_BOOLEAN .
  methods LOG_CONTAINS_MESSAGES
    returning
      value(RESULT) type ABAP_BOOLEAN .
  methods LOG_CONTAINS_ERROR
    returning
      value(RESULT) type ABAP_BOOLEAN .
  methods LOG_CONTAINS_WARNING
    returning
      value(RESULT) type ABAP_BOOLEAN .
  methods SEARCH_MESSAGE
    importing
      !IM_SEARCH type SYMSG
    returning
      value(RESULT) type ABAP_BOOLEAN .
endinterface.
