INTERFACE zif_cloud_logger
  PUBLIC .


  TYPES:
    tt_bapiret2 TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY .
  TYPES:
    tt_rap_messages  TYPE STANDARD TABLE OF REF TO if_abap_behv_message WITH EMPTY KEY .
  TYPES TY_flat_message TYPE string .
  TYPES:
    tt_flat_messages TYPE STANDARD TABLE OF TY_flat_message WITH EMPTY KEY .
  TYPES:
    BEGIN OF t_log_messages,
      symsg     TYPE symsg,
      message   TYPE bapiret2-message,
      type      TYPE symsgty,
      user_name TYPE syuname,
      date      TYPE xsddate_d,
      time      TYPE xsdtime_t,
      item      TYPE REF TO if_bali_item_setter,
    END OF t_log_messages .
  TYPES:
    tt_log_messages     TYPE STANDARD TABLE OF t_log_messages    WITH DEFAULT KEY .
  TYPES:
    BEGIN OF t_logger_instance,
      log_object    TYPE        cl_bali_header_setter=>ty_object,
      log_subobject TYPE        cl_bali_header_setter=>ty_subobject,
      extnumber     TYPE        cl_bali_header_setter=>ty_external_id,
      logger        TYPE REF TO zif_cloud_logger,
    END OF t_logger_instance .
  TYPES:
    tt_logger_instances TYPE STANDARD TABLE OF t_logger_instance WITH KEY log_object log_subobject extnumber .

  CONSTANTS:
    BEGIN OF c_message_type,
      error           TYPE symsgty VALUE 'E',
      success         TYPE symsgty VALUE 'S',
      warning         TYPE symsgty VALUE 'W',
      abandon         TYPE symsgty VALUE 'A',
      terminate       TYPE symsgty VALUE 'X',
      error_pattern   TYPE c       LENGTH 3 VALUE 'AEX',
      warning_pattern TYPE c       LENGTH 4 VALUE 'AEXW',
    END OF c_message_type .
  CONSTANTS:
    BEGIN OF c_default_message_attributes,
      type TYPE symsgty VALUE c_message_type-warning,
      id   TYPE symsgid VALUE 'CL',
      no   TYPE symsgno VALUE '000',
    END OF c_default_message_attributes .
  CONSTANTS:
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

  METHODS merge_logs
    IMPORTING
      !im_external_log TYPE REF TO zif_cloud_logger .
  METHODS log_string_add
    IMPORTING
      !iv_string TYPE string
      !iv_msgty  TYPE symsgty DEFAULT c_default_message_attributes-type .
  METHODS log_message_add
    IMPORTING
      !iv_msgty TYPE symsgty DEFAULT c_default_message_attributes-type
      !iv_msgid TYPE symsgid DEFAULT c_default_message_attributes-id
      !iv_msgno TYPE symsgno DEFAULT c_default_message_attributes-no
      !iv_msgv1 TYPE symsgv OPTIONAL
      !iv_msgv2 TYPE symsgv OPTIONAL
      !iv_msgv3 TYPE symsgv OPTIONAL
      !iv_msgv4 TYPE symsgv OPTIONAL .
  METHODS log_syst_add .
  METHODS log_exception_add
    IMPORTING
      !iv_severity  TYPE symsgty DEFAULT c_default_message_attributes-type
      !iv_exception TYPE REF TO cx_root .
  METHODS log_bapiret2_table_add
    IMPORTING
      !it_bapiret2_t TYPE tt_bapiret2 .
  METHODS log_bapiret2_structure_add
    IMPORTING
      !is_bapiret2 TYPE bapiret2 .
  METHODS save_application_log
    IMPORTING
      !im_use_2nd_db_connection      TYPE abap_boolean DEFAULT abap_false
      !im_assign_to_current_appl_job TYPE abap_boolean DEFAULT abap_false .
  METHODS get_messages
    RETURNING
      VALUE(re_result) TYPE tt_log_messages .
  METHODS get_messages_flat
    RETURNING
      VALUE(re_result) TYPE tt_flat_messages .
  METHODS get_messages_as_bapiret2
    RETURNING
      VALUE(re_bapiret2) TYPE tt_bapiret2 .
  METHODS get_messages_rap
    RETURNING
      VALUE(re_result) TYPE tt_rap_messages .
  METHODS get_handle
    RETURNING
      VALUE(re_handle) TYPE balloghndl .
  METHODS get_log_handle
    RETURNING
      VALUE(re_result) TYPE REF TO if_bali_log .
  METHODS get_message_count
    RETURNING
      VALUE(re_count) TYPE int4 .
  METHODS reset_appl_log .
  METHODS log_is_empty
    RETURNING
      VALUE(re_result) TYPE abap_boolean .
  METHODS log_contains_messages
    RETURNING
      VALUE(re_result) TYPE abap_boolean .
  METHODS log_contains_error
    RETURNING
      VALUE(re_result) TYPE abap_boolean .
  METHODS log_contains_warning
    RETURNING
      VALUE(RE_result) TYPE abap_boolean .
  METHODS search_message
    IMPORTING
      !im_search       TYPE symsg
    RETURNING
      VALUE(re_result) TYPE abap_boolean .
ENDINTERFACE.
