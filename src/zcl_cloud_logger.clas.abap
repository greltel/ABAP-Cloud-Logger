class ZCL_CLOUD_LOGGER definition
  public
  create private .

public section.

  interfaces ZIF_CLOUD_LOGGER .

  aliases C_DEFAULT_MESSAGE_ATTRIBUTES
    for ZIF_CLOUD_LOGGER~C_DEFAULT_MESSAGE_ATTRIBUTES .
  aliases C_MESSAGE_TYPE
    for ZIF_CLOUD_LOGGER~C_MESSAGE_TYPE .
  aliases C_SELECT_OPTIONS
    for ZIF_CLOUD_LOGGER~C_SELECT_OPTIONS .
  aliases FREE
    for ZIF_CLOUD_LOGGER~FREE .
  aliases GET_HANDLE
    for ZIF_CLOUD_LOGGER~GET_HANDLE .
  aliases GET_LOG_HANDLE
    for ZIF_CLOUD_LOGGER~GET_LOG_HANDLE .
  aliases GET_MESSAGES
    for ZIF_CLOUD_LOGGER~GET_MESSAGES .
  aliases GET_MESSAGES_AS_BAPIRET2
    for ZIF_CLOUD_LOGGER~GET_MESSAGES_AS_BAPIRET2 .
  aliases GET_MESSAGES_FLAT
    for ZIF_CLOUD_LOGGER~GET_MESSAGES_FLAT .
  aliases GET_MESSAGES_RAP
    for ZIF_CLOUD_LOGGER~GET_MESSAGES_RAP .
  aliases GET_MESSAGE_COUNT
    for ZIF_CLOUD_LOGGER~GET_MESSAGE_COUNT .
  aliases LOG_BAPIRET2_STRUCTURE_ADD
    for ZIF_CLOUD_LOGGER~LOG_BAPIRET2_STRUCTURE_ADD .
  aliases LOG_BAPIRET2_TABLE_ADD
    for ZIF_CLOUD_LOGGER~LOG_BAPIRET2_TABLE_ADD .
  aliases LOG_CONTAINS_ERROR
    for ZIF_CLOUD_LOGGER~LOG_CONTAINS_ERROR .
  aliases LOG_CONTAINS_MESSAGES
    for ZIF_CLOUD_LOGGER~LOG_CONTAINS_MESSAGES .
  aliases LOG_CONTAINS_WARNING
    for ZIF_CLOUD_LOGGER~LOG_CONTAINS_WARNING .
  aliases LOG_DATA_ADD
    for ZIF_CLOUD_LOGGER~LOG_DATA_ADD .
  aliases LOG_EXCEPTION_ADD
    for ZIF_CLOUD_LOGGER~LOG_EXCEPTION_ADD .
  aliases LOG_IS_EMPTY
    for ZIF_CLOUD_LOGGER~LOG_IS_EMPTY .
  aliases LOG_MESSAGE_ADD
    for ZIF_CLOUD_LOGGER~LOG_MESSAGE_ADD .
  aliases LOG_STRING_ADD
    for ZIF_CLOUD_LOGGER~LOG_STRING_ADD .
  aliases LOG_SYST_ADD
    for ZIF_CLOUD_LOGGER~LOG_SYST_ADD .
  aliases MERGE_LOGS
    for ZIF_CLOUD_LOGGER~MERGE_LOGS .
  aliases RESET_APPL_LOG
    for ZIF_CLOUD_LOGGER~RESET_APPL_LOG .
  aliases SAVE_APPLICATION_LOG
    for ZIF_CLOUD_LOGGER~SAVE_APPLICATION_LOG .
  aliases SEARCH_MESSAGE
    for ZIF_CLOUD_LOGGER~SEARCH_MESSAGE .
  aliases TT_BAPIRET2
    for ZIF_CLOUD_LOGGER~TT_BAPIRET2 .
  aliases TT_FLAT_MESSAGES
    for ZIF_CLOUD_LOGGER~TT_FLAT_MESSAGES .
  aliases TT_LOGGER_INSTANCES
    for ZIF_CLOUD_LOGGER~TT_LOGGER_INSTANCES .
  aliases TT_LOG_MESSAGES
    for ZIF_CLOUD_LOGGER~TT_LOG_MESSAGES .
  aliases TT_RAP_MESSAGES
    for ZIF_CLOUD_LOGGER~TT_RAP_MESSAGES .
  aliases TY_FLAT_MESSAGE
    for ZIF_CLOUD_LOGGER~TY_FLAT_MESSAGE .
  aliases T_LOGGER_INSTANCE
    for ZIF_CLOUD_LOGGER~T_LOGGER_INSTANCE .
  aliases T_LOG_MESSAGES
    for ZIF_CLOUD_LOGGER~T_LOG_MESSAGES .

  class-methods GET_INSTANCE
    importing
      !IV_ENABLE_EMERGENCY_LOG type ABAP_BOOLEAN default ABAP_FALSE
      !IV_OBJECT type CL_BALI_HEADER_SETTER=>TY_OBJECT optional
      !IV_SUBOBJECT type CL_BALI_HEADER_SETTER=>TY_SUBOBJECT optional
      !IV_EXT_NUMBER type CL_BALI_HEADER_SETTER=>TY_EXTERNAL_ID optional
      !IV_DB_SAVE type ABAP_BOOLEAN default ABAP_FALSE
      !IV_EXPIRY_DATE type XSDDATE_D optional
    returning
      value(RE_LOGGER_INSTANCE) type ref to ZIF_CLOUD_LOGGER .
  PROTECTED SECTION.
private section.

  types:
    t_severity_filter_range TYPE RANGE OF symsgty .

  class-data LT_LOGGER_INSTANCES type TT_LOGGER_INSTANCES .
  data LO_LOG_HANDLE type ref to IF_BALI_LOG .
  data LO_HEADER type ref to IF_BALI_HEADER_SETTER .
  data LO_EMERGENCY_LOG type ref to IF_XCO_CP_BAL_LOG .
  data LT_LOG_MESSAGES type TT_LOG_MESSAGES .
  data LV_DB_SAVE type ABAP_BOOLEAN .
  data LV_OBJECT type CL_BALI_HEADER_SETTER=>TY_OBJECT .
  data LV_SUBOBJECT type CL_BALI_HEADER_SETTER=>TY_SUBOBJECT .
  data LV_EXT_NUMBER type CL_BALI_HEADER_SETTER=>TY_EXTERNAL_ID .
  data LV_EXPIRY_DATE type XSDDATE_D .
  data LV_ENABLE_EMERGENCY_LOG type ABAP_BOOLEAN .
  DATA mv_timer_start TYPE timestampl.

  methods CONSTRUCTOR
    importing
      !IV_ENABLE_EMERGENCY_LOG type ABAP_BOOLEAN default ABAP_FALSE
      !IV_OBJECT type CL_BALI_HEADER_SETTER=>TY_OBJECT optional
      !IV_SUBOBJECT type CL_BALI_HEADER_SETTER=>TY_SUBOBJECT optional
      !IV_EXT_NUMBER type CL_BALI_HEADER_SETTER=>TY_EXTERNAL_ID optional
      !IV_DB_SAVE type ABAP_BOOLEAN default ABAP_TRUE
      !IV_EXPIRY_DATE type XSDDATE_D optional .
  methods ADD_MESSAGE_INTERNAL_LOG
    importing
      !IV_SYMSG type SYMSG optional
      !IR_ITEM type ref to IF_BALI_ITEM_SETTER optional .
  class-methods GET_LONG_TEXT_FROM_MESSAGE
    importing
      !IV_SYMSG type SYMSG
    returning
      value(RE_LONG_TEXT) type BAPIRET2-MESSAGE .
  class-methods GET_STRING_FROM_MESSAGE
    importing
      !IM_MESSAGE type SYMSG
    returning
      value(RE_RESULT) type TY_FLAT_MESSAGE .
  methods CREATE_EMERGENCY_LOG .
  methods CREATE_HEADER
    returning
      value(RE_HEADER) type ref to IF_BALI_HEADER_SETTER .
  methods GET_SEVERITY_FILTER
    importing
      !IV_MSGTY type SYMSGTY
    returning
      value(RV_FILTER) type ZCL_CLOUD_LOGGER=>T_SEVERITY_FILTER_RANGE .
ENDCLASS.



CLASS ZCL_CLOUD_LOGGER IMPLEMENTATION.


  METHOD add_message_internal_log.

    INSERT VALUE #( item         = ir_item
                    symsg        = iv_symsg
                    message      = get_long_text_from_message( iv_symsg )
                    type         = iv_symsg-msgty
                    user_name    = cl_abap_context_info=>get_user_alias( )
                    date         = cl_abap_context_info=>get_system_date( )
                    time         = cl_abap_context_info=>get_system_time( ) ) INTO TABLE me->lt_log_messages.

    IF lo_emergency_log IS BOUND AND me->lv_enable_emergency_log EQ abap_true.

      lo_emergency_log->add_message( is_symsg = iv_symsg ).

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    TRY.

        me->lo_log_handle           = cl_bali_log=>create( ).
        me->lv_object               = iv_object.
        me->lv_subobject            = iv_subobject.
        me->lv_ext_number           = iv_ext_number.
        me->lv_expiry_date          = iv_expiry_date.
        me->lv_enable_emergency_log = iv_enable_emergency_log.

        TRY.
            me->lo_header = create_header( ).

            me->lo_log_handle->set_header( lo_header ).

          CATCH cx_bali_runtime cx_uuid_error INTO DATA(lo_exception).
            DATA(lv_exception_text) = lo_exception->get_text( ).
            RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_creation
                                                        previous = lo_exception ).
        ENDTRY.

        me->lv_db_save          = COND #( WHEN iv_object IS SUPPLIED AND iv_object IS NOT INITIAL THEN iv_db_save
                                          ELSE abap_false ).

        me->create_emergency_log( ).

      CATCH cx_bali_runtime cx_uuid_error INTO DATA(lo_exception_new).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_creation
                                                    previous = lo_exception_new ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_instance.

    READ TABLE lt_logger_instances INTO DATA(ls_instance)
      WITH TABLE KEY log_object    = iv_object
                     log_subobject = iv_subobject
                     extnumber     = iv_ext_number.

    IF syst-subrc IS INITIAL.
      re_logger_instance = ls_instance-logger.
      RETURN.
    ENDIF.

    re_logger_instance = NEW zcl_cloud_logger( iv_object                = iv_object
                                               iv_subobject             = iv_subobject
                                               iv_ext_number            = iv_ext_number
                                               iv_db_save               = iv_db_save
                                               iv_enable_emergency_log  = iv_enable_emergency_log
                                               iv_expiry_date           = iv_expiry_date  ).

    INSERT VALUE #( log_object    = iv_object
                    log_subobject = iv_subobject
                    extnumber     = iv_ext_number
                    logger        = re_logger_instance ) INTO TABLE lt_logger_instances.

  ENDMETHOD.


  METHOD get_long_text_from_message.

    RETURN xco_cp=>message( iv_symsg )->get_text( ).

  ENDMETHOD.


  METHOD get_string_from_message.

    RETURN |{ im_message-msgty }{ im_message-msgno }({ im_message-msgid }) - { xco_cp=>message( im_message )->get_text( ) }|.

  ENDMETHOD.


  METHOD zif_cloud_logger~get_handle.

    RETURN me->lo_log_handle->get_handle( ).

  ENDMETHOD.


  METHOD zif_cloud_logger~get_log_handle.

    RETURN me->lo_log_handle.

  ENDMETHOD.


  METHOD zif_cloud_logger~get_messages.

    RETURN me->lt_log_messages.

  ENDMETHOD.


  METHOD zif_cloud_logger~get_messages_as_bapiret2.

    RETURN VALUE #( FOR ls_message IN me->lt_log_messages
              ( id         = ls_message-symsg-msgid
                number     = ls_message-symsg-msgno
                type       = ls_message-symsg-msgty
                message_v1 = ls_message-symsg-msgv1
                message_v2 = ls_message-symsg-msgv2
                message_v3 = ls_message-symsg-msgv3
                message_v4 = ls_message-symsg-msgv4
                message    = ls_message-message ) ).


  ENDMETHOD.


  METHOD zif_cloud_logger~get_messages_flat.

    RETURN VALUE #( FOR ls_message IN me->lt_log_messages
                   ( get_string_from_message( im_message = VALUE #( msgty = ls_message-symsg-msgty
                                                                    msgid = ls_message-symsg-msgid
                                                                    msgno = ls_message-symsg-msgno
                                                                    msgv1 = ls_message-symsg-msgv1
                                                                    msgv2 = ls_message-symsg-msgv2
                                                                    msgv3 = ls_message-symsg-msgv3
                                                                    msgv4 = ls_message-symsg-msgv4
                                                                  )
                                             )
                    )
                   ).

  ENDMETHOD.


  METHOD zif_cloud_logger~get_messages_rap.

    RETURN VALUE #( FOR ls_message IN me->lt_log_messages
                  ( zcx_cloud_logger_message=>new_message_from_symsg( ls_message-symsg ) ) ).

  ENDMETHOD.


  METHOD zif_cloud_logger~get_message_count.

    CHECK me->lo_log_handle IS BOUND.

    TRY.
        RETURN lines( me->lo_log_handle->get_all_items( ) ).
      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_bapiret2_structure_add.

    CHECK is_bapiret2 IS NOT INITIAL.

    TRY.
        DATA(lo_item) = cl_bali_message_setter=>create_from_bapiret2( is_bapiret2 ).

        me->lo_log_handle->add_item( lo_item ).

        me->add_message_internal_log( iv_symsg =  VALUE #( msgid = is_bapiret2-id
                                                           msgno = is_bapiret2-number
                                                           msgty = is_bapiret2-type
                                                           msgv1 = is_bapiret2-message_v1
                                                           msgv2 = is_bapiret2-message_v2
                                                           msgv3 = is_bapiret2-message_v3
                                                           msgv4 = is_bapiret2-message_v4 )
                                      ir_item  = lo_item ).

        ro_logger = me.

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_logging
                                                    previous = lo_exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_bapiret2_table_add.

    DATA(lr_severity_filter) = COND #( WHEN iv_min_severity IS NOT INITIAL THEN get_severity_filter( iv_min_severity )
                                       ELSE VALUE #( ) ).

    LOOP AT it_bapiret2_t REFERENCE INTO DATA(lo_bapiret2_structure) WHERE type IN lr_severity_filter.
      me->log_bapiret2_structure_add( lo_bapiret2_structure->* ).
    ENDLOOP.

    ro_logger = me.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_contains_error.

    CLEAR re_result.
    CHECK me->lo_log_handle IS BOUND.

    TRY.

        LOOP AT me->lo_log_handle->get_all_items( ) ASSIGNING FIELD-SYMBOL(<fs>) WHERE item->severity CA c_message_type-error_pattern.
          RETURN abap_true.
        ENDLOOP.

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_contains_messages.

    CLEAR re_result.
    CHECK me->lo_log_handle IS BOUND.

    TRY.

        RETURN COND #( WHEN me->lo_log_handle->get_all_items( ) IS NOT INITIAL THEN abap_true
                       ELSE abap_false ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_contains_warning.

    CLEAR re_result.
    CHECK me->lo_log_handle IS BOUND.

    TRY.

        LOOP AT me->lo_log_handle->get_all_items( ) ASSIGNING FIELD-SYMBOL(<fs>) WHERE item->severity CA c_message_type-warning_pattern.
          RETURN abap_true.
        ENDLOOP.

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_exception_add.

    CHECK iv_exception IS BOUND AND me->lo_log_handle IS BOUND..

    TRY.

        DATA(lo_item) =  cl_bali_exception_setter=>create( severity  = iv_severity
                                                            exception = iv_exception ).

        me->lo_log_handle->add_item( lo_item ).

        me->add_message_internal_log( iv_symsg =  VALUE #( msgty = iv_severity
                                                           msgv1 = CONV #( iv_exception->get_text( ) ) )
                                      ir_item  = lo_item   ).

        ro_logger = me.

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_logging
                                                    previous = lo_exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_is_empty.

    CHECK me->lo_log_handle IS BOUND.

    TRY.
        RETURN COND #( WHEN me->get_message_count( ) IS INITIAL THEN abap_true
                       ELSE abap_false ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_message_add.

    CHECK me->lo_log_handle IS BOUND.

    TRY.

        DATA(lo_item) = cl_bali_message_setter=>create( severity   = iv_symsg-msgty
                                                        id         = iv_symsg-msgid
                                                        number     = iv_symsg-msgno
                                                        variable_1 = iv_symsg-msgv1
                                                        variable_2 = iv_symsg-msgv2
                                                        variable_3 = iv_symsg-msgv3
                                                        variable_4 = iv_symsg-msgv4 ).

        me->lo_log_handle->add_item( lo_item ).

        me->add_message_internal_log( iv_symsg = iv_symsg
                                      ir_item  = lo_item ).

        ro_logger = me.

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_logging
                                                    previous = lo_exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_string_add.

    CHECK me->lo_log_handle IS BOUND.

    TRY.

        DATA(lo_item) = cl_bali_free_text_setter=>create( severity = iv_msgty
                                                          text     = CONV #( iv_string ) ).

        me->lo_log_handle->add_item( lo_item ).

        me->add_message_internal_log( iv_symsg = VALUE #( msgty = iv_msgty msgv1 = CONV #( iv_string ) )
                                      ir_item  = lo_item ).

        ro_logger = me.

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_logging
                                                    previous = lo_exception ).
    ENDTRY.


  ENDMETHOD.


  METHOD zif_cloud_logger~log_syst_add.

    CHECK me->lo_log_handle IS BOUND.

    TRY.

        DATA(lo_xco_message) = xco_cp=>sy->message( ).
        DATA(lo_item)        = cl_bali_message_setter=>create_from_sy( ).

        lo_log_handle->add_item( lo_item ).

        me->add_message_internal_log( iv_symsg = VALUE #( msgid = lo_xco_message->value-msgid
                                                          msgno = lo_xco_message->value-msgno
                                                          msgty = lo_xco_message->value-msgty
                                                          msgv1 = lo_xco_message->value-msgv1
                                                          msgv2 = lo_xco_message->value-msgv2
                                                          msgv3 = lo_xco_message->value-msgv3
                                                          msgv4 = lo_xco_message->value-msgv4 )
                                      ir_item  = lo_item ).

        ro_logger = me.

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_logging
                                                    previous = lo_exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~merge_logs.

    "Add to Internal Log
    INSERT LINES OF im_external_log->get_messages( ) INTO TABLE me->lt_log_messages.

    "Add to Handle
    TRY.
        me->lo_log_handle->add_all_items_from_other_log( im_external_log->get_log_handle( ) ).
      CATCH cx_bali_runtime INTO DATA(lo_exception).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~reset_appl_log.

    TRY.

        "Delete from Database
        IF me->lo_log_handle IS BOUND AND im_delete_from_db EQ abap_true.
          cl_bali_log_db=>get_instance( )->delete_log( me->lo_log_handle ).
        ENDIF.

        "Handle Recreation
        CLEAR me->lo_log_handle.
        me->lo_log_handle = cl_bali_log=>create( ).

        me->lo_header = COND #( WHEN me->lo_header IS BOUND THEN me->lo_header
                                ELSE create_header( ) ).

        me->lo_log_handle->set_header( me->lo_header ).

        CLEAR me->lt_log_messages.

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~save_application_log.

    CHECK me->lv_enable_emergency_log NE abap_true.
    CHECK me->lo_log_handle IS BOUND AND me->lv_db_save EQ abap_true.

    TRY.
        cl_bali_log_db=>get_instance( )->save_log( log                        = me->lo_log_handle
                                                   use_2nd_db_connection      = im_use_2nd_db_connection
                                                   assign_to_current_appl_job = im_assign_to_current_appl_job ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_release
                                                    previous = lo_exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~search_message.

    DATA lr_search_class  TYPE RANGE OF symsgid VALUE IS INITIAL.
    DATA lr_search_number TYPE RANGE OF symsgno VALUE IS INITIAL.
    DATA lr_search_type   TYPE RANGE OF symsgty VALUE IS INITIAL.

    lr_search_class = COND #( WHEN im_search-msgid IS NOT INITIAL
                              THEN VALUE #( ( sign   = zcl_cloud_logger=>c_select_options-sign_include
                                             option = zcl_cloud_logger=>c_select_options-option_equal
                                             low    = im_search-msgid ) )
                              ELSE VALUE #( ) ).

    lr_search_number = COND #( WHEN im_search-msgno IS NOT INITIAL
                               THEN VALUE #( ( sign  = zcl_cloud_logger=>c_select_options-sign_include
                                               option = zcl_cloud_logger=>c_select_options-option_equal
                                               low    = im_search-msgno ) )
                               ELSE VALUE #( ) ).

    lr_search_type = COND #( WHEN im_search-msgty IS NOT INITIAL
                             THEN VALUE #( ( sign   = zcl_cloud_logger=>c_select_options-sign_include
                                             option = zcl_cloud_logger=>c_select_options-option_equal
                                             low    = im_search-msgty ) )
                             ELSE VALUE #( ) ).

    LOOP AT me->lt_log_messages INTO DATA(found_message) WHERE    symsg-msgid IN lr_search_class
                                                              AND symsg-msgno IN lr_search_number
                                                              AND symsg-msgty IN lr_search_type.
      RETURN abap_true.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_emergency_log.

    CHECK me->lv_enable_emergency_log EQ abap_true.

    me->lv_ext_number = COND #( WHEN me->lv_ext_number IS INITIAL
                                THEN  xco_cp=>uuid( )->as( xco_cp_uuid=>format->c36 )->value
                                ELSE me->lv_ext_number ).

    TRY.

        lo_emergency_log = xco_cp_bal=>for->database( )->log->create( iv_object      = me->lv_object
                                                                      iv_subobject   = me->lv_subobject
                                                                      iv_external_id = me->lv_ext_number ).

      CATCH cx_root INTO DATA(lo_xco_error).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_emergency_log
                                                    previous = lo_xco_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD create_header.

    TRY.

        re_header = cl_bali_header_setter=>create( object      = me->lv_object
                                                   subobject   = me->lv_subobject
                                                   external_id = me->lv_ext_number
                    )->set_expiry( expiry_date       = COND #( WHEN me->lv_expiry_date IS NOT INITIAL
                                                               THEN me->lv_expiry_date
                                                               ELSE CONV d( cl_abap_context_info=>get_system_date( ) + 5 ) )
                                   keep_until_expiry = abap_true ).

      CATCH cx_bali_runtime cx_uuid_error INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_creation
                                                    previous = lo_exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~free.

    DELETE TABLE lt_logger_instances
      WITH TABLE KEY log_object    = me->lv_object
                     log_subobject = me->lv_subobject
                     extnumber     = me->lv_ext_number.

    CLEAR: me->lo_log_handle,
           me->lo_header,
           me->lt_log_messages.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_data_add.

    TRY.

        DATA(lv_json_string) = xco_cp_json=>data->from_abap( iv_data )->to_string( ).

        me->log_string_add( iv_string = lv_json_string
                            iv_msgty  = iv_msgty ).

      CATCH cx_root INTO DATA(lo_error).
        me->log_string_add( iv_string = |Error serializing data to JSON: { lo_error->get_text( ) }|
                            iv_msgty  = 'E' ).
    ENDTRY.

    ro_logger = me.

  ENDMETHOD.


  METHOD get_severity_filter.

    rv_filter = SWITCH #( iv_msgty
                          WHEN zif_cloud_logger~c_message_type-abandon OR zif_cloud_logger~c_message_type-terminate
                          THEN VALUE #( sign   = zcl_cloud_logger=>c_select_options-sign_include
                                        option = zcl_cloud_logger=>c_select_options-option_equal
                                       ( low = zif_cloud_logger~c_message_type-abandon )
                                       ( low = zif_cloud_logger~c_message_type-terminate ) )
                          WHEN zif_cloud_logger~c_message_type-error
                          THEN VALUE #( sign   = zcl_cloud_logger=>c_select_options-sign_include
                                        option = zcl_cloud_logger=>c_select_options-option_equal
                                      ( low = zif_cloud_logger~c_message_type-error )
                                      ( low = zif_cloud_logger~c_message_type-abandon )
                                      ( low = zif_cloud_logger~c_message_type-terminate ) )
                          WHEN zif_cloud_logger~c_message_type-warning
                          THEN VALUE #( sign   = zcl_cloud_logger=>c_select_options-sign_include
                                        option = zcl_cloud_logger=>c_select_options-option_equal
                                      ( low = zif_cloud_logger~c_message_type-warning )
                                      ( low = zif_cloud_logger~c_message_type-error )
                                      ( low = zif_cloud_logger~c_message_type-abandon )
                                      ( low = zif_cloud_logger~c_message_type-terminate ) )
                          ELSE VALUE #( ) ).

  ENDMETHOD.


  METHOD zif_cloud_logger~start_timer.

    GET TIME STAMP FIELD me->mv_timer_start.
    ro_logger = me.

  ENDMETHOD.


  METHOD zif_cloud_logger~stop_timer.

    IF me->mv_timer_start IS INITIAL.
      me->log_string_add( iv_string = CONV #( TEXT-002 )
                          iv_msgty  = zif_cloud_logger=>c_message_type-warning ).
      ro_logger = me.
      RETURN.
    ENDIF.

    GET TIME STAMP FIELD DATA(lv_now).

    TRY.
        DATA(lv_diff) = cl_abap_tstmp=>subtract( tstmp1 = lv_now
                                                 tstmp2 = me->mv_timer_start ).

        me->log_string_add( iv_string = |{ TEXT-003 } { iv_text } { TEXT-004 } { lv_diff } { TEXT-005 }|
                            iv_msgty  = zif_cloud_logger=>c_message_type-information ).

        CLEAR me->mv_timer_start.

      CATCH cx_parameter_invalid_range cx_parameter_invalid_type.
        me->log_string_add( iv_string = CONV #( TEXT-001 )
                            iv_msgty  = zif_cloud_logger=>c_message_type-error ).
    ENDTRY.

    ro_logger = me.

  ENDMETHOD.
ENDCLASS.
