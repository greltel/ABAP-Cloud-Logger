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
      !IV_MSGID type SYMSGID default C_DEFAULT_MESSAGE_ATTRIBUTES-ID
      !IV_MSGNO type SYMSGNO default C_DEFAULT_MESSAGE_ATTRIBUTES-NO
      !IV_MSGTY type SYMSGTY default C_DEFAULT_MESSAGE_ATTRIBUTES-TYPE
      !IV_MSGV1 type SYMSGV
      !IV_MSGV2 type SYMSGV optional
      !IV_MSGV3 type SYMSGV optional
      !IV_MSGV4 type SYMSGV optional
      !IR_ITEM type ref to IF_BALI_ITEM_SETTER optional .
  class-methods GET_LONG_TEXT_FROM_MESSAGE
    importing
      !IV_MSGID type SYMSGID default C_DEFAULT_MESSAGE_ATTRIBUTES-ID
      !IV_MSGNO type SYMSGNO default C_DEFAULT_MESSAGE_ATTRIBUTES-NO
      !IV_MSGTY type SYMSGTY default C_DEFAULT_MESSAGE_ATTRIBUTES-TYPE
      !IV_MSGV1 type SYMSGV
      !IV_MSGV2 type SYMSGV
      !IV_MSGV3 type SYMSGV
      !IV_MSGV4 type SYMSGV
    returning
      value(RE_LONG_TEXT) type BAPIRET2-MESSAGE .
  class-methods GET_STRING_FROM_MESSAGE
    importing
      !IM_MESSAGE type SYMSG
    returning
      value(RE_RESULT) type TY_FLAT_MESSAGE .
  methods CREATE_EMERGENCY_LOG .
ENDCLASS.



CLASS ZCL_CLOUD_LOGGER IMPLEMENTATION.


  METHOD add_message_internal_log.

    INSERT VALUE #( item         = ir_item
                    symsg        = VALUE #( msgty = iv_msgty
                                            msgid = iv_msgid
                                            msgno = iv_msgno
                                            msgv1 = iv_msgv1
                                            msgv2 = iv_msgv2
                                            msgv3 = iv_msgv3
                                            msgv4 = iv_msgv4 )
                    message      = get_long_text_from_message( iv_msgid = iv_msgid
                                                               iv_msgno = iv_msgno
                                                               iv_msgty = iv_msgty
                                                               iv_msgv1 = iv_msgv1
                                                               iv_msgv2 = iv_msgv2
                                                               iv_msgv3 = iv_msgv3
                                                               iv_msgv4 = iv_msgv4
                                                             )
                    type         = iv_msgty
                    user_name    = cl_abap_context_info=>get_user_alias( )
                    date         = cl_abap_context_info=>get_system_date( )
                    time         = cl_abap_context_info=>get_system_time( ) ) INTO TABLE me->lt_log_messages.

    IF lo_emergency_log IS BOUND AND me->lv_enable_emergency_log EQ abap_true.

      lo_emergency_log->add_message( is_symsg = VALUE #( msgty = iv_msgty
                                                         msgid = iv_msgid
                                                         msgno = iv_msgno
                                                         msgv1 = iv_msgv1
                                                         msgv2 = iv_msgv2
                                                         msgv3 = iv_msgv3
                                                         msgv4 = iv_msgv4 ) ).

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    TRY.

        me->lo_log_handle = cl_bali_log=>create( ).
        me->lv_object     = iv_object.
        me->lv_subobject  = iv_subobject.
        me->lv_ext_number = iv_ext_number.
        me->lv_expiry_date = iv_expiry_date.
        me->lv_enable_emergency_log = iv_enable_emergency_log.

        TRY.
            me->lo_header = cl_bali_header_setter=>create( object      = me->lv_object
                                                           subobject   = me->lv_subobject
                                                           external_id = me->lv_ext_number
            )->set_expiry( expiry_date       = COND #( WHEN iv_expiry_date IS SUPPLIED AND iv_expiry_date IS NOT INITIAL
                                                       THEN iv_expiry_date
                                                       ELSE CONV d( cl_abap_context_info=>get_system_date( ) + 5 ) )
                           keep_until_expiry = abap_true ).

            me->lo_log_handle->set_header( lo_header ).

          CATCH cx_bali_runtime cx_uuid_error INTO DATA(lo_exception_2).
            DATA(lv_exception_text) = lo_exception_2->get_text( ).
        ENDTRY.

        me->lv_db_save          = COND #( WHEN iv_object IS SUPPLIED AND iv_object IS NOT INITIAL THEN iv_db_save
                                          ELSE abap_false ).

        me->create_emergency_log( ).

      CATCH cx_bali_runtime cx_uuid_error INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_creation
                                                    previous = lo_exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_instance.

    IF line_exists( lt_logger_instances[ log_object    = iv_object
                                         log_subobject = iv_subobject
                                         extnumber     = iv_ext_number ] ).

      re_logger_instance = VALUE #( lt_logger_instances[ log_object = iv_object log_subobject = iv_subobject extnumber = iv_ext_number ]-logger OPTIONAL )..


    ELSE.

      re_logger_instance = NEW zcl_cloud_logger( iv_object                = iv_object
                                                 iv_subobject             = iv_subobject
                                                 iv_ext_number            = iv_ext_number
                                                 iv_db_save               = iv_db_save
                                                 iv_enable_emergency_log  = iv_enable_emergency_log
                                                 iv_expiry_date           = iv_expiry_date  ).

      APPEND VALUE #( log_object    = iv_object
                      log_subobject = iv_subobject
                      extnumber     = iv_ext_number
                      logger        = re_logger_instance ) TO lt_logger_instances.

    ENDIF.

  ENDMETHOD.


  METHOD get_long_text_from_message.

    RETURN xco_cp=>message( VALUE #( msgty = iv_msgty
                                     msgid = iv_msgid
                                     msgno = iv_msgno
                                     msgv1 = iv_msgv1
                                     msgv2 = iv_msgv2
                                     msgv3 = iv_msgv3
                                     msgv4 = iv_msgv4 ) )->get_text( ).

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

    TRY.
        DATA(lo_item) = cl_bali_message_setter=>create_from_bapiret2( is_bapiret2 ).

        me->lo_log_handle->add_item( lo_item ).

        me->add_message_internal_log( iv_msgid = is_bapiret2-id
                                      iv_msgno = is_bapiret2-number
                                      iv_msgty = is_bapiret2-type
                                      iv_msgv1 = is_bapiret2-message_v1
                                      iv_msgv2 = is_bapiret2-message_v2
                                      iv_msgv3 = is_bapiret2-message_v3
                                      iv_msgv4 = is_bapiret2-message_v4
                                      ir_item  = lo_item ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_logging
                                                    previous = lo_exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_bapiret2_table_add.

    LOOP AT it_bapiret2_t REFERENCE INTO DATA(lo_bapiret2_structure).
      me->log_bapiret2_structure_add( lo_bapiret2_structure->* ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_contains_error.

    CHECK me->lo_log_handle IS BOUND.

    TRY.

        LOOP AT me->lo_log_handle->get_all_items( ) ASSIGNING FIELD-SYMBOL(<fs>).

          IF <fs>-item->severity CA c_message_type-error_pattern.
            RETURN abap_true.
          ENDIF.

        ENDLOOP.

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_contains_messages.

    CHECK me->lo_log_handle IS BOUND.

    TRY.

        RETURN COND #( WHEN me->lo_log_handle->get_all_items( ) IS NOT INITIAL THEN abap_true
                       ELSE abap_false ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_contains_warning.

    CHECK me->lo_log_handle IS BOUND.

    TRY.

        LOOP AT me->lo_log_handle->get_all_items( ) ASSIGNING FIELD-SYMBOL(<fs>).

          IF <fs>-item->severity CA c_message_type-warning_pattern.
            RETURN abap_true.
          ENDIF.

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

        me->add_message_internal_log( iv_msgty = iv_severity
                                      iv_msgv1 = CONV #( iv_exception->get_text( ) )
                                      ir_item  = lo_item ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_logging
                                                    previous = lo_exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_is_empty.

    CHECK me->lo_log_handle IS BOUND.

    TRY.
        RETURN COND #( WHEN me->lo_log_handle->get_all_items( ) IS INITIAL THEN abap_true
                       ELSE abap_false ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~log_message_add.

    CHECK me->lo_log_handle IS BOUND.

    TRY.

        DATA(lo_item) = cl_bali_message_setter=>create( severity   = iv_msgty
                                                        id         = iv_msgid
                                                        number     = iv_msgno
                                                        variable_1 = iv_msgv1
                                                        variable_2 = iv_msgv2
                                                        variable_3 = iv_msgv3
                                                        variable_4 = iv_msgv4 ).

        me->lo_log_handle->add_item( lo_item ).

        me->add_message_internal_log( iv_msgid = iv_msgid
                                      iv_msgno = iv_msgno
                                      iv_msgty = iv_msgty
                                      iv_msgv1 = iv_msgv1
                                      iv_msgv2 = iv_msgv2
                                      iv_msgv3 = iv_msgv3
                                      iv_msgv4 = iv_msgv4
                                      ir_item  = lo_item ).

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

        me->add_message_internal_log( iv_msgty = iv_msgty
                                      iv_msgv1 = CONV #( iv_string )
                                      ir_item  = lo_item ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_logging
                                                    previous = lo_exception ).
    ENDTRY.


  ENDMETHOD.


  METHOD zif_cloud_logger~log_syst_add.

    CHECK me->lo_log_handle IS BOUND.

    TRY.

        DATA(lo_item) = cl_bali_message_setter=>create_from_sy( ).

        lo_log_handle->add_item( lo_item ).

        DATA(lo_xco_message) = xco_cp=>sy->message( ).

        me->add_message_internal_log( iv_msgid = lo_xco_message->value-msgid
                                      iv_msgno = lo_xco_message->value-msgno
                                      iv_msgty = lo_xco_message->value-msgty
                                      iv_msgv1 = lo_xco_message->value-msgv1
                                      iv_msgv2 = lo_xco_message->value-msgv2
                                      iv_msgv3 = lo_xco_message->value-msgv3
                                      iv_msgv4 = lo_xco_message->value-msgv4
                                      ir_item  = lo_item
                                     ).

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

    CHECK me->lo_log_handle IS BOUND.

    TRY.
        cl_bali_log_db=>get_instance( )->delete_log( me->lo_log_handle ).
      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
    ENDTRY.

    TRY.
        CLEAR me->lo_log_handle.
        me->lo_log_handle = cl_bali_log=>create( ).

        CLEAR me->lt_log_messages.

      CATCH cx_bali_runtime INTO lo_exception.
        lv_exception_text = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_cloud_logger~save_application_log.

    IF me->lv_enable_emergency_log = abap_true.
      RETURN.
    ENDIF.

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

    DATA lr_search_class  TYPE RANGE OF symsgid.
    DATA lr_search_number TYPE RANGE OF symsgno.
    DATA lr_search_type   TYPE RANGE OF symsgty.

    IF im_search-msgid IS NOT INITIAL.
      lr_search_class = VALUE #( ( sign   = zcl_cloud_logger=>c_select_options-sign_include
                                option = zcl_cloud_logger=>c_select_options-option_equal
                                low    = im_search-msgid ) ).
    ENDIF.

    IF im_search-msgno IS NOT INITIAL.
      lr_search_number = VALUE #( ( sign  = zcl_cloud_logger=>c_select_options-sign_include
                                option = zcl_cloud_logger=>c_select_options-option_equal
                                low    = im_search-msgno ) ).
    ENDIF.

    IF im_search-msgty IS NOT INITIAL.
      lr_search_type = VALUE #( ( sign   = zcl_cloud_logger=>c_select_options-sign_include
                               option = zcl_cloud_logger=>c_select_options-option_equal
                               low    = im_search-msgty ) ).
    ENDIF.

    LOOP AT me->lt_log_messages INTO DATA(found_message) WHERE    symsg-msgid IN lr_search_class
                                                              AND symsg-msgno IN lr_search_number
                                                              AND symsg-msgty IN lr_search_type.
      RETURN abap_true.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_emergency_log.

    CHECK me->lv_enable_emergency_log EQ abap_true.

    IF me->lv_ext_number IS INITIAL.
      me->lv_ext_number = xco_cp=>uuid( )->as( xco_cp_uuid=>format->c36 )->value.
    ENDIF.

    TRY.

        lo_emergency_log = xco_cp_bal=>for->database( )->log->create( iv_object      = me->lv_object
                                                                      iv_subobject   = me->lv_subobject
                                                                      iv_external_id = me->lv_ext_number ).

      CATCH cx_root INTO DATA(lo_xco_error).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_emergency_log
                                                    previous = lo_xco_error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
