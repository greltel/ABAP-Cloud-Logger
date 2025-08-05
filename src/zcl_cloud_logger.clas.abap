CLASS zcl_cloud_logger DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      tt_bapiret2 TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY .
    TYPES:
      tt_rap_messages  TYPE STANDARD TABLE OF REF TO if_abap_behv_message WITH EMPTY KEY .
    TYPES flat_message TYPE string .
    TYPES:
      tt_flat_messages TYPE STANDARD TABLE OF flat_message WITH EMPTY KEY .
    TYPES:
      BEGIN OF t_log_messages,
        symsg     TYPE symsg,
        message   TYPE bapiret2-message,
        type      TYPE symsgty,
        user_name TYPE syuname,
        date      TYPE datum,
        time      TYPE uzeit,
        item      TYPE REF TO if_bali_item_setter,
      END OF t_log_messages .
    TYPES:
      tt_log_messages     TYPE STANDARD TABLE OF t_log_messages    WITH DEFAULT KEY .

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

    CLASS-METHODS get_instance
      IMPORTING
        !iv_object                TYPE cl_bali_header_setter=>ty_object OPTIONAL
        !iv_subobject             TYPE cl_bali_header_setter=>ty_subobject OPTIONAL
        !iv_ext_number            TYPE cl_bali_header_setter=>ty_external_id OPTIONAL
        !iv_db_save               TYPE abap_boolean DEFAULT abap_false
        !iv_expiry_date           TYPE datum OPTIONAL
      RETURNING
        VALUE(re_logger_instance) TYPE REF TO zcl_cloud_logger .
    METHODS merge_logs
      IMPORTING
        !im_external_log TYPE REF TO zcl_cloud_logger .
    METHODS log_string_add
      IMPORTING
        !iv_string TYPE string
        !iv_msgty  TYPE symsgty DEFAULT c_default_message_attributes-type .
    METHODS log_message_add
      IMPORTING
        !iv_msgty TYPE symsgty DEFAULT c_default_message_attributes-type
        !iv_msgid TYPE symsgid DEFAULT c_default_message_attributes-id
        !iv_msgno TYPE symsgno DEFAULT c_default_message_attributes-no
        !iv_msgv1 TYPE symsgv
        !iv_msgv2 TYPE symsgv OPTIONAL
        !iv_msgv3 TYPE symsgv OPTIONAL
        !iv_msgv4 TYPE symsgv OPTIONAL .
    METHODS log_syst_add .
    METHODS log_exception_add
      IMPORTING
        !iv_severity  TYPE symsgty
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
        VALUE(result) TYPE tt_log_messages .
    METHODS get_messages_flat
      RETURNING
        VALUE(result) TYPE tt_flat_messages .
    METHODS get_messages_as_bapiret2
      RETURNING
        VALUE(re_bapiret2) TYPE tt_bapiret2 .
    METHODS get_messages_rap
      RETURNING
        VALUE(result) TYPE tt_rap_messages .
    METHODS get_handle
      RETURNING
        VALUE(re_handle) TYPE balloghndl .
    METHODS get_message_count
      RETURNING
        VALUE(re_count) TYPE int4 .
    METHODS reset_appl_log .
    METHODS log_is_empty
      RETURNING
        VALUE(re_empty) TYPE abap_boolean .
    METHODS log_contains_messages
      RETURNING
        VALUE(re_message) TYPE abap_boolean .
    METHODS log_contains_error
      RETURNING
        VALUE(re_error) TYPE abap_boolean .
    METHODS log_contains_warning
      RETURNING
        VALUE(re_warning) TYPE abap_boolean .
    METHODS search_message
      IMPORTING
        !im_search    TYPE symsg
      RETURNING
        VALUE(result) TYPE abap_boolean .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_logger_instance,
        log_object    TYPE        cl_bali_header_setter=>ty_object,
        log_subobject TYPE        cl_bali_header_setter=>ty_subobject,
        extnumber     TYPE        cl_bali_header_setter=>ty_external_id,
        logger        TYPE REF TO zcl_cloud_logger,
      END OF t_logger_instance .
    TYPES:
      tt_logger_instances TYPE STANDARD TABLE OF t_logger_instance WITH KEY log_object log_subobject extnumber .

    CLASS-DATA lt_logger_instances TYPE tt_logger_instances .
    DATA lo_log_handle TYPE REF TO if_bali_log .
    DATA lo_header TYPE REF TO if_bali_header_setter .
    DATA lv_db_save TYPE abap_boolean .
    DATA lt_log_messages TYPE tt_log_messages .

    METHODS constructor
      IMPORTING
        !iv_object      TYPE cl_bali_header_setter=>ty_object OPTIONAL
        !iv_subobject   TYPE cl_bali_header_setter=>ty_subobject OPTIONAL
        !iv_ext_number  TYPE cl_bali_header_setter=>ty_external_id OPTIONAL
        !iv_db_save     TYPE abap_boolean DEFAULT abap_true
        !iv_expiry_date TYPE datum OPTIONAL .
    METHODS add_message_internal_log
      IMPORTING
        !iv_msgid TYPE symsgid DEFAULT c_default_message_attributes-id
        !iv_msgno TYPE symsgno DEFAULT c_default_message_attributes-no
        !iv_msgty TYPE symsgty DEFAULT c_default_message_attributes-type
        !iv_msgv1 TYPE symsgv
        !iv_msgv2 TYPE symsgv OPTIONAL
        !iv_msgv3 TYPE symsgv OPTIONAL
        !iv_msgv4 TYPE symsgv OPTIONAL
        !ir_item  TYPE REF TO if_bali_item_setter OPTIONAL .
    CLASS-METHODS get_long_text_from_message
      IMPORTING
        !iv_msgid           TYPE symsgid DEFAULT c_default_message_attributes-id
        !iv_msgno           TYPE symsgno DEFAULT c_default_message_attributes-no
        !iv_msgty           TYPE symsgty DEFAULT c_default_message_attributes-type
        !iv_msgv1           TYPE symsgv
        !iv_msgv2           TYPE symsgv
        !iv_msgv3           TYPE symsgv
        !iv_msgv4           TYPE symsgv
      RETURNING
        VALUE(re_long_text) TYPE bapiret2-message .
    CLASS-METHODS get_string_from_message
      IMPORTING
        !im_message   TYPE symsg
      RETURNING
        VALUE(result) TYPE flat_message .
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

  ENDMETHOD.


  METHOD constructor.

    TRY.

        me->lo_log_handle = cl_bali_log=>create( ).

        me->lo_header = cl_bali_header_setter=>create( object      = iv_object
                                                       subobject   = iv_subobject
                                                       external_id = cl_system_uuid=>create_uuid_c32_static( )
        )->set_expiry( expiry_date       = COND #( WHEN iv_expiry_date IS SUPPLIED AND iv_expiry_date IS NOT INITIAL
                                                   THEN iv_expiry_date
                                                   ELSE CONV d( cl_abap_context_info=>get_system_date( ) + 5 ) )
                       keep_until_expiry = abap_true ).

        me->lo_log_handle->set_header( lo_header ).

        me->lv_db_save          = COND #( WHEN iv_object IS SUPPLIED AND iv_object IS NOT INITIAL THEN iv_db_save
                                          ELSE abap_false ).

      CATCH cx_bali_runtime cx_uuid_error INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_in_creation
                                                    previous = lo_exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD log_contains_messages.

    CHECK me->lo_log_handle IS BOUND.

    TRY.

        RETURN COND #( WHEN me->lo_log_handle->get_all_items( ) IS NOT INITIAL THEN abap_true
                       ELSE abap_false ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_Text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD log_exception_add.

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


  METHOD log_string_add.

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


  METHOD log_syst_add.

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


  METHOD get_handle.

    RETURN me->lo_log_handle->get_handle( ).

  ENDMETHOD.


  METHOD get_instance.

    IF line_exists( lt_logger_instances[ log_object    = iv_object
                                         log_subobject = iv_subobject
                                         extnumber     = iv_ext_number ] ).

      re_logger_instance                  = VALUE #( lt_logger_instances[ log_object = iv_object log_subobject = iv_subobject extnumber = iv_ext_number ]-logger OPTIONAL ).

      re_logger_instance->lv_db_save      = COND #( WHEN iv_object IS NOT INITIAL THEN iv_db_save
                                                    ELSE abap_false ).


    ELSE.

      re_logger_instance = NEW #( iv_object           = iv_object
                                  iv_subobject        = iv_subobject
                                  iv_ext_number       = iv_ext_number
                                  iv_db_save          = iv_db_save ).

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


  METHOD get_messages_as_bapiret2.

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


  METHOD get_message_count.

    CHECK me->lo_log_handle IS BOUND.

    TRY.
        RETURN lines( me->lo_log_handle->get_all_items( ) ).
      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_Text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD log_bapiret2_structure_add.

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


  METHOD log_bapiret2_table_add.

    LOOP AT it_bapiret2_t ASSIGNING FIELD-SYMBOL(<fs_bapiret2>).
      me->log_bapiret2_structure_add( <fs_bapiret2> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD log_contains_error.

    CHECK me->lo_log_handle IS BOUND.

    TRY.

        LOOP AT me->lo_log_handle->get_all_items( ) ASSIGNING FIELD-SYMBOL(<fs>).

          IF <fs>-item->severity CA c_message_type-error_pattern.
            RETURN abap_true.
          ENDIF.

        ENDLOOP.

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_Text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD log_is_empty.

    CHECK me->lo_log_handle IS BOUND.

    TRY.
        RETURN COND #( WHEN me->lo_log_handle->get_all_items( ) IS INITIAL THEN abap_true
                       ELSE abap_false ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_Text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD log_message_add.

    CHECK me->lo_log_handle IS BOUND.

    TRY.

        DATA(lo_item) = cl_bali_message_setter=>create( severity   = iv_msgty
                                                        id         = iv_msgid
                                                        number     = iv_msgno
                                                        variable_1 = iv_msgv1
                                                        variable_2 = iv_msgv2
                                                        variable_3 = iv_msgv3
                                                        variable_4 = iv_msgv4 ).

        me->lo_log_handle->add_item( lo_item  ).

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


  METHOD reset_appl_log.

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


  METHOD save_application_log.

    CHECK me->lo_log_handle IS BOUND.

    TRY.
        cl_bali_log_db=>get_instance( )->save_log( log                        = me->lo_log_handle
                                                   use_2nd_db_connection      = im_use_2nd_db_connection
                                                   assign_to_current_appl_job = im_assign_to_current_appl_job ).
      CATCH cx_bali_runtime INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_cloud_logger_error( textid   = zcx_cloud_logger_error=>error_release
                                                    previous = lo_exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_messages.

    RETURN me->lt_log_messages.

  ENDMETHOD.


  METHOD get_messages_flat.

    RETURN VALUE #( FOR LS_message IN me->lt_log_messages
                ( get_string_from_message( im_message = VALUE #( msgty = LS_message-symsg-msgty
                                                                 msgid = LS_message-symsg-msgid
                                                                 msgno = LS_message-symsg-msgno
                                                                 msgv1 = LS_message-symsg-msgv1
                                                                 msgv2 = LS_message-symsg-msgv2
                                                                 msgv3 = LS_message-symsg-msgv3
                                                                 msgv4 = LS_message-symsg-msgv4
                                                                 ) ) ) ).

  ENDMETHOD.


  METHOD get_messages_rap.

    RETURN VALUE #( FOR LS_message IN me->lt_log_messages
                  ( zcx_aml_message=>new_message_from_symsg( LS_message-symsg ) ) ).

  ENDMETHOD.


  METHOD get_string_from_message.

    RETURN |{ im_message-msgty }{ im_message-msgno }({ im_message-msgid }) - { xco_cp=>message( im_message )->get_text( ) }|.

  ENDMETHOD.


  METHOD log_contains_warning.

    CHECK me->lo_log_handle IS BOUND.

    TRY.

        LOOP AT me->lo_log_handle->get_all_items( ) ASSIGNING FIELD-SYMBOL(<fs>).

          IF <fs>-item->severity CA c_message_type-warning_pattern.
            RETURN abap_true.
          ENDIF.

        ENDLOOP.

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_Text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD merge_logs.



    "Add to Internal Log
    DATA(lt_external_messages) = im_external_log->get_messages( ).
    INSERT LINES OF lt_external_messages INTO TABLE me->lt_log_messages.

    "Add to Handle
*    DATA(lo_external_handler) = im_external_log->GET_HANDLe( ).
*
*    LOOP AT lo_external_handler->get_all_items( ) ASSIGNING FIELD-SYMBOL(<fs>).
*
*    ENDLOOP.

  ENDMETHOD.


  METHOD search_message.

    DATA search_class  TYPE RANGE OF symsgid.
    DATA search_number TYPE RANGE OF symsgno.
    DATA search_type   TYPE RANGE OF symsgty.


    IF im_search-msgid IS NOT INITIAL.
      search_class = VALUE #( ( sign   = zcl_cloud_logger=>c_select_options-sign_include
                                option = zcl_cloud_logger=>c_select_options-option_equal
                                low    = im_search-msgid ) ).
    ENDIF.

    IF im_search-msgno IS NOT INITIAL.
      search_number = VALUE #( ( sign  = zcl_cloud_logger=>c_select_options-sign_include
                                option = zcl_cloud_logger=>c_select_options-option_equal
                                low    = im_search-msgno ) ).
    ENDIF.

    IF im_search-msgty IS NOT INITIAL.
      search_type = VALUE #( ( sign   = zcl_cloud_logger=>c_select_options-sign_include
                               option = zcl_cloud_logger=>c_select_options-option_equal
                               low    = im_search-msgty ) ).
    ENDIF.

    LOOP AT me->lt_log_messages INTO DATA(found_message) WHERE    symsg-msgid IN search_class
                                                              AND symsg-msgno IN search_number
                                                              AND symsg-msgty IN search_type.
      RETURN abap_True.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
