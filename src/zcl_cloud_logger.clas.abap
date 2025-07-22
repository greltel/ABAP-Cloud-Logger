CLASS zcl_cloud_logger DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      tt_bapiret2 TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY,
      t_Char100   TYPE c LENGTH 100.
    CONSTANTS:
      BEGIN OF c_message_type,
        error         TYPE symsgty VALUE 'E',
        success       TYPE symsgty VALUE 'S',
        warning       TYPE symsgty VALUE 'W',
        abandon       TYPE symsgty VALUE 'A',
        terminate     TYPE symsgty VALUE 'X',
        error_pattern TYPE c       LENGTH 3  VALUE 'AEX',
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
        !iv_object                TYPE balobj_d OPTIONAL
        !iv_subobject             TYPE balsubobj OPTIONAL
        !iv_ext_number            TYPE t_Char100 OPTIONAL
        !iv_db_save               TYPE abap_bool DEFAULT abap_false
        !iv_expiry_date           TYPE datum OPTIONAL
      RETURNING
        VALUE(re_logger_instance) TYPE REF TO zcl_cloud_logger .
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
        !iv_in_update_task     TYPE abap_bool DEFAULT abap_false
        !iv_save_all           TYPE abap_bool DEFAULT abap_false
        !im_2th_connection     TYPE abap_bool DEFAULT abap_false
        !im_2th_connect_commit TYPE abap_bool DEFAULT abap_false .
    METHODS get_messages_as_bapiret2
      RETURNING
        VALUE(re_bapiret2) TYPE tt_bapiret2 .
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
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_log_messages,
        message   TYPE bapiret2-message,
        type      TYPE symsgty,
        user_name TYPE syuname,
        date      TYPE datum,
        time      TYPE uzeit,
      END OF t_log_messages .
    TYPES:
      BEGIN OF t_logger_instance,
        log_object    TYPE        balobj_d,
        log_subobject TYPE        balsubobj,
        extnumber     TYPE        t_Char100,
        logger        TYPE REF TO zcl_cloud_logger,
      END OF t_logger_instance .
    TYPES:
      tt_log_messages     TYPE STANDARD TABLE OF t_log_messages    WITH DEFAULT KEY .
    TYPES:
      tt_logger_instances TYPE STANDARD TABLE OF t_logger_instance WITH KEY log_object log_subobject extnumber .

    CLASS-DATA lt_logger_instances TYPE tt_logger_instances .
    DATA lo_log_handle TYPE REF TO if_bali_log .
    DATA lo_header TYPE REF TO if_bali_header_setter .
    DATA lv_db_save TYPE abap_bool .
    DATA lt_log_messages TYPE tt_log_messages .
    DATA lt_bapiret2_messages TYPE tt_bapiret2 .

    METHODS constructor
      IMPORTING
        !iv_object      TYPE balobj_d OPTIONAL
        !iv_subobject   TYPE balsubobj OPTIONAL
        !iv_ext_number  TYPE t_Char100   OPTIONAL
        !iv_db_save     TYPE abap_bool DEFAULT abap_true
        !iv_expiry_date TYPE datum OPTIONAL .
    METHODS add_message_internal_log
      IMPORTING
        !iv_msgid TYPE symsgid DEFAULT c_default_message_attributes-id
        !iv_msgno TYPE symsgno DEFAULT c_default_message_attributes-no
        !iv_msgty TYPE symsgty DEFAULT c_default_message_attributes-type
        !iv_msgv1 TYPE symsgv
        !iv_msgv2 TYPE symsgv OPTIONAL
        !iv_msgv3 TYPE symsgv OPTIONAL
        !iv_msgv4 TYPE symsgv OPTIONAL .
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
ENDCLASS.



CLASS ZCL_CLOUD_LOGGER IMPLEMENTATION.


  METHOD add_message_internal_log.

    APPEND VALUE #( message      = get_long_text_from_message( iv_msgid = iv_msgid
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
                    time         = cl_abap_context_info=>get_system_time( ) ) TO me->lt_log_messages.

    me->lt_bapiret2_messages = VALUE #( BASE me->lt_bapiret2_messages ( id         = iv_msgid
                                                                        number     = iv_msgno
                                                                        type       = iv_msgty
                                                                        message_v1 = iv_msgv1
                                                                        message_v2 = iv_msgv2
                                                                        message_v3 = iv_msgv3
                                                                        message_v4 = iv_msgv4
                                                                        message    = get_long_text_from_message( iv_msgid = iv_msgid
                                                                                                                 iv_msgno = iv_msgno
                                                                                                                 iv_msgty = iv_msgty
                                                                                                                 iv_msgv1 = iv_msgv1
                                                                                                                 iv_msgv2 = iv_msgv2
                                                                                                                 iv_msgv3 = iv_msgv3
                                                                                                                 iv_msgv4 = iv_msgv4
                                                                                                                )
                                                                        )
                                     ).

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
        DATA(lv_exception_Text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD log_contains_messages.

    CHECK me->lo_log_handle IS BOUND.

    TRY.

        re_message = COND #( WHEN me->lo_log_handle->get_all_items( ) IS NOT INITIAL THEN abap_true
                             ELSE abap_false ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_Text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD log_exception_add.

    CHECK iv_exception IS BOUND AND me->lo_log_handle IS BOUND..

    TRY.

        me->lo_log_handle->add_item( cl_bali_exception_setter=>create( severity  = iv_severity
                                                                       exception = iv_exception ) ).

        me->add_message_internal_log( iv_msgty = iv_severity
                                      iv_msgv1 = CONV #( iv_exception->get_text( ) ) ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_Text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD log_string_add.

    CHECK me->lo_log_handle IS BOUND.

    TRY.
        me->lo_log_handle->add_item( cl_bali_free_text_setter=>create( severity = iv_msgty
                                                                       text     = CONV #( iv_string ) ) ).

        me->add_message_internal_log( iv_msgty = iv_msgty
                                      iv_msgv1 = CONV #( iv_string ) ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_Text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD log_syst_add.

    CHECK me->lo_log_handle IS BOUND.

    TRY.
        lo_log_handle->add_item( cl_bali_message_setter=>create_from_sy( ) ).

        DATA(lo_xco_message) = xco_cp=>sy->message( ).

        me->add_message_internal_log( iv_msgid = lo_xco_message->value-msgid
                                      iv_msgno = lo_xco_message->value-msgno
                                      iv_msgty = lo_xco_message->value-msgty
                                      iv_msgv1 = lo_xco_message->value-msgv1
                                      iv_msgv2 = lo_xco_message->value-msgv2
                                      iv_msgv3 = lo_xco_message->value-msgv3
                                      iv_msgv4 = lo_xco_message->value-msgv4
                                     ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_Text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_handle.

    re_handle = me->lo_log_handle->get_handle( ).

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

    DATA(lo_xco_message) = xco_cp=>sy->message( ).

    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id                    = CONV bapiret2-id( iv_msgid ) ##OPERATOR[ID]
        number                = CONV bapiret2-number( iv_msgno ) ##OPERATOR[NUMBER]
        textformat            = CONV bapitga-textformat( 'ASC' ) ##OPERATOR[TEXTFORMAT]
        message_v1            = CONV bapiret2-message_v1( iv_msgv1 ) ##OPERATOR[MESSAGE_V1]
        message_v2            = CONV bapiret2-message_v2( iv_msgv2 ) ##OPERATOR[MESSAGE_V2]
        message_v3            = CONV bapiret2-message_v3( iv_msgv3 ) ##OPERATOR[MESSAGE_V3]
        message_v4            = CONV bapiret2-message_v3( iv_msgv4 ) ##OPERATOR[MESSAGE_V4]
      IMPORTING
        message               = re_long_text
      EXCEPTIONS
        error_message         = 1
        communication_failure = 2
        OTHERS                = 3.

  ENDMETHOD.


  METHOD get_messages_as_bapiret2.

    re_bapiret2 = me->lt_bapiret2_messages.

  ENDMETHOD.


  METHOD get_message_count.

    CHECK me->lo_log_handle IS BOUND.

    TRY.
        re_count       = lines( me->lo_log_handle->get_all_items( ) ).
      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_Text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD log_bapiret2_structure_add.

    TRY.
        me->lo_log_handle->add_item( cl_bali_message_setter=>create_from_bapiret2( is_bapiret2 ) ).

        me->add_message_internal_log( iv_msgid = is_bapiret2-id
                                      iv_msgno = is_bapiret2-number
                                      iv_msgty = is_bapiret2-type
                                      iv_msgv1 = is_bapiret2-message_v1
                                      iv_msgv2 = is_bapiret2-message_v2
                                      iv_msgv3 = is_bapiret2-message_v3
                                      iv_msgv4 = is_bapiret2-message_v4 ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
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
            re_error = abap_true.
            RETURN.
          ENDIF.

        ENDLOOP.

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_Text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD log_is_empty.

    CHECK me->lo_log_handle IS BOUND.

    TRY.
        re_empty = COND #( WHEN me->lo_log_handle->get_all_items( ) IS INITIAL THEN abap_true
                           ELSE abap_false ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_Text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD log_message_add.

    CHECK me->lo_log_handle IS BOUND.

    TRY.
        me->lo_log_handle->add_item( cl_bali_message_setter=>create( severity   = iv_msgty
                                                                     id         = iv_msgid
                                                                     number     = iv_msgno
                                                                     variable_1 = iv_msgv1
                                                                     variable_2 = iv_msgv2
                                                                     variable_3 = iv_msgv3
                                                                     variable_4 = iv_msgv4 ) ).

        me->add_message_internal_log( iv_msgid = iv_msgid
                                      iv_msgno = iv_msgno
                                      iv_msgty = iv_msgty
                                      iv_msgv1 = iv_msgv1
                                      iv_msgv2 = iv_msgv2
                                      iv_msgv3 = iv_msgv3
                                      iv_msgv4 = iv_msgv4 ).

      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_Text) = lo_exception->get_text( ).
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

        CLEAR:me->lt_bapiret2_messages,me->lt_log_messages.

      CATCH cx_bali_runtime INTO lo_exception.
        lv_exception_text = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD save_application_log.

    CHECK me->lo_log_handle IS BOUND.

    TRY.
        cl_bali_log_db=>get_instance( )->save_log( me->lo_log_handle ).
      CATCH cx_bali_runtime INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
