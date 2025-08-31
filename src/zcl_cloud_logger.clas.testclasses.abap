
CLASS ltc_external_methods DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS add_messages                  FOR TESTING RAISING cx_static_check.
    METHODS create_and_save_log           FOR TESTING RAISING cx_static_check.
    METHODS merge_logs                    FOR TESTING RAISING cx_static_check.
    METHODS check_error_in_log            FOR TESTING RAISING cx_static_check.
    METHODS check_warning_in_log          FOR TESTING RAISING cx_static_check.
    METHODS save_log_not_accessible       FOR TESTING RAISING cx_static_check.
    METHODS search_message_found          FOR TESTING RAISING cx_static_check.
    METHODS search_message_not_found      FOR TESTING RAISING cx_static_check.
    METHODS search_message_with_type      FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_external_methods IMPLEMENTATION.

  METHOD add_messages.

    TRY.
        DATA(lo_logger) = zcl_cloud_logger=>get_instance( iv_db_save = abap_false ).

        lo_logger->log_message_add( iv_msgty = 'W'
                                    iv_msgid = 'CL'
                                    iv_msgno = '000'
                                    iv_msgv1 = 'Messages for WBS:'
                                    iv_msgv2 = 'KALISPERA'
                                    iv_msgv3 = ''
                                    iv_msgv4 = '' ).

        lo_logger->log_bapiret2_structure_add( VALUE #( id         = 'Z_AML'
                                    type       = 'W'
                                    number     = '002'
                                    message_v1 = 'TEST' ) ).

        lo_logger->log_bapiret2_table_add( VALUE #( id     = 'Z_AML'
                                                    type   = 'W'
                                                    number = '002'
                                                    ( message_v1 = 'BAPIS' )
                                                    ( message_v1 = 'More' ) ) ).

        lo_logger->log_exception_add( iv_exception = NEW cx_sy_itab_line_not_found( ) ).

        MESSAGE s003(z_aml) INTO DATA(dummy) ##NEEDED.
        lo_logger->log_syst_add( ).

        lo_logger->log_string_add( 'Some Freestyle text' ).

        cl_abap_unit_assert=>assert_equals( exp = 7
                                            act = lo_logger->get_message_count( ) ).
        lo_logger->reset_appl_log( ).

      CATCH zcx_cloud_logger_error.
    ENDTRY.

  ENDMETHOD.


  METHOD create_and_save_log.

    TRY.
        DATA(lo_logger) = zcl_cloud_logger=>get_instance( iv_db_save = abap_true iv_object = 'APLO_TEST' iv_subobject = 'SUBOBJECT1' ).

        lo_logger->log_string_add( 'Message to save' ).

        lo_logger->save_application_log( ).

        cl_abap_unit_assert=>assert_true( abap_true ).

      CATCH zcx_cloud_logger_error.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD merge_logs.

    TRY.
        DATA(lo_first_logger) = zcl_cloud_logger=>get_instance( iv_ext_number = '123' iv_db_save = abap_false ).

        DATA(lo_second_logger) = zcl_cloud_logger=>get_instance( iv_ext_number = '1234' iv_db_save = abap_false ).

        MESSAGE e007(00) WITH 'Merge' INTO DATA(dummy2).
        lo_second_logger->log_syst_add( ).

        lo_first_logger->merge_logs( lo_second_logger ).

        cl_abap_unit_assert=>assert_equals( exp = 1
                                            act = lo_first_logger->get_message_count( ) ).


      CATCH zcx_cloud_logger_error.
    ENDTRY.

  ENDMETHOD.


  METHOD check_error_in_log.

    TRY.
        DATA(lo_logger) = zcl_cloud_logger=>get_instance( iv_db_save = abap_false ).

        lo_logger->log_message_add(   iv_msgty = 'W'
                                      iv_msgid = 'CL'
                                      iv_msgno = '000'
                                      iv_msgv1 = 'Messages for WBS:'
                                      iv_msgv2 = 'KALISPERA'
                                      iv_msgv3 = ''
                                      iv_msgv4 = '' ).

        lo_logger->log_message_add(   iv_msgty = 'E'
                                      iv_msgid = 'CL'
                                      iv_msgno = '000'
                                      iv_msgv1 = 'Messages for WBS:'
                                      iv_msgv2 = 'KALISPERA'
                                      iv_msgv3 = ''
                                      iv_msgv4 = '' ).

        cl_abap_unit_assert=>assert_true( lo_logger->log_contains_error( ) ).
        lo_logger->reset_appl_log( ).

      CATCH zcx_cloud_logger_error.
    ENDTRY.

  ENDMETHOD.


  METHOD check_warning_in_log.

    TRY.
        DATA(lo_logger) = zcl_cloud_logger=>get_instance( iv_db_save = abap_false ).

        lo_logger->log_message_add(   iv_msgty = 'W'
                                      iv_msgid = 'CL'
                                      iv_msgno = '000'
                                      iv_msgv1 = 'Messages for WBS:'
                                      iv_msgv2 = 'KALISPERA'
                                      iv_msgv3 = ''
                                      iv_msgv4 = '' ).

        lo_logger->log_message_add(   iv_msgty = 'E'
                                      iv_msgid = 'CL'
                                      iv_msgno = '000'
                                      iv_msgv1 = 'Messages for WBS:'
                                      iv_msgv2 = 'KALISPERA'
                                      iv_msgv3 = ''
                                      iv_msgv4 = '' ).

        cl_abap_unit_assert=>assert_true( lo_logger->log_contains_warning( ) ).
        lo_logger->reset_appl_log( ).

      CATCH zcx_cloud_logger_error.
    ENDTRY.

  ENDMETHOD.


  METHOD save_log_not_accessible.

    TRY.
        DATA(lo_logger) = zcl_cloud_logger=>get_instance( iv_db_save = abap_true iv_object = 'APLO_TEST1' iv_subobject = 'SUBOBJECT1' ).

        lo_logger->log_string_add( 'Message to save' ).

        lo_logger->save_application_log( ).

        cl_abap_unit_assert=>fail( ).

        lo_logger->reset_appl_log( ).

      CATCH zcx_cloud_logger_error.
        cl_abap_unit_assert=>assert_true( abap_true ).
    ENDTRY.

  ENDMETHOD.


  METHOD search_message_found.


    TRY.
        DATA(lo_logger) = zcl_cloud_logger=>get_instance( iv_db_save = abap_false ).

        lo_logger->log_message_add(   iv_msgty = 'W'
                                      iv_msgid = 'CL'
                                      iv_msgno = '000'
                                      iv_msgv1 = 'Messages for WBS:'
                                      iv_msgv2 = 'KALISPERA'
                                      iv_msgv3 = ''
                                      iv_msgv4 = '' ).

        DATA(lv_result) = lo_logger->search_message( im_search = VALUE #( msgid = 'CL' msgno = '000' ) ).

        cl_abap_unit_assert=>assert_true( lv_result ).
        lo_logger->reset_appl_log( ).

      CATCH zcx_cloud_logger_error.
    ENDTRY.

  ENDMETHOD.


  METHOD search_message_not_found.

    TRY.
        DATA(lo_logger) = zcl_cloud_logger=>get_instance( iv_db_save = abap_false ).

        lo_logger->log_message_add(   iv_msgty = 'W'
                                      iv_msgid = 'CL'
                                      iv_msgno = '000'
                                      iv_msgv1 = 'Messages for WBS:'
                                      iv_msgv2 = 'KALISPERA'
                                      iv_msgv3 = ''
                                      iv_msgv4 = '' ).

        DATA(lv_result) = lo_logger->search_message( im_search = VALUE #( msgid = 'CL' msgno = '003' ) ).

        cl_abap_unit_assert=>assert_false( lv_result ).
        lo_logger->reset_appl_log( ).

      CATCH zcx_cloud_logger_error.
    ENDTRY.

  ENDMETHOD.


  METHOD search_message_with_type.

    TRY.
        DATA(lo_logger) = zcl_cloud_logger=>get_instance( iv_db_save = abap_false ).

        lo_logger->log_message_add(   iv_msgty = 'W'
                                      iv_msgid = 'CL'
                                      iv_msgno = '000'
                                      iv_msgv1 = 'Messages for WBS:'
                                      iv_msgv2 = 'KALISPERA'
                                      iv_msgv3 = ''
                                      iv_msgv4 = '' ).

        DATA(lv_result) = lo_logger->search_message( im_search = VALUE #( msgid = 'CL' msgno = '000' msgty = 'W' ) ).

        cl_abap_unit_assert=>assert_true( lv_result ).
        lo_logger->reset_appl_log( ).

      CATCH zcx_cloud_logger_error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
