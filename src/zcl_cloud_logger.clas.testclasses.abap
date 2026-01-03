
CLASS ltc_external_methods DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    DATA mo_log TYPE REF TO zif_cloud_logger.

    METHODS setup.
    METHODS teardown.
    METHODS test_instantiation            FOR TESTING RAISING cx_static_check.
    METHODS add_messages                  FOR TESTING RAISING cx_static_check.
    METHODS create_and_save_log           FOR TESTING RAISING cx_static_check.
    METHODS create_wrong_log              FOR TESTING RAISING cx_static_check.
    METHODS merge_logs                    FOR TESTING RAISING cx_static_check.
    METHODS check_error_in_log            FOR TESTING RAISING cx_static_check.
    METHODS check_warning_in_log          FOR TESTING RAISING cx_static_check.
    METHODS search_message_found          FOR TESTING RAISING cx_static_check.
    METHODS search_message_not_found      FOR TESTING RAISING cx_static_check.
    METHODS search_message_with_type      FOR TESTING RAISING cx_static_check.
    METHODS test_fluent_chaining          FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_external_methods IMPLEMENTATION.

  METHOD setup.

    mo_log = zcl_cloud_logger=>get_instance(
      iv_object    = 'Z_CLOUD_LOG'
      iv_subobject = 'Z_CLOUD_LOG'
      iv_db_save   = abap_false ).

    mo_log->reset_appl_log( ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_log.
  ENDMETHOD.

  METHOD test_instantiation.

    cl_abap_unit_assert=>assert_bound(
      act = mo_log
      msg = 'Logger instance should be created' ).

  ENDMETHOD.

  METHOD add_messages.

    TRY.

        mo_log->log_message_add( iv_symsg = VALUE #( msgty = 'W'
                                                        msgid = 'CL'
                                                        msgno = '000'
                                                        msgv1 = 'Test Message'
                                                        msgv2 = ''
                                                        msgv3 = ''
                                                        msgv4 = '' ) ).

        mo_log->log_bapiret2_structure_add( VALUE #( id         = 'Z_CLOUD_LOGGER'
                                                        type       = 'W'
                                                        number     = '002'
                                                        message_v1 = 'TEST' ) ).

        mo_log->log_bapiret2_table_add( VALUE #( id     = 'Z_CLOUD_LOGGER'
                                                    type   = 'W'
                                                    number = '002'
                                                    ( message_v1 = 'BAPIS' )
                                                    ( message_v1 = 'More' ) ) ).

        mo_log->log_exception_add( iv_exception = NEW cx_sy_itab_line_not_found( ) ).

        MESSAGE s003(z_aml) INTO DATA(dummy) ##NEEDED.
        mo_log->log_syst_add( ).

        mo_log->log_string_add( 'Some Freestyle text' ).

        cl_abap_unit_assert=>assert_equals( exp = 7
                                            act = mo_log->get_message_count( ) ).
        mo_log->reset_appl_log( ).

      CATCH zcx_cloud_logger_error INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD create_wrong_log.

    TRY.
        DATA(mo_log_error) = zcl_cloud_logger=>get_instance( iv_db_save = abap_true iv_object = 'Z_DUMMY_WRONG' iv_subobject = 'Z_DUMMY_WRONG' ).

        cl_abap_unit_assert=>fail( ).

      CATCH zcx_cloud_logger_error INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
        cl_abap_unit_assert=>assert_true( abap_true ).
    ENDTRY.

  ENDMETHOD.

  METHOD create_and_save_log.

    TRY.

        mo_log->log_string_add( 'Message to save' ).

        mo_log->save_application_log( ).

        cl_abap_unit_assert=>assert_true( abap_true ).

      CATCH zcx_cloud_logger_error INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD merge_logs.

    TRY.

        DATA(lo_second_logger) = zcl_cloud_logger=>get_instance( iv_object = 'Z_CLOUD_LOG' iv_subobject = 'Z_CLOUD_LOG' iv_ext_number = '1234' iv_db_save = abap_false ).

        MESSAGE e005(z_cloud_logger) INTO DATA(dummy2).
        lo_second_logger->log_syst_add( ).

        mo_log->merge_logs( lo_second_logger ).

        cl_abap_unit_assert=>assert_equals( exp = 1
                                            act = mo_log->get_message_count( ) ).


      CATCH zcx_cloud_logger_error INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.


  METHOD check_error_in_log.

    TRY.

        mo_log->log_message_add( iv_symsg = VALUE #( msgty = 'W'
                                                msgid = 'CL'
                                                msgno = '000'
                                                msgv1 = 'Test Message 1'
                                                msgv2 = ''
                                                msgv3 = ''
                                                msgv4 = '' ) ).

        mo_log->log_message_add( iv_symsg = VALUE #( msgty = 'E'
                                                        msgid = 'CL'
                                                        msgno = '000'
                                                        msgv1 = 'Test Message 2'
                                                        msgv2 = ''
                                                        msgv3 = ''
                                                        msgv4 = '' ) ).

        cl_abap_unit_assert=>assert_true( mo_log->log_contains_error( ) ).
        mo_log->reset_appl_log( ).

      CATCH zcx_cloud_logger_error INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.


  METHOD check_warning_in_log.

    TRY.

        mo_log->log_message_add( iv_symsg = VALUE #( msgty = 'W'
                                                msgid = 'CL'
                                                msgno = '000'
                                                msgv1 = 'Test Message 1'
                                                msgv2 = ''
                                                msgv3 = ''
                                                msgv4 = '' ) ).

        mo_log->log_message_add( iv_symsg = VALUE #( msgty = 'E'
                                                        msgid = 'CL'
                                                        msgno = '000'
                                                        msgv1 = 'Test Message 2'
                                                        msgv2 = ''
                                                        msgv3 = ''
                                                        msgv4 = '' ) ).

        cl_abap_unit_assert=>assert_true( mo_log->log_contains_warning( ) ).
        mo_log->reset_appl_log( ).

      CATCH zcx_cloud_logger_error INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD search_message_found.


    TRY.

        mo_log->log_message_add( iv_symsg = VALUE #( msgty = 'W'
                                                msgid = 'CL'
                                                msgno = '000'
                                                msgv1 = 'Test Message 1'
                                                msgv2 = ''
                                                msgv3 = ''
                                                msgv4 = '' ) ).

        DATA(lv_result) = mo_log->search_message( im_search = VALUE #( msgid = 'CL' ) ).

        cl_abap_unit_assert=>assert_true( lv_result ).
        mo_log->reset_appl_log( ).

      CATCH zcx_cloud_logger_error INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.


  METHOD search_message_not_found.

    TRY.

        mo_log->log_message_add( iv_symsg = VALUE #( msgty = 'W'
                                                msgid = 'CL'
                                                msgno = '000'
                                                msgv1 = 'Test Message 1'
                                                msgv2 = ''
                                                msgv3 = ''
                                                msgv4 = '' ) ).

        DATA(lv_result) = mo_log->search_message( im_search = VALUE #( msgno = '003' ) ).

        cl_abap_unit_assert=>assert_false( lv_result ).
        mo_log->reset_appl_log( ).

      CATCH zcx_cloud_logger_error INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.


  METHOD search_message_with_type.

    TRY.

        mo_log->log_message_add( iv_symsg = VALUE #( msgty = 'W'
                                                msgid = 'CL'
                                                msgno = '000'
                                                msgv1 = 'Test Message 1'
                                                msgv2 = ''
                                                msgv3 = ''
                                                msgv4 = '' ) ).

        DATA(lv_result) = mo_log->search_message( im_search = VALUE #( msgid = 'CL' msgno = '000' msgty = 'W' ) ).

        cl_abap_unit_assert=>assert_true( lv_result ).
        mo_log->reset_appl_log( ).

      CATCH zcx_cloud_logger_error INTO DATA(lo_exception).
        DATA(lv_exception_text) = lo_exception->get_text( ).
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.
  METHOD test_fluent_chaining.

    mo_log->log_string_add( 'Message 1' )->log_string_add( 'Message 2' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_log->get_message_count( )
      exp = 2
      msg = 'Should have logged 2 messages via chaining'
    ).
  ENDMETHOD.
ENDCLASS.
