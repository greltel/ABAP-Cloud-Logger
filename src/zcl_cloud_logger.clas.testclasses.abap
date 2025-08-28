
CLASS ltc_external_methods DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS stacked_message               FOR TESTING RAISING cx_static_check.
    METHODS stacked_message_without_stack FOR TESTING RAISING cx_static_check.
    METHODS handle_is_filled              FOR TESTING RAISING cx_static_check.
    METHODS add_messages                  FOR TESTING RAISING cx_static_check.
    METHODS create_and_save_log           FOR TESTING RAISING cx_static_check.
    METHODS get_formatted_logs            FOR TESTING RAISING cx_static_check.
    METHODS default_values                FOR TESTING RAISING cx_static_check.
    METHODS merge_three_logs              FOR TESTING RAISING cx_static_check.
    METHODS check_error_in_log            FOR TESTING RAISING cx_static_check.
    METHODS check_warning_in_log          FOR TESTING RAISING cx_static_check.
    METHODS save_log_not_accessible       FOR TESTING RAISING cx_static_check.
    METHODS search_message_found          FOR TESTING RAISING cx_static_check.
    METHODS search_message_not_found      FOR TESTING RAISING cx_static_check.
    METHODS search_message_with_type      FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_external_methods IMPLEMENTATION.
  METHOD stacked_message.
    DATA(cut) = zcl_aml_log_factory=>create( ).

    DATA(last_exception) = NEW cx_sy_itab_line_not_found( ).
    DATA(middle_exception) = NEW cx_abap_api_state( previous = last_exception ).
    DATA(main_exception) = NEW cx_abap_datfm( previous = middle_exception ).

    cut->add_message_exception( main_exception ).

    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = cut->get_number_of_messages( ) ).
  ENDMETHOD.


  METHOD stacked_message_without_stack.
    DATA(cut) = zcl_aml_log_factory=>create( VALUE #( no_stacked_exception = abap_true ) ).

    DATA(last_exception) = NEW cx_sy_itab_line_not_found( ).
    DATA(middle_exception) = NEW cx_abap_api_state( previous = last_exception ).
    DATA(main_exception) = NEW cx_abap_datfm( previous = middle_exception ).

    cut->add_message_exception( main_exception ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = cut->get_number_of_messages( ) ).
  ENDMETHOD.


  METHOD handle_is_filled.
    DATA(cut) = zcl_aml_log_factory=>create( ).

    DATA(result) = cut->get_log_handle( ).

    cl_abap_unit_assert=>assert_not_initial( result ).
  ENDMETHOD.


  METHOD add_messages.
    DATA(cut) = zcl_aml_log_factory=>create( ).

    cut->add_message( class  = 'Z_AML'
                      number = '001'
                      v1     = 'One'
                      v2     = 'More'
                      v3     = 'Time' ).
    cut->add_message_bapi( VALUE #( id         = 'Z_AML'
                                    type       = 'W'
                                    number     = '002'
                                    message_v1 = 'TEST' ) ).
    cut->add_message_bapis( VALUE #( id     = 'Z_AML'
                                     type   = 'W'
                                     number = '002'
                                     ( message_v1 = 'BAPIS' )
                                     ( message_v1 = 'More' ) ) ).
    cut->add_message_exception( NEW cx_sy_itab_line_not_found( ) ).

    MESSAGE s003(z_aml) INTO DATA(dummy) ##NEEDED.
    cut->add_message_system( ).

    cut->add_message_text( 'Some Freestyle text' ).

    cl_abap_unit_assert=>assert_equals( exp = 7
                                        act = cut->get_number_of_messages( ) ).
  ENDMETHOD.


  METHOD create_and_save_log.
    DATA(cut) = zcl_aml_log_factory=>create( VALUE #( object    = 'Z_AML_LOG'
                                                      subobject = 'TEST' ) ).
    cut->add_message_text( 'Message to save' ).

    DATA(result) = cut->save( ).

    cl_abap_unit_assert=>assert_true( result-saved ).
    cl_abap_unit_assert=>assert_not_initial( result-handle ).
    cl_abap_unit_assert=>assert_initial( result-message ).
  ENDMETHOD.


  METHOD get_formatted_logs.
    DATA(cut) = zcl_aml_log_factory=>create( ).

    MESSAGE s003(z_aml) INTO DATA(dummy) ##NEEDED.
    cut->add_message_system( ).

    cut->add_message_text( 'Some Freestyle text' ).

    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( cut->get_messages( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( cut->get_messages_bapi( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( cut->get_messages_flat( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( cut->get_messages_rap( ) ) ).
  ENDMETHOD.


  METHOD default_values.
    DATA(cut) = zcl_aml_log_factory=>create( VALUE #( default_message_class = 'Z_AML'
                                                      default_message_type  = 'W' ) ).
    cut->add_message( '000' ).

    DATA(messages) = cut->get_messages( ).
    DATA(result) = messages[ 1 ].

    cl_abap_unit_assert=>assert_equals( exp = 'W'
                                        act = result-message-msgty ).
    cl_abap_unit_assert=>assert_equals( exp = 'Z_AML'
                                        act = result-message-msgid ).
  ENDMETHOD.


  METHOD merge_three_logs.
    DATA(cut) = zcl_aml_log_factory=>create( ).

    DATA(log_with_one_message) = zcl_aml_log_factory=>create( VALUE #( default_message_class = 'Z_AML'
                                                                       default_message_type  = 'I' ) ).
    log_with_one_message->add_message( '000' ).

    DATA(log_with_two_message) = zcl_aml_log_factory=>create( ).

    MESSAGE s003(z_aml) INTO DATA(dummy) ##NEEDED.
    log_with_two_message->add_message_system( ).

    log_with_two_message->add_message_text( 'Dummy Text' ).

    cut->merge_log( log_with_one_message ).
    cut->merge_log( log_with_two_message ).

    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = cut->get_number_of_messages( ) ).
  ENDMETHOD.


  METHOD check_error_in_log.
    DATA(cut) = zcl_aml_log_factory=>create( ).

    cut->add_message_text( type = 'S'
                           text = 'Dummy Text' ).
    cl_abap_unit_assert=>assert_false( cut->has_error( ) ).

    cut->add_message_text( type = 'E'
                           text = 'Error Message' ).
    cl_abap_unit_assert=>assert_true( cut->has_error( ) ).
  ENDMETHOD.


  METHOD check_warning_in_log.
    DATA(cut) = zcl_aml_log_factory=>create( ).

    cut->add_message_text( type = 'W'
                           text = 'Dummy Text' ).
    cl_abap_unit_assert=>assert_true( cut->has_warning( ) ).

    cut->add_message_text( type = 'E'
                           text = 'Error Message' ).
    cl_abap_unit_assert=>assert_true( cut->has_warning( ) ).
  ENDMETHOD.


  METHOD save_log_not_accessible.
    DATA(cut) = zcl_aml_log_factory=>create( VALUE #( object    = 'APLO_TEST'
                                                      subobject = 'SUBOBJECT1' ) ).
    cut->add_message_text( 'Message to save' ).

    TRY.
        cut->save( ).

        cl_abap_unit_assert=>fail( ).

      CATCH zcx_aml_error.
        cl_abap_unit_assert=>assert_true( abap_true ).
    ENDTRY.
  ENDMETHOD.


  METHOD search_message_found.
    DATA(cut) = zcl_aml_log_factory=>create( ).
    cut->add_message( class  = 'Z_AML'
                      number = '001'
                      v1     = 'One'
                      v2     = 'More'
                      v3     = 'Time' ).
    cut->add_message( class  = 'Z_AML'
                      number = '003' ).

    DATA(result) = cut->search_message( VALUE #( msgid = 'Z_AML'
                                                 msgno = '003' ) ).

    cl_abap_unit_assert=>assert_true( result-found ).
  ENDMETHOD.


  METHOD search_message_not_found.
    DATA(cut) = zcl_aml_log_factory=>create( ).
    cut->add_message( class  = 'Z_AML'
                      number = '001'
                      v1     = 'One'
                      v2     = 'More'
                      v3     = 'Time' ).
    cut->add_message( class  = 'Z_AML'
                      number = '003' ).

    DATA(result) = cut->search_message( VALUE #( msgid = 'Z_AML'
                                                 msgno = '002' ) ).

    cl_abap_unit_assert=>assert_false( result-found ).
  ENDMETHOD.


  METHOD search_message_with_type.
    DATA(cut) = zcl_aml_log_factory=>create( ).
    cut->add_message( class  = 'Z_AML'
                      number = '001'
                      type   = 'E'
                      v1     = 'One'
                      v2     = 'More'
                      v3     = 'Time' ).
    cut->add_message( class  = 'Z_AML'
                      number = '003'
                      type   = 'W' ).

    DATA(result) = cut->search_message( VALUE #( msgid = 'Z_AML'
                                                 msgty = 'W' ) ).

    cl_abap_unit_assert=>assert_true( result-found ).
  ENDMETHOD.
ENDCLASS.
