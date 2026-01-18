class ZCL_CLOUD_LOGGER_VIEW_ALV definition
  public
  final
  create public .

public section.

  interfaces ZIF_CLOUD_LOGGER_VIEWER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CLOUD_LOGGER_VIEW_ALV IMPLEMENTATION.


  METHOD zif_cloud_logger_viewer~view.

    DATA(lt_bapiret) = io_logger->get_messages_as_bapiret2( ).
    CHECK lt_bapiret IS NOT INITIAL.

    TRY.

        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                                CHANGING  t_table      = lt_bapiret ).

        lo_alv->set_screen_popup(
          start_column = 10
          end_column   = 120
          start_line   = 5
          end_line     = 25 ).

        lo_alv->get_columns( )->set_optimize( abap_true ).
        lo_alv->display( ).

      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
