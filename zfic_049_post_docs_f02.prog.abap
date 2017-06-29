*----------------------------------------------------------------------*
***INCLUDE ZFIC_049_POST_DOCS_F02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BUILD_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_alv TABLES ct_output STRUCTURE gs_output.

  DATA: salv_tab_ctrl  TYPE REF TO cl_salv_table,
        salv_exception TYPE REF TO cx_salv_msg.

  TRY.
      cl_salv_table=>factory(
            IMPORTING
              r_salv_table = salv_tab_ctrl
            CHANGING
              t_table      = gt_output[]
          ).

      salv_tab_ctrl->get_display_settings( )->set_striped_pattern( abap_true ).
      salv_tab_ctrl->get_columns( )->set_optimize( abap_true ).
      salv_tab_ctrl->get_functions( )->set_all( abap_true ).

      salv_tab_ctrl->display( ).

    CATCH cx_salv_msg INTO salv_exception.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_ROW_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_0242   text
*----------------------------------------------------------------------*
FORM add_row_output   USING   ud_kunnr
                              ud_belnr
                              ud_message .

  gs_output-kunnr = ud_kunnr .
  gs_output-belnr = ud_belnr .
  gs_output-message = ud_message .
  APPEND gs_output TO gt_output[] .

ENDFORM.
