*&---------------------------------------------------------------------*
*&  Include           ZFIC_049_POST_DOCS_E01
*&---------------------------------------------------------------------*

INITIALIZATION.

  p_budat   = sy-datum .
  p_mode    = 'N' .

START-OF-SELECTION.

  PERFORM get_ranges.
  PERFORM get_data .
  PERFORM process_data .
  PERFORM build_alv TABLES gt_output[].
