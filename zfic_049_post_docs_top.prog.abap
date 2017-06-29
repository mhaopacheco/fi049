*&---------------------------------------------------------------------*
*&  Include           ZFIC_049_POST_DOCS_TOP
*&---------------------------------------------------------------------*
REPORT zfic_049_post_docs.

INCLUDE zfic_049_bdcrecx1. " Data BDC

" Type's ***********************************************************
TYPES : BEGIN OF ty_bsid,
          kunnr TYPE bsid-kunnr,
          belnr TYPE bsid-belnr,
          blart TYPE bsid-blart,
          zfbdt TYPE bsid-zfbdt,
          dmbtr TYPE bsid-dmbtr,
          waers TYPE bsid-waers,
          shkzg TYPE bsid-shkzg,
        END OF ty_bsid.

TYPES : BEGIN OF ty_output ,
          kunnr   TYPE kna1-kunnr,
          belnr   TYPE bkpf-belnr,
          message TYPE bapiret2-message,
        END OF ty_output.

" Range's ************************************************************
DATA: gr_creditos TYPE RANGE OF blart,
      gr_debitos  TYPE RANGE OF blart.

" Data's ************************************************************
DATA: gt_bsid     TYPE STANDARD TABLE OF ty_bsid,
      gs_ax_bsid  LIKE LINE OF gt_bsid,
      gt_output   TYPE STANDARD TABLE OF ty_output,
      gs_output   LIKE LINE OF gt_output[].

" *******************************************************************"
* SELECTION-SCREEN
" *******************************************************************

SELECTION-SCREEN BEGIN OF BLOCK bq001 WITH FRAME TITLE t00 .
  PARAMETERS: p_budat TYPE budat .
  SELECT-OPTIONS s_kunnr  FOR gs_ax_bsid-kunnr NO INTERVALS .
SELECTION-SCREEN END OF BLOCK bq001.
