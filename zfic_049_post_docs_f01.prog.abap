*&---------------------------------------------------------------------*
*&  Include           ZFIC_049_POST_DOCS_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lr_all  TYPE RANGE OF blart.

  APPEND LINES OF gr_creditos TO lr_all[].
  APPEND LINES OF gr_debitos  TO lr_all[].

  IF lr_all[] IS INITIAL .
    MESSAGE 'Clases de documento Vacias Revisar ZBC_UDC' TYPE 'E' .
  ENDIF.

  SELECT kunnr belnr blart zfbdt dmbtr waers shkzg INTO TABLE gt_bsid[] FROM bsid
  WHERE blart IN lr_all[]
    AND kunnr IN s_kunnr
    AND budat <= p_budat .

ENDFORM .
*&---------------------------------------------------------------------*
*&      Form  GET_RANGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ranges .

  " *******************************************************************
  DATA : lit_range                        TYPE zttbc_range.

*    get_range 'RANGE_CLAS_DOC_ANSWE' r_bstdk .
  zcl_bc_udc_utilities=>get_range(
    EXPORTING
      im_progname = sy-repid    " Nombre de programa ABAP
      im_range    = 'DOC_DEBITOS' " Identificador de Rango
    IMPORTING
      ex_range  = lit_range " BC : Estructura generica para rangos
                                     ).

  gr_debitos = lit_range[].
  " *******************************************************************
  " *******************************************************************
  CLEAR lit_range[].

*    get_range 'RANGE_CLAS_DOC_ANSWE' r_bstdk .
  zcl_bc_udc_utilities=>get_range(
    EXPORTING
      im_progname = sy-repid    " Nombre de programa ABAP
      im_range    = 'DOC_CREDITOS' " Identificador de Rango
    IMPORTING
      ex_range  = lit_range " BC : Estructura generica para rangos
  ).

  gr_creditos = lit_range[].
  " *******************************************************************

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data .

  DATA: lt_bsid_kunnr LIKE gt_bsid,
        ls_bsid_kunnr LIKE LINE OF lt_bsid_kunnr[],
        lt_return     TYPE TABLE OF bapiret2.

  SORT gt_bsid BY kunnr zfbdt ASCENDING .

  LOOP AT gt_bsid ASSIGNING FIELD-SYMBOL(<ls_bsid>).

    APPEND <ls_bsid> TO lt_bsid_kunnr[] .

    AT END OF kunnr .
      IF lt_bsid_kunnr[] IS INITIAL .
        PERFORM add_row_output USING <ls_bsid>-kunnr space 'No se encontraron datos a procesar' .
      ELSE.
        PERFORM bdc_f32 TABLES lt_bsid_kunnr[] lt_return[] USING <ls_bsid> <ls_bsid>-kunnr .
      ENDIF.

      CLEAR: lt_bsid_kunnr[], lt_return[] .
*      PERFORM call_bapi_billing_post TABLES lt_bsid_kunnr[] USING <ls_bsid> .
    ENDAT.
  ENDLOOP.

  IF gt_bsid[] IS INITIAL .
    PERFORM add_row_output USING space space 'No se encontraron datos a procesar' .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi_billing_post TABLES ct_bsid LIKE gt_bsid[]
                      USING us_bsid LIKE LINE OF gt_bsid[].
*
*  " *******************************************************************
**  DATA: lt_return     TYPE bapiret2_t .
**
**  CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
**    TABLES
**      imt_bdcmsgcoll = lt_mestab[]
**      ext_return     = lt_return[].
*
*  DATA: ls_header      TYPE bapiache01 .
*
*  DATA: lt_recibe  TYPE TABLE OF bapiacar01,
*        lt_gl      TYPE TABLE OF bapiacgl01,
*        lt_taxes   TYPE TABLE OF bapiactx01,
*        lt_valores TYPE TABLE OF bapiaccr01,
*        lt_return  TYPE TABLE OF bapiret2.
*
*  LOOP AT ct_bsid ASSIGNING FIELD-SYMBOL(<ls_bsid>).
*
*    IF <ls_bsid>-blart IN gr_creditos . "Cuenta x Cobrarle
*      accountreceivable-itemno_acc = '1'.
*      accountreceivable-customer = '100193'. "Customer number
*      accountreceivable-item_text = 'TEXTO'. "Item Text
**       wa_ACCOUNTRECEIVABLE-GL_ACCOUNT = '7690000001'.
*
*      APPEND accountreceivable." TO accountreceivable.
*
*    ELSEIF <ls_bsid>-blart IN gr_debitos. "Abono/Saldo a Favor
*
*    ENDIF.
*
*  ENDLOOP.
*
*
*  CALL FUNCTION 'BAPI_ACC_BILLING_POST'
*    EXPORTING
*      documentheader    = ls_header
**     CUSTOMERCPD       =
**   IMPORTING
**     OBJ_TYPE          =
**     OBJ_KEY           =
**     OBJ_SYS           =
*    TABLES
*      accountreceivable = lt_recibe[]
*      accountgl         = lt_gl[]
*      accounttax        = lt_taxes[]
**     CRITERIA          =
**     VALUEFIELD        =
*      currencyamount    = lt_valores[]
*      return            = lt_return[]
**     SALESORDER        =
**     SALESAMOUNT       =
**     EXTENSION1        =
*    .



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LS_BSID>  text
*----------------------------------------------------------------------*
FORM bdc_f32 TABLES ct_bsid LIKE gt_bsid[]
                    lt_return STRUCTURE bapiret2
                      USING us_bsid LIKE LINE OF gt_bsid[]
                            ud_kunnr .

  DATA: ld_today           TYPE d,
        lt_debitos         LIKE gt_bsid[],
        lt_creditos        LIKE gt_bsid[],
        ld_total_abon      LIKE bsid-dmbtr,
        ld_total_cargos    LIKE bsid-dmbtr,
        ld_subtotal_abon   LIKE bsid-dmbtr,
        ld_subtotal_cargos LIKE bsid-dmbtr.

  DATA:         ld_last_doc   TYPE belnr_d.


  lt_debitos[] = lt_creditos[] = ct_bsid[] .

  "BEGINOF: MPACHECO: 10.04.2017 09:50:35
  "Obser:  Abonos
  DELETE lt_debitos WHERE blart IN gr_creditos .
  "ENDOF: MPACHECO: 10.04.2017 09:50:43

  "BEGINOF: MPACHECO: 10.04.2017 09:50:51
  "Obser: Creditos
  DELETE lt_creditos WHERE blart IN gr_debitos .
  "ENDOF: MPACHECO: 10.04.2017 09:50:53

  BREAK con_abap1.

  IF lt_debitos IS INITIAL .
    PERFORM add_row_output USING ud_kunnr space 'No se encontraron Abonos para contabilización' .
    RETURN.
  ELSEIF lt_creditos[] IS INITIAL .
    PERFORM add_row_output USING ud_kunnr space 'No se encontro Deuda para aplicación' .
    RETURN.
  ELSE.
    "BEGINOF: MPACHECO: 27.05.2017 08:51:08
    "Obser: Encontrar Limite Debitos O Creditos Antes de ct_bsid[]
    FIELD-SYMBOLS <ls_debito> LIKE LINE OF lt_debitos[].
    FIELD-SYMBOLS <ls_credito> LIKE LINE OF lt_creditos[].

    CLEAR ct_bsid[].
    " *******************************************************************
    SORT lt_debitos BY zfbdt ASCENDING.
    LOOP AT lt_debitos[] ASSIGNING <ls_debito>.
      ld_total_abon = ld_total_abon + <ls_debito>-dmbtr .
    ENDLOOP.
    " *******************************************************************
    SORT lt_creditos BY zfbdt ASCENDING.
    LOOP AT lt_creditos[] ASSIGNING <ls_credito>.
      ld_total_cargos = ld_total_cargos + <ls_credito>-dmbtr.
    ENDLOOP.
    " *******************************************************************
    "Append Lines Debitos
    LOOP AT lt_debitos[] ASSIGNING <ls_debito>.
      ld_subtotal_abon = ld_subtotal_abon + <ls_debito>-dmbtr .
      APPEND <ls_debito> TO ct_bsid[] .

      ld_last_doc = <ls_debito>-belnr.

      IF ld_subtotal_abon > ld_total_cargos AND sy-tabix <> 1 .
        EXIT .
      ENDIF.
    ENDLOOP.

    " *******************************************************************
    "Append Lines Creditos
    LOOP AT lt_creditos[] ASSIGNING <ls_credito>.
      ld_subtotal_cargos = ld_subtotal_cargos + <ls_credito>-dmbtr .
      APPEND <ls_credito> TO ct_bsid[] .

      IF ld_subtotal_cargos > ld_total_abon AND sy-tabix <> 1.
        DELETE ct_bsid[] WHERE belnr = <ls_credito>-belnr .
        ld_subtotal_cargos = ld_subtotal_cargos - <ls_credito>-dmbtr.
        EXIT .
      ENDIF.
    ENDLOOP.

    " *******************************************************************
    "ENDOF: MPACHECO: 27.05.2017 08:51:20
  ENDIF.

  "BEGINOF: MPACHECO: 12.06.2017 10:52:54
  "Obser: Validación no dispara compensación
  IF ld_subtotal_cargos > ld_total_abon.
    PERFORM add_row_output USING ud_kunnr space 'Los Cargos son mayores que los abonos, No se lanza compensación' .
    RETURN.
  ENDIF.
  "ENDOF: MPACHECO: 12.06.2017 10:53:26

*  lo_bdc->bdc_dynpro( EXPORTING program = program dynpro  = dynpro ).

  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
    EXPORTING
      input  = p_budat "sy-datum
    IMPORTING
      output = ld_today.

*  BREAK con_abap1.

  PERFORM bdc_dynpro      USING 'SAPMF05A' '0131'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'RF05A-AGKON'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field       USING 'RF05A-AGKON' us_bsid-kunnr. "Cuenta
  PERFORM bdc_field       USING 'BKPF-BUDAT' ld_today.
  PERFORM bdc_field       USING 'BKPF-MONAT' sy-datum+4(2).
  PERFORM bdc_field       USING 'BKPF-BUKRS' '1000'.
  PERFORM bdc_field       USING 'BKPF-WAERS' us_bsid-waers.
  PERFORM bdc_field       USING 'RF05A-AGUMS'	'ACEF'.
  PERFORM bdc_field       USING 'RF05A-XNOPS' 'X'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=OMX'. "'=OSU'.
*  PERFORM bdc_field       USING 'BDC_CURSOR' 'DF05B-PSSKT(01)'.
  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=Z-'.
*  perform bdc_field       using 'BDC_CURSOR' 'DF05B-PSSKT(01)'.
  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=OSU'.
*  perform bdc_field       using 'BDC_CURSOR' 'DF05B-PSSKT(01)'.
  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '2000'.
*  PERFORM bdc_field       USING 'BDC_CURSOR' 'RF05A-XPOS1(03)'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=GO'.
  PERFORM bdc_field       USING 'RF05A-XPOS1(01)' space.
  PERFORM bdc_field       USING 'RF05A-XPOS1(03)' 'X'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '0731'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'RF05A-SEL01(03)'.

  "BEGINOF: MPACHECO: 09.04.2017 12:52:44
  "Obser: Calculate Documents
  DESCRIBE TABLE ct_bsid[] LINES DATA(ld_lines).
  DATA: ld_line(070)  TYPE c,
        ld_tabix(003) TYPE c.

  LOOP AT ct_bsid ASSIGNING FIELD-SYMBOL(<ls_bsid>).

    ld_tabix = sy-tabix.

    IF ( sy-tabix MOD 7 = 1 AND sy-tabix > 7  ) .
      PERFORM bdc_field       USING 'BDC_OKCODE' '=SU1'.
      PERFORM bdc_dynpro      USING 'SAPDF05X' '0731'.
      PERFORM bdc_field       USING 'BDC_CURSOR' 'RF05A-SEL01(03)'.
    ENDIF.

*    IF <ls_bsid>-shkzg = 'H'. "Totalizar Abono
*      ld_total_abon = ld_total_abon + <ls_bsid>-dmbtr .
*    ELSEIF <ls_bsid>-shkzg = 'S'.
*      ld_total_cargos = ld_total_cargos + <ls_bsid>-dmbtr .
*    ENDIF.

    IF ld_tabix > 7 .
      ld_tabix = ( ld_tabix MOD 7 ) + 1 .
    ENDIF.

    CONDENSE ld_tabix NO-GAPS .
    CONCATENATE 'RF05A-SEL01(0' ld_tabix ')' INTO ld_line .
    PERFORM bdc_field       USING ld_line <ls_bsid>-belnr.

*    IF ld_total_cargos >= ld_total_abon .
*      EXIT.
*    ENDIF.

    CLEAR ld_line.

*  PERFORM bdc_field       USING 'RF05A-SEL01(01)' record-sel01_01_010.
*  PERFORM bdc_field       USING 'RF05A-SEL01(02)' record-sel01_02_011.
*  PERFORM bdc_field       USING 'RF05A-SEL01(03)' record-sel01_03_012.
  ENDLOOP.


  PERFORM bdc_field       USING 'BDC_OKCODE' '=GO'.
  "ENDOF: MPACHECO: 09.04.2017 12:52:53


  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=OMX'.
*  PERFORM bdc_field       USING 'BDC_CURSOR' 'DF05B-PSSKT(01)'.
  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=Z+'.
*  PERFORM bdc_field       USING 'BDC_CURSOR' 'DF05B-PSSKT(01)'.
  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.

*  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
*  PERFORM bdc_field       USING 'BDC_OKCODE' '=OMX'.
*  PERFORM bdc_field       USING 'BDC_CURSOR' 'DF05B-PSSKT(01)'.
*  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.
*
*  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
*  PERFORM bdc_field       USING 'BDC_OKCODE' '=Z+'.
*  PERFORM bdc_field       USING 'BDC_CURSOR' 'DF05B-PSSKT(01)'.
*  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.

  "BEGINOF: MPACHECO: 26.04.2017 11:48:08
  "Obser: Insert New Record
  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=REST'.
*  perform bdc_field       using 'BDC_CURSOR' 'DF05B-PSSKT(01)'.
  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=OSU'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'DF05B-PSDIF(01)'.
  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '2000'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'RF05A-XPOS1(03)'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=GO'.
  PERFORM bdc_field       USING 'RF05A-XPOS1(01)' space.
  PERFORM bdc_field       USING 'RF05A-XPOS1(03)' 'X'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '0731'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'RF05A-SEL01(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=GO'.
  PERFORM bdc_field       USING 'RF05A-SEL01(01)' ld_last_doc.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=PI'.
*  perform bdc_field       using 'BDC_CURSOR' 'DF05B-PSDIF(05)'.
  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=AB'.
*  perform bdc_field       using 'BDC_CURSOR' 'DF05B-PSDIF(05)'.
  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.
  "ENDOF: MPACHECO: 26.04.2017 11:48:13

*  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
*  PERFORM bdc_field       USING 'BDC_OKCODE' '=PART'.
*  PERFORM bdc_field       USING 'BDC_CURSOR' 'DF05B-PSSKT(01)'.
*  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.
*
*  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
*  PERFORM bdc_field       USING 'BDC_OKCODE' '=PI'.
*  PERFORM bdc_field       USING 'BDC_CURSOR' 'DF05B-PSZAH(01)'.
*  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.
*
*  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
*  PERFORM bdc_field       USING 'BDC_OKCODE' '=AB'.
*  PERFORM bdc_field       USING 'BDC_CURSOR' 'DF05B-PSZAH(01)'.
*  PERFORM bdc_field       USING 'RF05A-ABPOS' '1'.

  PERFORM bdc_dynpro      USING 'SAPMF05A' '0700'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'RF05A-NEWBS'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=BU'.

  "BEGINOF: MPACHECO: 26.04.2017 11:50:53
  "Obser: Include new record
  PERFORM bdc_dynpro      USING 'SAPMF05A' '0700'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'RF05A-AZEI1(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=PI'.

  PERFORM bdc_dynpro      USING 'SAPMF05A' '0301'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'BSEG-SGTXT'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=BU'.
*  perform bdc_field       using 'BSEG-HKONT' record-HKONT_025.
*  perform bdc_field       using 'BSEG-ZTERM' record-ZTERM_026.
*  perform bdc_field       using 'BSEG-ZFBDT' record-ZFBDT_027.
*  perform bdc_field       using 'BSEG-ZUONR' record-ZUONR_028.
  PERFORM bdc_field       USING 'BSEG-SGTXT' 'Saldo Factura Compensación'.
  "ENDOF: MPACHECO: 26.04.2017 11:50:58

*  PERFORM bdc_dynpro      USING 'SAPMF05A' '0700'.
*  PERFORM bdc_field       USING 'BDC_CURSOR' 'RF05A-NEWBS'.
*  PERFORM bdc_field       USING 'BDC_OKCODE' '=BU'.

  opt-updmode = 'S'.
  opt-nobinpt = 'X'.
*  ls_opt-nobinpt = 'X'.
*  opt-dismode = 'A'.
*  ls_opt-nobiend = abap_true.
*  ls_opt-racommit = abap_true.
*  ls_opt-defsize =  abap_true.

  PERFORM bdc_transaction3 USING 'F-32' ud_kunnr .

*  CALL TRANSACTION 'F-32' USING bdcdata OPTIONS FROM ls_opt MESSAGES INTO messtab.

*  CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
*    TABLES
*      imt_bdcmsgcoll = messtab[]
*      ext_return     = lt_return[].
*
*  IF line_exists( lt_return[ type = 'S' number = '312' ] ).
*    DELETE lt_return WHERE NOT ( type = 'S' AND number = '312' ) .
*  ENDIF.

ENDFORM.
