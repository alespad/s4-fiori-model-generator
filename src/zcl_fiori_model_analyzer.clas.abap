class ZCL_FIORI_MODEL_ANALYZER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_app,
             fiori_id          TYPE string,
             app_name          TYPE string,
             library_link      TYPE string,
             main_service_name TYPE string,
             bsp_name          TYPE string,
           END OF ty_app .
  types:
    BEGIN OF ty_res,
             fiori_id          TYPE string,
             app_name          TYPE string,
             library_link      TYPE string,
             main_service_name TYPE string,
             service_uri       TYPE string,
             odata_version     TYPE string,
             segw_project      TYPE string,
             programming_model TYPE string,
             business_entity   TYPE string,
             fpm_Extended            TYPE string,
           END OF ty_res .
  types:
    ty_app_tt TYPE STANDARD TABLE OF ty_app WITH EMPTY KEY .
  types:
    ty_res_tt TYPE STANDARD TABLE OF ty_res WITH EMPTY KEY .
  types:
    ty_string_tt TYPE STANDARD TABLE OF string WITH NON-UNIQUE KEY table_line .

  constants C_MODEL_RAP type STRING value 'RAP' ##NO_TEXT.
  constants C_MODEL_BOPF type STRING value 'BOPF' ##NO_TEXT.
  constants C_MODEL_NA type STRING value 'N/A' ##NO_TEXT.

  class-methods RUN_COLLECT
    importing
      !IV_INFILE type RLGRAP-FILENAME
      !IV_IDS type STRING optional
    returning
      value(RT) type TY_RES_TT .
  class-methods WRITE_OUTPUT_CSV
    importing
      !IT_RES type TY_RES_TT
      !IV_PATH type RLGRAP-FILENAME .
  class-methods WRITE_OUTPUT_JSON
    importing
      !IT_RES type TY_RES_TT
      !IV_PATH type RLGRAP-FILENAME .
  PRIVATE SECTION.

    CLASS-METHODS split_ids
      IMPORTING iv_ids    TYPE string
      RETURNING VALUE(rt) TYPE ty_string_tt.

    CLASS-METHODS read_input_csv
      IMPORTING iv_path   TYPE rlgrap-filename
                it_ids    TYPE ty_string_tt OPTIONAL
      RETURNING VALUE(rt) TYPE ty_app_tt.

    CLASS-METHODS analyze_app
      IMPORTING is_app    TYPE ty_app
      RETURNING VALUE(rs) TYPE ty_res.

    CLASS-METHODS get_metadata_xml
      IMPORTING iv_service_name TYPE string
      RETURNING VALUE(rv_xml)   TYPE string.

    CLASS-METHODS detect_odata_version
      IMPORTING iv_xml        TYPE string
      RETURNING VALUE(rv_ver) TYPE string.

    CLASS-METHODS guess_cds_c_from_metadata
      IMPORTING iv_xml          TYPE string
      RETURNING VALUE(rv_cds_c) TYPE string.

    CLASS-METHODS c_to_i
      IMPORTING iv_cds_c        TYPE string
      RETURNING VALUE(rv_cds_i) TYPE string.

    CLASS-METHODS read_ddl_source
      IMPORTING iv_ddlname    TYPE string
      RETURNING VALUE(rv_src) TYPE string.

    CLASS-METHODS get_manifest_json
      IMPORTING iv_bsp_name    TYPE string
      RETURNING VALUE(rv_json) TYPE string.

    CLASS-METHODS detect_fe_fpm
      IMPORTING iv_manifest_json TYPE string
      EXPORTING ev_is_fe         TYPE abap_bool
                ev_fe_version    TYPE string
                ev_fpm_flag      TYPE string.

    CLASS-METHODS classify
      IMPORTING iv_odata_version TYPE string
                iv_c_name        TYPE string
                iv_i_name        TYPE string
                iv_src_c         TYPE string
                iv_src_i         TYPE string
      EXPORTING ev_model         TYPE string
                ev_business_ent  TYPE string.

    CLASS-METHODS split_csv_line
      IMPORTING iv_line   TYPE string
                iv_delim  TYPE c DEFAULT ','
                iv_quote  TYPE c DEFAULT '"'
      RETURNING VALUE(rt) TYPE ty_string_tt.

ENDCLASS.



CLASS ZCL_FIORI_MODEL_ANALYZER IMPLEMENTATION.


METHOD analyze_app.
  DATA: lv_manifest_json TYPE string,
        lv_metadata_xml  TYPE string,
        lo_mani          TYPE REF TO zcl_fiori_model_manifest,
        ls_ds            TYPE zcl_fiori_model_manifest=>ty_main_ds_info,
        lv_entity_set    TYPE string,
        lv_c_name        TYPE string.

  rs = VALUE ty_res(
         fiori_id          = is_app-fiori_id
         app_name          = is_app-app_name
         library_link      = is_app-library_link
         main_service_name = is_app-main_service_name
         service_uri       = ''
         odata_version     = ''
         programming_model = ''
         business_entity   = ''
         fpm_Extended             = 'N/A' ).

  IF is_app-bsp_name IS NOT INITIAL.
    lv_manifest_json = get_manifest_json( is_app-bsp_name ).
  ENDIF.

  IF lv_manifest_json IS NOT INITIAL.
    TRY.
        CREATE OBJECT lo_mani
          EXPORTING
            iv_manifest_json = lv_manifest_json.

        ls_ds = lo_mani->get_main_datasource( ).

        IF ls_ds-uri IS NOT INITIAL.
          rs-service_uri = ls_ds-uri.
        ENDIF.

        IF ls_ds-odata_version IS NOT INITIAL.
          rs-odata_version = ls_ds-odata_version.
        ENDIF.

        " Extract entitySet from manifest (used as CDS C-view name)
        lv_entity_set = lo_mani->get_primary_entityset( ).

      CATCH cx_root.
    ENDTRY.
  ENDIF.

  DATA: lv_segw_project TYPE /iwbep/i_sbd_sv-project,
        lv_service_name TYPE string,
        lt_services     TYPE TABLE OF string.

  " Check if main_service_name contains multiple services (comma-separated)
  IF is_app-main_service_name IS NOT INITIAL.
    IF is_app-main_service_name CS ','.
      " Split by comma and take only the first service
      SPLIT is_app-main_service_name AT ',' INTO TABLE lt_services.
      READ TABLE lt_services INDEX 1 INTO lv_service_name.
    ELSE.
      " Use the service name as is
      lv_service_name = is_app-main_service_name.
    ENDIF.

    " Query the SEGW project using the single service name
    SELECT SINGLE project
      FROM /iwbep/i_sbd_sv
      WHERE technical_name = @lv_service_name
      INTO @lv_segw_project.

    IF sy-subrc = 0.
      " SEGW project found
      " Store or process lv_segw_project as needed
      IF sy-subrc = 0.
        rs-odata_version     = '2.0'.
        rs-segw_project = lv_segw_project.

        IF lv_manifest_json IS NOT INITIAL.
          DATA: lv_is_fe  TYPE abap_bool,
                lv_fe_ver TYPE string,
                lv_fpm    TYPE string.
          detect_fe_fpm(
            EXPORTING iv_manifest_json = lv_manifest_json
            IMPORTING ev_is_fe         = lv_is_fe
                      ev_fe_version    = lv_fe_ver
                      ev_fpm_flag      = lv_fpm ).
          rs-fpm_Extended  = lv_fpm.
        ENDIF.

*      RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  "disabling the $metadata
***  IF rs-service_uri IS NOT INITIAL.
***    lv_metadata_xml = get_metadata_xml( rs-service_uri ).
***  ELSEIF is_app-main_service_name IS NOT INITIAL.
***    lv_metadata_xml = get_metadata_xml( is_app-main_service_name ).
***  ENDIF.

**  IF lv_metadata_xml IS INITIAL.
**    rs-programming_model = COND string( WHEN rs-odata_version = '4.0' THEN 'RAP' ELSE 'Unknown' ).
**
**    IF lv_manifest_json IS NOT INITIAL.
**      DATA: lv_is_fe2  TYPE abap_bool,
**            lv_fe_ver2 TYPE string,
**            lv_fpm2    TYPE string.
**      detect_fe_fpm(
**        EXPORTING iv_manifest_json = lv_manifest_json
**        IMPORTING ev_is_fe         = lv_is_fe2
**                  ev_fe_version    = lv_fe_ver2
**                  ev_fpm_flag      = lv_fpm2 ).
**      rs-fe_fpm = lv_fpm2.
**    ENDIF.
**    RETURN.
**  ENDIF.

**  IF rs-odata_version IS INITIAL.
**    rs-odata_version = detect_odata_version( lv_metadata_xml ).
**  ENDIF.

  " Priority 1: Use entitySet from manifest as CDS C-view name
  lv_c_name = lv_entity_set.

  "disabling the $metadata
**  " Priority 2: Fallback to guessing from metadata if manifest didn't provide it
**  IF lv_c_name IS INITIAL.
**    lv_c_name = guess_cds_c_from_metadata( lv_metadata_xml ).
**  ENDIF.

  IF lv_c_name IS INITIAL.
    rs-programming_model = COND string( WHEN rs-odata_version = '4.0' THEN c_model_rap ELSE c_model_na ).

    IF lv_manifest_json IS NOT INITIAL.
      DATA: lv_is_fe3  TYPE abap_bool,
            lv_fe_ver3 TYPE string,
            lv_fpm3    TYPE string.
      detect_fe_fpm(
        EXPORTING iv_manifest_json = lv_manifest_json
        IMPORTING ev_is_fe         = lv_is_fe3
                  ev_fe_version    = lv_fe_ver3
                  ev_fpm_flag      = lv_fpm3 ).
      rs-fpm_Extended  = lv_fpm3.
    ENDIF.
    RETURN.
  ENDIF.

  DATA(lv_i_name) = c_to_i( lv_c_name ).
  DATA(lv_src_i)  = read_ddl_source( lv_i_name ).
  DATA(lv_src_c)  = read_ddl_source( lv_c_name ).

  classify(
    EXPORTING
      iv_odata_version = rs-odata_version
      iv_c_name        = lv_c_name
      iv_i_name        = lv_i_name
      iv_src_c         = lv_src_c
      iv_src_i         = lv_src_i
    IMPORTING
      ev_model         = rs-programming_model
      ev_business_ent  = rs-business_entity ).

  IF lv_manifest_json IS NOT INITIAL.
    DATA: lv_is_fe4  TYPE abap_bool,
          lv_fe_ver4 TYPE string,
          lv_fpm4    TYPE string.
    detect_fe_fpm(
      EXPORTING iv_manifest_json = lv_manifest_json
      IMPORTING ev_is_fe         = lv_is_fe4
                ev_fe_version    = lv_fe_ver4
                ev_fpm_flag      = lv_fpm4 ).
    rs-fpm_Extended  = lv_fpm4.
  ELSE.
    rs-fpm_Extended = 'N/A'.
  ENDIF.

ENDMETHOD.


METHOD classify.
  DATA: lv_be    TYPE string VALUE '',
        lv_model TYPE string.

  IF iv_odata_version = '4.0'.
    " OData V4 = sempre RAP
    IF iv_src_i CS 'define root view entity'.
      lv_model = c_model_rap.
      lv_be    = iv_i_name.
    ELSEIF iv_src_c CS 'define root view entity'.
      lv_model = c_model_rap.
      lv_be    = iv_c_name.
    ELSE.
      lv_model = c_model_rap.
      lv_be    = COND string( WHEN iv_src_i IS NOT INITIAL THEN iv_i_name ELSE iv_c_name ).
    ENDIF.

  ELSE. " OData V2

    " Check 1: Explicit BOPF annotation
    IF iv_src_i CS '@ObjectModel.modelCategory: #BOPF'
       OR iv_src_c CS '@ObjectModel.modelCategory: #BOPF'.
      lv_model = c_model_bopf.
      lv_be    = COND string( WHEN iv_src_i IS NOT INITIAL THEN iv_i_name ELSE iv_c_name ).

    " Check 2: BOPF pattern - transactionalProcessingDelegated in C-view
    ELSEIF iv_src_c CS 'transactionalProcessingDelegated'.
      IF iv_c_name CP 'C_*' AND iv_src_c IS NOT INITIAL.
        DATA lv_i_ref TYPE string.
        FIND PCRE 'from\s+(I_[A-Za-z0-9_]+)' IN iv_src_c
             SUBMATCHES lv_i_ref IGNORING CASE.
        IF sy-subrc = 0 AND lv_i_ref IS NOT INITIAL.
          DATA(lv_src_i_ref) = read_ddl_source( lv_i_ref ).
          IF lv_src_i_ref CS 'transactionalProcessingEnabled'
             OR lv_src_i_ref CS 'writeDraftPersistence'.
            lv_model = c_model_bopf.
            lv_be    = lv_i_ref.
            ev_model        = lv_model.
            ev_business_ent = lv_be.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.

    " Check 3: RAP root view entity (transactional o meno)
    ELSEIF iv_src_i CS 'define root view entity'
       OR iv_src_c CS 'define root view entity'.
      lv_model = c_model_rap.
      lv_be    = COND string( WHEN iv_src_i IS NOT INITIAL THEN iv_i_name ELSE iv_c_name ).

    " Check 4: RAP Consumption View (read-only) - NUOVO!
    ELSEIF ( iv_src_c CS '@VDM.viewType: #CONSUMPTION'
          OR iv_src_i CS '@VDM.viewType: #CONSUMPTION' )
       OR  ( ( iv_src_c CS '@Metadata.allowExtensions' OR iv_src_i CS '@Metadata.allowExtensions' )
         AND ( iv_c_name CP 'C_*' OR iv_i_name CP 'C_*' ) ).
      lv_model = c_model_rap.
      lv_be    = COND string( WHEN iv_src_c IS NOT INITIAL THEN iv_c_name ELSE iv_i_name ).

    ELSE.
      lv_model = c_model_na.
      lv_be    = iv_c_name.
    ENDIF.

  ENDIF.

  ev_model        = lv_model.
  ev_business_ent = lv_be.
ENDMETHOD.


  METHOD C_TO_I.
    rv_cds_i = iv_cds_c.
    IF rv_cds_i CP 'C_*'.
      REPLACE FIRST OCCURRENCE OF 'C_' IN rv_cds_i WITH 'I_'.
    ENDIF.
  ENDMETHOD.


  METHOD DETECT_FE_FPM.
    ev_is_fe      = abap_false.
    ev_fe_version = '?'.
    ev_fpm_flag   = 'N/A'.

    IF iv_manifest_json IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_manifest_json CS 'sap.ui.controllerExtensions'
   OR iv_manifest_json CS 'viewExtensions'
   OR iv_manifest_json CS '"extends"'
   OR iv_manifest_json CS '''extends'''
   OR iv_manifest_json CS '/ext/'
   OR iv_manifest_json CS '"ext/"'.
      ev_fpm_flag = 'Yes'.
    ELSE.
      ev_fpm_flag = 'No'.
    ENDIF.

    IF iv_manifest_json CS '"sap.ui.generic.app"'
       OR iv_manifest_json CS '''sap.ui.generic.app'''.
      ev_is_fe      = abap_true.
      ev_fe_version = 'V2'.
      IF ev_fpm_flag IS INITIAL.
        ev_fpm_flag   = 'N/A'.
      ENDIF.
      RETURN.
    ENDIF.

    IF iv_manifest_json CS '"sap.fe"'
       OR iv_manifest_json CS '''sap.fe'''
       OR iv_manifest_json CS 'fiorielements.v4'.
      ev_is_fe      = abap_true.
      ev_fe_version = 'V4'.
      RETURN.
    ENDIF.

    ev_is_fe      = abap_false.
    ev_fe_version = '?'.
    ev_fpm_flag   = 'N/A'.
  ENDMETHOD.


  METHOD DETECT_ODATA_VERSION.
    DATA: lv_ver_root TYPE string,
          lv_ns_edmx  TYPE string.

    FIND PCRE '<edmx:Edmx[^>]*Version="([^"]+)"[^>]*xmlns:edmx="([^"]+)"'
         IN iv_xml
         SUBMATCHES lv_ver_root lv_ns_edmx.

    IF sy-subrc = 0.
      IF lv_ver_root CP '4*'
         OR lv_ns_edmx CS 'docs.oasis-open.org/odata/ns/edmx'.
        rv_ver = '4.0'.
        RETURN.
      ELSE.
        rv_ver = '2.0'.
        RETURN.
      ENDIF.
    ENDIF.

    IF iv_xml CS 'm:DataServiceVersion="2.0"'
       OR iv_xml CS 'DataServiceVersion="2.0"'.
      rv_ver = '2.0'.
      RETURN.
    ENDIF.

    FIND PCRE '<Schema[^>]*xmlns="([^"]+)"' IN iv_xml SUBMATCHES lv_ns_edmx.
    IF sy-subrc = 0.
      IF lv_ns_edmx CS 'docs.oasis-open.org/odata/ns/edm'.
        rv_ver = '4.0'.
        RETURN.
      ELSEIF lv_ns_edmx CS 'schemas.microsoft.com/ado/2008/09/edm'.
        rv_ver = '2.0'.
        RETURN.
      ENDIF.
    ENDIF.

    rv_ver = '2.0'.
  ENDMETHOD.


  METHOD GET_MANIFEST_JSON.
    DATA: lo_client TYPE REF TO if_http_client,
          lv_uri    TYPE string,
          lv_status TYPE i.

    IF iv_bsp_name IS INITIAL.
      RETURN.
    ENDIF.

    cl_http_client=>create_by_destination(
      EXPORTING destination = 'NONE'
      IMPORTING client      = lo_client
      EXCEPTIONS OTHERS     = 1 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lv_uri = |/sap/bc/ui5_ui5/sap/{ iv_bsp_name }/manifest.json|.
    lo_client->request->set_header_field( name  = '~request_method'
                                          value = 'GET' ).
    lo_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    lo_client->send( ).
    lo_client->receive( ).

    lo_client->response->get_status( IMPORTING code = lv_status ).
    IF lv_status = 200.
      rv_json = lo_client->response->get_cdata( ).
    ENDIF.
  ENDMETHOD.


  METHOD GET_METADATA_XML.
    DATA: lv_root   TYPE string,
          lo_client TYPE REF TO if_http_client,
          lv_status TYPE i.

    lv_root = ||.
*    IF iv_service_name CS '_V4'.
    "lv_root = |/sap/opu/odata4/sap/{ to_lower( iv_service_name ) }/0001/|.
    lv_root = iv_service_name.
*    ELSE.
*      lv_root = |/sap/opu/odata/sap/{ iv_service_name }/|.
*    ENDIF.

    cl_http_client=>create_by_destination(
      EXPORTING destination = 'NONE'
      IMPORTING client      = lo_client
      EXCEPTIONS OTHERS     = 1 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_client->request->set_header_field( name  = '~request_method'
                                          value = 'GET' ).
    lo_client->request->set_header_field( name = '~request_uri'    value = lv_root && '$metadata' ).
    lo_client->send( ).
    lo_client->receive( ).

    lo_client->response->get_status( IMPORTING code = lv_status ).
    IF lv_status = 200.
      rv_xml = lo_client->response->get_cdata( ).
    ENDIF.
  ENDMETHOD.


  METHOD GUESS_CDS_C_FROM_METADATA.
    DATA lv1 TYPE string.

    FIND PCRE '<EntitySet[^>]*Name="([^"]+)"'
         IN iv_xml
         SUBMATCHES lv1.

    IF sy-subrc = 0.
      IF lv1 CP 'C_*' OR lv1 CP '*TP'.
        rv_cds_c = lv1.
        RETURN.
      ENDIF.
    ENDIF.

    FIND PCRE '<EntitySet[^>]*Name="(C_[A-Za-z0-9_]+)"'
         IN iv_xml
         SUBMATCHES rv_cds_c.

    IF sy-subrc <> 0.
      rv_cds_c = lv1.
    ENDIF.
  ENDMETHOD.


  METHOD READ_DDL_SOURCE.
    DATA lv_ddlname TYPE string.
    lv_ddlname = iv_ddlname.
    TRANSLATE lv_ddlname TO UPPER CASE.
    SELECT SINGLE source FROM ddddlsrc
      INTO @rv_src
      WHERE ddlname = @lv_ddlname
        AND as4local  = 'A'.
  ENDMETHOD.


  METHOD READ_INPUT_CSV.
    DATA: lv_line     TYPE string,
          lt_cols     TYPE ty_string_tt,
          idx_id      TYPE i VALUE 0,
          idx_appname TYPE i VALUE 0,
          idx_link    TYPE i VALUE 0,
          idx_bsp     TYPE i VALUE 0,
          idx_primary TYPE i VALUE 0,
          idx_add     TYPE i VALUE 0,
          idx_v4group TYPE i VALUE 0,
          lv_name     TYPE string,
          z_bom       TYPE string,
          z_zwsp      TYPE string.

    OPEN DATASET iv_path FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      MESSAGE 'Cannot open input CSV' TYPE 'E'.
    ENDIF.

    READ DATASET iv_path INTO lv_line.
    IF sy-subrc <> 0 OR lv_line IS INITIAL.
      CLOSE DATASET iv_path.
      RETURN.
    ENDIF.

    lt_cols = split_csv_line( lv_line ).
    IF lt_cols IS INITIAL.
      CLOSE DATASET iv_path.
      RETURN.
    ENDIF.

    z_bom  = cl_abap_conv_in_ce=>uccp( 'FEFF' ).
    z_zwsp = cl_abap_conv_in_ce=>uccp( '200B' ).

    DO lines( lt_cols ) TIMES.
      lv_name = lt_cols[ sy-index ].
      TRANSLATE lv_name TO UPPER CASE.

      REPLACE ALL OCCURRENCES OF z_bom  IN lv_name WITH ''.
      REPLACE ALL OCCURRENCES OF z_zwsp IN lv_name WITH ''.
      REPLACE ALL OCCURRENCES OF REGEX '[[:cntrl:]]' IN lv_name WITH ''.

      CASE lv_name.
        WHEN 'FIORIID'.                         idx_id      = sy-index.
        WHEN 'APPNAME'.                         idx_appname = sy-index.
        WHEN 'LINK'.                            idx_link    = sy-index.
        WHEN 'BSPNAME'.                         idx_bsp     = sy-index.
        WHEN 'PRIMARYODATASERVICENAME'.         idx_primary = sy-index.
        WHEN 'ADDITIONALODATASERVICES'.         idx_add     = sy-index.
        WHEN 'ODATAV4SERVICEGROUP'.             idx_v4group = sy-index.
      ENDCASE.
    ENDDO.

    DO.
      READ DATASET iv_path INTO lv_line.
      IF sy-subrc <> 0. EXIT. ENDIF.
      IF lv_line IS INITIAL. CONTINUE. ENDIF.

      lt_cols = split_csv_line( lv_line ).

      IF lt_cols IS INITIAL. CONTINUE. ENDIF.

      DATA(lv_id)       = COND string( WHEN idx_id      > 0 AND idx_id      <= lines( lt_cols ) THEN lt_cols[ idx_id      ] ELSE '' ).
      DATA(lv_app)      = COND string( WHEN idx_appname > 0 AND idx_appname <= lines( lt_cols ) THEN lt_cols[ idx_appname ] ELSE '' ).
      DATA(lv_link)     = COND string( WHEN idx_link    > 0 AND idx_link    <= lines( lt_cols ) THEN lt_cols[ idx_link    ] ELSE '' ).
      DATA(lv_bsp)      = COND string( WHEN idx_bsp     > 0 AND idx_bsp     <= lines( lt_cols ) THEN lt_cols[ idx_bsp     ] ELSE '' ).
      DATA(lv_primary)  = COND string( WHEN idx_primary > 0 AND idx_primary <= lines( lt_cols ) THEN lt_cols[ idx_primary ] ELSE '' ).
      DATA(lv_add_raw)  = COND string( WHEN idx_add     > 0 AND idx_add     <= lines( lt_cols ) THEN lt_cols[ idx_add     ] ELSE '' ).
      DATA(lv_v4grp)    = COND string( WHEN idx_v4group > 0 AND idx_v4group <= lines( lt_cols ) THEN lt_cols[ idx_v4group ] ELSE '' ).

      IF lv_id IS INITIAL.
        CONTINUE.
      ENDIF.

      IF it_ids IS SUPPLIED AND it_ids IS NOT INITIAL.
        READ TABLE it_ids WITH KEY table_line = lv_id TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      DATA(lv_add_first) = ''.
      IF lv_add_raw IS NOT INITIAL.
        DATA lt_add TYPE ty_string_tt.
        SPLIT lv_add_raw AT ',' INTO TABLE lt_add.
        LOOP AT lt_add ASSIGNING FIELD-SYMBOL(<s>).
          CONDENSE <s>.
          IF <s> IS NOT INITIAL.
            lv_add_first = <s>.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

      DATA(lv_main) = lv_primary.
      IF lv_main IS INITIAL AND lv_add_first IS NOT INITIAL.
        lv_main = lv_add_first.
      ENDIF.
      IF lv_main IS INITIAL AND lv_v4grp IS NOT INITIAL.
        lv_main = lv_v4grp.
      ENDIF.

      APPEND VALUE ty_app(
        fiori_id          = lv_id
        app_name          = lv_app
        library_link      = lv_link
        main_service_name = lv_main
        bsp_name          = lv_bsp ) TO rt.
    ENDDO.

    CLOSE DATASET iv_path.
  ENDMETHOD.


  METHOD RUN_COLLECT.
    DATA(ids)  = split_ids( iv_ids ).
    DATA(apps) = read_input_csv( iv_path = iv_infile it_ids = ids ).
    rt = VALUE #( ).
    LOOP AT apps ASSIGNING FIELD-SYMBOL(<a>).
      INSERT analyze_app( <a> ) INTO TABLE rt.
    ENDLOOP.
  ENDMETHOD.


  METHOD SPLIT_CSV_LINE.
    DATA: lv_len      TYPE i,
          lv_i        TYPE i VALUE 0,
          lv_offset   TYPE i,
          lv_c        TYPE c LENGTH 1,
          lv_next     TYPE c LENGTH 1,
          lv_in_quote TYPE abap_bool VALUE abap_false,
          lv_token    TYPE string,
          lv_cr       TYPE c LENGTH 1,
          lv_lf       TYPE c LENGTH 1.

    rt = VALUE ty_string_tt( ).

    lv_len = strlen( iv_line ).

    lv_cr = cl_abap_char_utilities=>cr_lf+0(1).
    lv_lf = cl_abap_char_utilities=>cr_lf+1(1).

    WHILE lv_len > 0.
      lv_offset = lv_len - 1.
      lv_c = iv_line+lv_offset(1).
      IF lv_c = lv_cr OR lv_c = lv_lf.
        lv_len = lv_len - 1.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.

    WHILE lv_i < lv_len.
      lv_c = iv_line+lv_i(1).

      IF lv_in_quote = abap_true.
        IF lv_c = iv_quote.
          lv_offset = lv_i + 1.
          IF lv_offset < lv_len.
            lv_next = iv_line+lv_offset(1).
            IF lv_next = iv_quote.
              lv_token = lv_token && iv_quote.
              lv_i = lv_i + 2.
              CONTINUE.
            ENDIF.
          ENDIF.
          lv_in_quote = abap_false.
        ELSE.
          lv_token = lv_token && lv_c.
        ENDIF.

      ELSE.
        IF lv_c = iv_quote.
          lv_in_quote = abap_true.
        ELSEIF lv_c = iv_delim.
          APPEND lv_token TO rt.
          CLEAR lv_token.
        ELSE.
          lv_token = lv_token && lv_c.
        ENDIF.
      ENDIF.

      lv_i = lv_i + 1.
    ENDWHILE.

    APPEND lv_token TO rt.
  ENDMETHOD.


  METHOD SPLIT_IDS.
    DATA lt TYPE STANDARD TABLE OF string.
    IF iv_ids IS INITIAL.
      RETURN.
    ENDIF.
    SPLIT iv_ids AT ',' INTO TABLE lt.
    LOOP AT lt ASSIGNING FIELD-SYMBOL(<s>).
      CONDENSE <s> NO-GAPS.
      IF <s> IS NOT INITIAL.
        APPEND <s> TO rt.
      ENDIF.
    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM rt.
  ENDMETHOD.


  METHOD write_output_csv.
    CONSTANTS c_nl TYPE c VALUE cl_abap_char_utilities=>newline.
    DATA lv_buffer TYPE string.
    OPEN DATASET iv_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc <> 0.
      MESSAGE 'Cannot open output CSV' TYPE 'E'.
    ENDIF.

    DATA(lv_head) =
      |FioriId;AppName;Link;MainServiceName;ServiceUri;ODataVersion;SEGWProject;ProgrammingModel;BusinessEntity;FPM_Extended|.
    lv_buffer = lv_head && c_nl.
    TRANSFER lv_buffer TO iv_path.

    LOOP AT it_res ASSIGNING FIELD-SYMBOL(<r>).
      DATA(lv_line) =
        |{ <r>-fiori_id };{ <r>-app_name };{ <r>-library_link };{ <r>-main_service_name };{ <r>-service_uri };{ <r>-odata_version };{ <r>-segw_project };{ <r>-programming_model };{ <r>-business_entity };{ <r>-fpm_extended }|.
      TRANSFER lv_line TO iv_path.
    ENDLOOP.

    CLOSE DATASET iv_path.
  ENDMETHOD.


  METHOD WRITE_OUTPUT_JSON.
    DATA lv_json TYPE string.
    TRY.
        lv_json = /ui2/cl_json=>serialize(
                 data        = it_res
                 compress    = abap_true
                 pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
      CATCH cx_root.
    ENDTRY.

    OPEN DATASET iv_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc <> 0.
      MESSAGE 'Cannot open output JSON' TYPE 'E'.
    ENDIF.
    TRANSFER lv_json TO iv_path.
    CLOSE DATASET iv_path.
  ENDMETHOD.
ENDCLASS.
