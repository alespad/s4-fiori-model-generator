CLASS zcl_fiori_model_analyzer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF app,
        fiori_id     TYPE string,
        app_name     TYPE string,
        library_link TYPE string,
        bsp_name     TYPE string,
      END OF app .
    TYPES:
      BEGIN OF result,
        fiori_id          TYPE string,
        app_name          TYPE string,
        library_link      TYPE string,
        main_service_name TYPE string,
        service_uri       TYPE string,
        segw_project      TYPE string,
        odata_version     TYPE string,
        programming_model TYPE string,
        business_entity   TYPE string,
        fpm_extended      TYPE string,
      END OF result .
    TYPES:
      app_table TYPE STANDARD TABLE OF app WITH EMPTY KEY .
    TYPES:
      result_table TYPE STANDARD TABLE OF result WITH EMPTY KEY .
    TYPES:
      string_table TYPE STANDARD TABLE OF string
                     WITH NON-UNIQUE KEY table_line .

    CONSTANTS model_rap TYPE string VALUE 'RAP' ##NO_TEXT.
    CONSTANTS model_bopf TYPE string VALUE 'BOPF' ##NO_TEXT.
    CONSTANTS model_na TYPE string VALUE 'N/A' ##NO_TEXT.

    CLASS-METHODS run_collect
      IMPORTING
        !infile       TYPE string
        !ids          TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE result_table .
    CLASS-METHODS run_collect_from_string
      IMPORTING
        !csv_content  TYPE string
        !ids          TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE result_table .
    CLASS-METHODS write_output_csv
      IMPORTING
        !res  TYPE result_table
        !path TYPE string .
    CLASS-METHODS write_output_json
      IMPORTING
        !res  TYPE result_table
        !path TYPE string .
  PRIVATE SECTION.

    CLASS-METHODS read_input_csv_from_string
      IMPORTING
        !csv_content  TYPE string
        !ids          TYPE string_table OPTIONAL
      RETURNING
        VALUE(result) TYPE app_table .
    CLASS-METHODS split_ids
      IMPORTING
        !ids          TYPE string
      RETURNING
        VALUE(result) TYPE string_table .
    CLASS-METHODS read_input_csv
      IMPORTING
        !path         TYPE string
        !ids          TYPE string_table OPTIONAL
      RETURNING
        VALUE(result) TYPE app_table .
    CLASS-METHODS analyze_app
      IMPORTING
        !app          TYPE app
      RETURNING
        VALUE(result) TYPE result .
    CLASS-METHODS read_ddl_source
      IMPORTING
        !ddlname      TYPE string
      RETURNING
        VALUE(source) TYPE string .
    CLASS-METHODS get_manifest_json
      IMPORTING
        !bsp_name   TYPE string
      RETURNING
        VALUE(json) TYPE string .
    CLASS-METHODS detect_fe_fpm
      IMPORTING
        !manifest_json TYPE string
      EXPORTING
        !is_fe         TYPE abap_bool
        !fe_version    TYPE string
        !fpm_flag      TYPE string .
    CLASS-METHODS classify
      IMPORTING
        !odata_version TYPE string
        !name_c        TYPE string
        !name_i        TYPE string
        !src_c         TYPE string
        !src_i         TYPE string
      EXPORTING
        !model         TYPE string
        !business_ent  TYPE string .
    CLASS-METHODS split_csv_line
      IMPORTING
        !line         TYPE string
        !delim        TYPE c DEFAULT ','
        !quote        TYPE c DEFAULT '"'
      RETURNING
        VALUE(result) TYPE string_table .
    CLASS-METHODS find_alias
      IMPORTING
        !entity          TYPE string
        !service_binding TYPE string
      RETURNING
        VALUE(name_c)    TYPE string .

    CLASS-METHODS initialize_result
      IMPORTING
        !app          TYPE app
      RETURNING
        VALUE(result) TYPE result.

    CLASS-METHODS extract_service_info
      IMPORTING
        !datasource TYPE zcl_fiori_model_manifest=>main_datasource_info
      CHANGING
        !result     TYPE result.

    CLASS-METHODS determine_odata_version
      IMPORTING
        !service_name              TYPE string
        !datasource                TYPE zcl_fiori_model_manifest=>main_datasource_info
      CHANGING
        !result                    TYPE result
      RETURNING
        VALUE(continue_processing) TYPE abap_bool.

    CLASS-METHODS resolve_cds_views
      IMPORTING
        !entity_set   TYPE string
        !service_name TYPE string
      EXPORTING
        !name_c       TYPE string
        !name_i       TYPE string
        !src_c        TYPE string
        !src_i        TYPE string.

    CLASS-METHODS normalize_entity_name
      IMPORTING
        !entity_set   TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS check_root_view_entity
      IMPORTING
        !src_i       TYPE string
        !src_c       TYPE string
        !name_i      TYPE string
        !name_c      TYPE string
      EXPORTING
        !is_root     TYPE abap_bool
        !entity_name TYPE string.

    CLASS-METHODS find_rap_bo_entity
      IMPORTING
        !src_c        TYPE string
        !src_i        TYPE string
        !name_c       TYPE string
        !name_i       TYPE string
      RETURNING
        VALUE(entity) TYPE string.

    CLASS-METHODS check_bopf_indicators_v2
      IMPORTING
        !src_i       TYPE string
        !src_c       TYPE string
        !name_c      TYPE string
        !name_i      TYPE string
      EXPORTING
        !is_bopf     TYPE abap_bool
        !entity_name TYPE string.

    CLASS-METHODS check_vdm_consumption
      IMPORTING
        !src_c        TYPE string
        !src_i        TYPE string
        !name_c       TYPE string
        !name_i       TYPE string
      RETURNING
        VALUE(is_vdm) TYPE abap_bool.

    CLASS-METHODS extract_entity_from_routing
      IMPORTING
        !manifest_json     TYPE string
      RETURNING
        VALUE(entity_name) TYPE string.

    " NEW METHOD: Extract view name from FROM clause
    CLASS-METHODS extract_from_clause
      IMPORTING
        !ddl_source      TYPE string
      RETURNING
        VALUE(view_name) TYPE string.
ENDCLASS.



CLASS ZCL_FIORI_MODEL_ANALYZER IMPLEMENTATION.


  METHOD initialize_result.
    result = VALUE result(
      fiori_id          = app-fiori_id
      app_name          = app-app_name
      library_link      = app-library_link
      main_service_name = ''
      service_uri       = ''
      programming_model = 'N/A'
      business_entity   = ''
      fpm_extended      = 'N/A' ).
  ENDMETHOD.


  METHOD extract_service_info.
    IF datasource-uri IS INITIAL.
      RETURN.
    ENDIF.

    result-service_uri = datasource-uri.

    " Remove trailing slash if present
    IF result-service_uri CP '*/' AND strlen( result-service_uri ) > 1.
      result-service_uri = substring(
        val = result-service_uri
        len = strlen( result-service_uri ) - 1 ).
    ENDIF.

    " Remove everything after semicolon
    SPLIT result-service_uri AT ';' INTO result-service_uri DATA(uri_params).

    " Split URI into segments
    SPLIT result-service_uri AT '/' INTO TABLE DATA(segments).

    " Start from last segment and go backwards until finding a non-numeric one
    DATA: lines   TYPE i,
          index   TYPE i,
          segment TYPE string.

    lines = lines( segments ).

    DO lines TIMES.
      index = lines - sy-index + 1.
      READ TABLE segments INTO segment INDEX index.
      IF sy-subrc = 0.
        " Check if segment contains non-numeric characters
        IF segment CN '0123456789'.
          result-main_service_name = segment.
          EXIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD determine_odata_version.
    DATA: segw_project       TYPE /iwbep/i_sbd_sv-project,
          service_name_srv   TYPE /iwbep/sbdm_project,
          service_definition TYPE string.

    continue_processing = abap_true.

    IF service_name IS INITIAL.
      RETURN.
    ENDIF.

    " Check if it's a SEGW project (OData V2)
    SELECT SINGLE project
      FROM /iwbep/i_sbd_sv
      WHERE technical_name = @service_name
      INTO @segw_project.

    IF sy-subrc = 0.
      result-odata_version = '2.0'.
      result-segw_project  = segw_project.
      RETURN.
    ELSE.
      "if not found try to search replacing _SRV
      service_name_srv = service_name.
      REPLACE '_SRV' WITH '' INTO service_name_srv.
      IF sy-subrc =  0.
        SELECT SINGLE project
          FROM /iwbep/i_sbd_sv
          WHERE project = @service_name_srv
          INTO @segw_project.
        IF sy-subrc = 0.
          result-odata_version = '2.0'.
          result-segw_project  = segw_project.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    " Check if it's a service binding (OData V2)
    IF datasource-odata_version = '2.0'.
      SELECT SINGLE service_name
        FROM srvb_service_details
        INTO @service_definition
        WHERE srvb_name = @service_name
          AND version = 'A'.

      result-odata_version = '2.0'.

      IF sy-subrc NE 0.
        " Probably deprecated app
        continue_processing = abap_false.
        RETURN.
      ENDIF.
    ELSE.
      " Default to OData V4
      result-odata_version = '4.0'.
    ENDIF.
  ENDMETHOD.


  METHOD normalize_entity_name.
    result = entity_set.

    " For analytical apps / CDS parameterized view
    " Check if it ends with 'Results' and remove it
    IF result CP '*Results'.
      REPLACE FIRST OCCURRENCE OF REGEX 'Results$' IN result WITH ''.
    ENDIF.
  ENDMETHOD.


  METHOD resolve_cds_views.
    DATA: normalized_entity TYPE string.

    " Normalize entity name (remove 'Results' suffix if present)
    normalized_entity = normalize_entity_name( entity_set ).

    " Start with the entity name as C_
    name_c = normalized_entity.
    src_c = read_ddl_source( name_c ).

    " If C_ not found, search for alias in service definition
    IF src_c IS INITIAL.
      name_c = find_alias(
        service_binding = service_name
        entity = entity_set ).

      IF name_c IS NOT INITIAL.
        src_c = read_ddl_source( name_c ).
      ELSE.
        CLEAR: name_c, name_i, src_c, src_i.
        RETURN.
      ENDIF.
    ENDIF.

    " ========================================
    " Extract I_ view from C_ source using FROM clause
    " This is the KEY change - no more c_to_i assumptions!
    " ========================================
    IF src_c IS NOT INITIAL.
      name_i = extract_from_clause( src_c ).

      IF name_i IS NOT INITIAL.
        src_i = read_ddl_source( name_i ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD analyze_app.
    DATA: manifest_json TYPE string,
          manifest      TYPE REF TO zcl_fiori_model_manifest,
          datasource    TYPE zcl_fiori_model_manifest=>main_datasource_info,
          entity_set    TYPE string,
          name_c        TYPE string,
          name_i        TYPE string,
          src_c         TYPE string,
          src_i         TYPE string.

    " Initialize result structure
    result = initialize_result( app ).

    " Early exit if no BSP name
    CHECK app-bsp_name IS NOT INITIAL.

    " Get manifest JSON
    manifest_json = get_manifest_json( app-bsp_name ).

    " Early exit if manifest not found (app doesn't exist in this system)
    IF manifest_json IS INITIAL.
      "if manifest not found, the fiori app doesn't exist in this s4 system
      CLEAR result.
      RETURN.
    ENDIF.

    " Parse manifest and extract datasource info
    TRY.
        CREATE OBJECT manifest
          EXPORTING
            manifest_json = manifest_json.

        datasource = manifest->get_main_datasource( ).
        extract_service_info(
          EXPORTING datasource = datasource
          CHANGING result = result ).

        entity_set = manifest->get_primary_entityset( ).

        " Fallback: if entity_set is empty, try to extract it from routing patterns
        IF entity_set IS INITIAL.
          entity_set = extract_entity_from_routing( manifest_json ).
        ENDIF.

      CATCH cx_root.
        " If manifest parsing fails, clear result and exit
        CLEAR result.
        RETURN.
    ENDTRY.

    " Determine OData version and SEGW project
    DATA(odatafound) = determine_odata_version(
      EXPORTING
        service_name = result-main_service_name
        datasource   = datasource
      CHANGING
        result = result ).

    " Exit if deprecated app detected
    CHECK odatafound = abap_true OR entity_set IS NOT INITIAL.

    " Resolve CDS views (C and I views)
    resolve_cds_views(
      EXPORTING
        entity_set   = entity_set
        service_name = result-main_service_name
      IMPORTING
        name_c = name_c
        name_i = name_i
        src_c  = src_c
        src_i  = src_i ).

    " If CDS views not found, determine programming model and exit
    IF name_c IS INITIAL.
      result-programming_model = COND string(
        WHEN result-odata_version = '4.0'
        THEN model_rap
        ELSE model_na ).
      RETURN.
    ENDIF.

    " Classify programming model and business entity
    classify(
      EXPORTING
        odata_version = result-odata_version
        name_c        = name_c
        name_i        = name_i
        src_c         = src_c
        src_i         = src_i
      IMPORTING
        model        = result-programming_model
        business_ent = result-business_entity ).

    " Detect Fiori Elements and FPM extensions
    detect_fe_fpm(
      EXPORTING
        manifest_json = manifest_json
      IMPORTING
        fpm_flag = result-fpm_extended ).

  ENDMETHOD.


  METHOD check_root_view_entity.
    is_root = abap_false.

    IF src_i CS 'define root view entity'.
      is_root = abap_true.
      entity_name = name_i.
    ELSEIF src_c CS 'define root view entity'.
      is_root = abap_true.
      entity_name = name_c.
    ELSE.
      entity_name = COND string(
        WHEN src_i IS NOT INITIAL
        THEN name_i
        ELSE name_c ).
    ENDIF.
  ENDMETHOD.


  METHOD find_rap_bo_entity.
    DATA: src_to_check TYPE string,
          entity_temp  TYPE string.

    CLEAR entity.

    src_to_check = COND string(
      WHEN src_c IS NOT INITIAL
      THEN src_c
      ELSE src_i ).

    CHECK src_to_check IS NOT INITIAL.

    " Look for 'as projection on R_' pattern
    FIND PCRE 'projection\s+on\s+(R_[A-Za-z0-9_]+)' IN src_to_check
      SUBMATCHES entity_temp IGNORING CASE.

    IF sy-subrc = 0 AND entity_temp IS NOT INITIAL.
      entity = entity_temp.
      RETURN.
    ENDIF.

    " Try alternative pattern 'from R_'
    FIND PCRE 'from\s+(R_[A-Za-z0-9_]+)' IN src_to_check
      SUBMATCHES entity_temp IGNORING CASE.

    IF sy-subrc = 0 AND entity_temp IS NOT INITIAL.
      entity = entity_temp.
    ENDIF.
  ENDMETHOD.


  METHOD check_bopf_indicators_v2.
    DATA: from_view   TYPE string,
          src_from    TYPE string,
          deeper_view TYPE string,
          src_deeper  TYPE string.

    is_bopf = abap_false.
    CLEAR entity_name.

    " ========================================
    " Check 1: Explicit BOPF annotation
    " ========================================
    IF src_i CS '@ObjectModel.modelCategory: #BOPF'
      OR src_c CS '@ObjectModel.modelCategory: #BOPF'.
      is_bopf = abap_true.
      entity_name = COND string(
        WHEN src_i IS NOT INITIAL THEN name_i
        ELSE name_c ).
      RETURN.
    ENDIF.

    " ========================================
    " Check 2: Direct transactionalProcessingEnabled on I_
    " ========================================
    IF src_i CS '@ObjectModel.transactionalProcessingEnabled'
      OR src_i CS 'writeDraftPersistence'
      OR src_i CS '@VDM.viewType: #TRANSACTIONAL'.
      is_bopf = abap_true.
      entity_name = name_i.
      RETURN.
    ENDIF.

    " ========================================
    " Check 3: Navigate from C_ to I_ using FROM clause
    " This handles C_BusinessPartner → I_BusinessPartnerTP
    " ========================================
    IF src_c IS NOT INITIAL.

      " Extract the FROM view from C_
      from_view = extract_from_clause( src_c ).

      IF from_view IS NOT INITIAL.
        " Read the FROM view source
        src_from = read_ddl_source( from_view ).

        " Check if FROM view has BOPF indicators
        IF src_from CS '@ObjectModel.transactionalProcessingEnabled'
          OR src_from CS 'writeDraftPersistence'
          OR src_from CS '@VDM.viewType: #TRANSACTIONAL'
          OR src_from CS '@ObjectModel.modelCategory: #BUSINESS_OBJECT'.

          is_bopf = abap_true.
          entity_name = from_view.  " ← The TP/BOPF view!
          RETURN.
        ENDIF.

        " ========================================
        " Navigate deeper: FROM view might also be a projection
        " Example: C_ → I_TP → I_ → /BOBF/I_
        " ========================================
        IF src_from CS 'as select from' OR src_from CS 'as projection on'.
          deeper_view = extract_from_clause( src_from ).

          IF deeper_view IS NOT INITIAL AND deeper_view <> from_view.
            src_deeper = read_ddl_source( deeper_view ).

            IF src_deeper CS '@ObjectModel.transactionalProcessingEnabled'
              OR src_deeper CS 'writeDraftPersistence'
              OR src_deeper CS '@ObjectModel.modelCategory: #BOPF'.
              is_bopf = abap_true.
              entity_name = deeper_view.
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD check_vdm_consumption.
    DATA: src_to_check     TYPE string,
          has_vdm_anno     TYPE abap_bool,
          has_metadata_ext TYPE abap_bool,
          has_c_naming     TYPE abap_bool.

    is_vdm = abap_false.

    " Determine which source to check
    src_to_check = COND string(
      WHEN src_c IS NOT INITIAL
      THEN src_c
      ELSE src_i ).

    CHECK src_to_check IS NOT INITIAL.

    " ========================================
    " CHECK 1: VDM annotation (flexible pattern for spacing)
    " ========================================
    FIND PCRE '@VDM\.viewType\s*:\s*#CONSUMPTION'
      IN src_to_check
      IGNORING CASE.

    IF sy-subrc = 0.
      is_vdm = abap_true.
      RETURN.
    ENDIF.

    " ========================================
    " CHECK 2: Virtual elements with calculated by (RAP pattern)
    " ========================================
    IF src_to_check CS 'virtualElement'
      AND src_to_check CS 'virtualElementCalculatedBy'.
      is_vdm = abap_true.
      RETURN.
    ENDIF.

    " ========================================
    " CHECK 3: CDS with parameters and VDM-style annotations
    " ========================================
    IF ( src_to_check CS 'with parameters'
      OR src_to_check CS '@Environment.systemField' )
      AND ( name_c CP 'C_*' OR name_i CP 'C_*' ).
      is_vdm = abap_true.
      RETURN.
    ENDIF.

    " ========================================
    " CHECK 4: Metadata extensions + C_ naming (common VDM pattern)
    " ========================================
    has_metadata_ext = xsdbool( src_to_check CS '@Metadata.allowExtensions' ).
    has_c_naming = xsdbool( name_c CP 'C_*' OR name_i CP 'C_*' ).

    IF has_metadata_ext = abap_true AND has_c_naming = abap_true.
      is_vdm = abap_true.
      RETURN.
    ENDIF.

    " ========================================
    " CHECK 5: UI annotations + AccessControl (Fiori-ready views)
    " ========================================
    IF ( src_to_check CS '@UI.' OR src_to_check CS '@Search.searchable' )
      AND src_to_check CS '@AccessControl.authorizationCheck'
      AND ( name_c CP 'C_*' OR name_i CP 'C_*' ).
      is_vdm = abap_true.
      RETURN.
    ENDIF.

    " ========================================
    " CHECK 6: ObjectModel usage type (VDM indicator)
    " ========================================
    IF src_to_check CS '@ObjectModel.usageType'.
      is_vdm = abap_true.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD classify.
    DATA: business_entity TYPE string,
          model_type      TYPE string,
          is_root         TYPE abap_bool,
          r_entity        TYPE string.

    CLEAR: model, business_ent.

    " ============================================================
    " OData V4 Classification
    " ============================================================
    IF odata_version = '4.0'.

      " Check for root view entity
      check_root_view_entity(
        EXPORTING
          src_i  = src_i
          src_c  = src_c
          name_i = name_i
          name_c = name_c
        IMPORTING
          is_root     = is_root
          entity_name = business_entity ).

      IF is_root = abap_true.
        model_type = model_rap.
      ENDIF.

      " Check if it's a projection on R_ entity (RAP BO)
      IF business_entity IS NOT INITIAL.
        r_entity = find_rap_bo_entity(
          src_c  = src_c
          src_i  = src_i
          name_c = name_c
          name_i = name_i ).

        IF r_entity IS NOT INITIAL.
          business_entity = r_entity.
          model_type = model_rap.
        ENDIF.
      ENDIF.

      " ============
      " ============================================================
      " OData V2 Classification
      " ============================================================
    ELSE.

      DATA: is_bopf     TYPE abap_bool,
            bopf_entity TYPE string.

      " Check for BOPF indicators
      check_bopf_indicators_v2(
        EXPORTING
          src_i  = src_i
          src_c  = src_c
          name_c = name_c
          name_i = name_i
        IMPORTING
          is_bopf     = is_bopf
          entity_name = bopf_entity ).

      IF is_bopf = abap_true.
        model_type = model_bopf.
        business_entity = bopf_entity.
        model = model_type.
        business_ent = business_entity.
        RETURN.
      ENDIF.

      " Check for root view entity (RAP in V2)
      check_root_view_entity(
        EXPORTING
          src_i  = src_i
          src_c  = src_c
          name_i = name_i
          name_c = name_c
        IMPORTING
          is_root     = is_root
          entity_name = business_entity ).

      IF is_root = abap_true.
        model_type = model_rap.
      ELSEIF check_vdm_consumption(
        src_c  = src_c
        src_i  = src_i
        name_c = name_c
        name_i = name_i ) = abap_true.
        model_type = model_rap.
        business_entity = COND string(
          WHEN src_c IS NOT INITIAL
          THEN name_c
          ELSE name_i ).
      ELSE.
        model_type = model_na.
        business_entity = name_c.
      ENDIF.

    ENDIF.

    " Set output parameters
    model = model_type.
    business_ent = business_entity.
  ENDMETHOD.


  METHOD detect_fe_fpm.
    is_fe      = abap_false.
    fe_version = '?'.
    fpm_flag   = 'N/A'.

    IF manifest_json IS INITIAL.
      RETURN.
    ENDIF.

    IF manifest_json CS 'sap.ui.controllerExtensions'
      OR manifest_json CS 'viewExtensions'
      OR manifest_json CS '.ext.'
      OR manifest_json CS '/ext/'
      OR manifest_json CS '"ext/"'.
      fpm_flag = 'Yes'.
    ELSE.
      fpm_flag = 'No'.
    ENDIF.

    IF manifest_json CS '"sap.ui.generic.app"'
      OR manifest_json CS '''sap.ui.generic.app'''.
      is_fe      = abap_true.
      fe_version = 'V2'.
      IF fpm_flag IS INITIAL.
        fpm_flag = 'N/A'.
      ENDIF.
      RETURN.
    ENDIF.

    IF manifest_json CS '"sap.fe"'
      OR manifest_json CS '''sap.fe'''
      OR manifest_json CS 'fiorielements.v4'.
      is_fe      = abap_true.
      fe_version = 'V4'.
      RETURN.
    ENDIF.

    is_fe      = abap_false.
    fe_version = '?'.
    fpm_flag   = 'N/A'.
  ENDMETHOD.


  METHOD get_manifest_json.
    DATA: client TYPE REF TO if_http_client,
          uri    TYPE string,
          status TYPE i.

    IF bsp_name IS INITIAL.
      RETURN.
    ENDIF.

    cl_http_client=>create_by_destination(
      EXPORTING
        destination = 'NONE'
      IMPORTING
        client      = client
      EXCEPTIONS
        OTHERS      = 1 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    uri = |/sap/bc/ui5_ui5/sap/{ bsp_name }/manifest.json|.
    client->request->set_header_field(
      name  = '~request_method'
      value = 'GET' ).
    client->request->set_header_field(
      name  = '~request_uri'
      value = uri ).
    client->send( ).
    client->receive( ).

    client->response->get_status( IMPORTING code = status ).
    IF status = 200.
      json = client->response->get_cdata( ).
    ENDIF.
  ENDMETHOD.


  METHOD read_ddl_source.
    DATA: ddlname_to_upper TYPE string.
    ddlname_to_upper = ddlname.
    TRANSLATE ddlname_to_upper TO UPPER CASE.
    SELECT SINGLE source FROM ddddlsrc
      INTO @source
      WHERE ddlname = @ddlname_to_upper
        AND as4local = 'A'.
  ENDMETHOD.


  METHOD read_input_csv.
    DATA: line        TYPE string,
          cols        TYPE string_table,
          idx_id      TYPE i VALUE 0,
          idx_appname TYPE i VALUE 0,
          idx_link    TYPE i VALUE 0,
          idx_bsp     TYPE i VALUE 0,
          idx_primary TYPE i VALUE 0,
          idx_add     TYPE i VALUE 0,
          idx_v4group TYPE i VALUE 0,
          name        TYPE string,
          z_bom       TYPE string,
          z_zwsp      TYPE string.

    OPEN DATASET path FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      MESSAGE 'Cannot open input CSV' TYPE 'E'.
    ENDIF.

    READ DATASET path INTO line.
    IF sy-subrc <> 0 OR line IS INITIAL.
      CLOSE DATASET path.
      RETURN.
    ENDIF.

    cols = split_csv_line( line ).
    IF cols IS INITIAL.
      CLOSE DATASET path.
      RETURN.
    ENDIF.

    z_bom  = cl_abap_conv_in_ce=>uccp( 'FEFF' ).
    z_zwsp = cl_abap_conv_in_ce=>uccp( '200B' ).

    DO lines( cols ) TIMES.
      name = cols[ sy-index ].
      TRANSLATE name TO UPPER CASE.

      REPLACE ALL OCCURRENCES OF z_bom  IN name WITH ''.
      REPLACE ALL OCCURRENCES OF z_zwsp IN name WITH ''.
      REPLACE ALL OCCURRENCES OF REGEX '[[:cntrl:]]' IN name WITH ''.

      CASE name.
        WHEN 'FIORIID'.
          idx_id = sy-index.
        WHEN 'APPNAME'.
          idx_appname = sy-index.
        WHEN 'LINK'.
          idx_link = sy-index.
        WHEN 'BSPNAME'.
          idx_bsp = sy-index.
      ENDCASE.
    ENDDO.

    DO.
      READ DATASET path INTO line.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      IF line IS INITIAL.
        CONTINUE.
      ENDIF.

      cols = split_csv_line( line ).

      IF cols IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(id) = COND string(
        WHEN idx_id > 0 AND idx_id <= lines( cols )
        THEN cols[ idx_id ]
        ELSE '' ).
      DATA(app_name) = COND string(
        WHEN idx_appname > 0 AND idx_appname <= lines( cols )
        THEN cols[ idx_appname ]
        ELSE '' ).
      DATA(link) = COND string(
        WHEN idx_link > 0 AND idx_link <= lines( cols )
        THEN cols[ idx_link ]
        ELSE '' ).
      DATA(bsp) = COND string(
        WHEN idx_bsp > 0 AND idx_bsp <= lines( cols )
        THEN cols[ idx_bsp ]
        ELSE '' ).

      IF id IS INITIAL.
        CONTINUE.
      ENDIF.

      IF ids IS SUPPLIED AND ids IS NOT INITIAL.
        READ TABLE ids WITH KEY table_line = id
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      APPEND VALUE app(
        fiori_id          = id
        app_name          = app_name
        library_link      = link
        bsp_name          = bsp ) TO result.
    ENDDO.

    CLOSE DATASET path.
  ENDMETHOD.


  METHOD run_collect.
    DATA(id_list) = split_ids( ids ).
    DATA(apps)    = read_input_csv( path = infile ids = id_list ).
    result = VALUE #( ).
    LOOP AT apps ASSIGNING FIELD-SYMBOL(<a>).
      DATA(analyzed) = zcl_fiori_model_analyzer=>analyze_app( <a> ).
      IF analyzed IS NOT INITIAL.
        INSERT analyzed INTO TABLE result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD split_csv_line.
    DATA: len      TYPE i,
          i        TYPE i VALUE 0,
          offset   TYPE i,
          c        TYPE c LENGTH 1,
          next     TYPE c LENGTH 1,
          in_quote TYPE abap_bool VALUE abap_false,
          token    TYPE string,
          cr       TYPE c LENGTH 1,
          lf       TYPE c LENGTH 1.

    result = VALUE string_table( ).

    len = strlen( line ).

    cr = cl_abap_char_utilities=>cr_lf+0(1).
    lf = cl_abap_char_utilities=>cr_lf+1(1).

    WHILE len > 0.
      offset = len - 1.
      c = line+offset(1).
      IF c = cr OR c = lf.
        len = len - 1.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.

    WHILE i < len.
      c = line+i(1).

      IF in_quote = abap_true.
        IF c = quote.
          offset = i + 1.
          IF offset < len.
            next = line+offset(1).
            IF next = quote.
              token = token && quote.
              i = i + 2.
              CONTINUE.
            ENDIF.
          ENDIF.
          in_quote = abap_false.
        ELSE.
          token = token && c.
        ENDIF.

      ELSE.
        IF c = quote.
          in_quote = abap_true.
        ELSEIF c = delim.
          APPEND token TO result.
          CLEAR token.
        ELSE.
          token = token && c.
        ENDIF.
      ENDIF.

      i = i + 1.
    ENDWHILE.

    APPEND token TO result.
  ENDMETHOD.


  METHOD split_ids.
    DATA temp TYPE STANDARD TABLE OF string.
    IF ids IS INITIAL.
      RETURN.
    ENDIF.
    SPLIT ids AT ',' INTO TABLE temp.
    LOOP AT temp ASSIGNING FIELD-SYMBOL(<s>).
      CONDENSE <s> NO-GAPS.
      IF <s> IS NOT INITIAL.
        APPEND <s> TO result.
      ENDIF.
    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM result.
  ENDMETHOD.


  METHOD write_output_csv.
    CONSTANTS nl TYPE c VALUE cl_abap_char_utilities=>newline.
    DATA buffer TYPE string.
    OPEN DATASET path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc <> 0.
      MESSAGE 'Cannot open output CSV' TYPE 'E'.
    ENDIF.

    DATA(head) =
      |FioriId;AppName;Link;MainServiceName;ServiceUri;| &&
      |ODataVersion;SEGWProject;ProgrammingModel;| &&
      |BusinessEntity;FPM_Extended|.
    buffer = head && nl.
    TRANSFER buffer TO path.

    LOOP AT res ASSIGNING FIELD-SYMBOL(<r>).
      DATA(line) =
        |{ <r>-fiori_id };{ <r>-app_name };| &&
        |{ <r>-library_link };{ <r>-main_service_name };| &&
        |{ <r>-service_uri };{ <r>-odata_version };| &&
        |{ <r>-segw_project };{ <r>-programming_model };| &&
        |{ <r>-business_entity };{ <r>-fpm_extended }|.
      TRANSFER line TO path.
    ENDLOOP.

    CLOSE DATASET path.
  ENDMETHOD.


  METHOD write_output_json.
    DATA json TYPE string.
    TRY.
        json = /ui2/cl_json=>serialize(
          data        = res
          compress    = abap_true
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
      CATCH cx_root.
    ENDTRY.

    OPEN DATASET path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc <> 0.
      MESSAGE 'Cannot open output JSON' TYPE 'E'.
    ENDIF.
    TRANSFER json TO path.
    CLOSE DATASET path.
  ENDMETHOD.


  METHOD find_alias.

    DATA: service_definition TYPE srvb_name,
          source             TYPE string,
          source_lines       TYPE TABLE OF string,
          service_bnd        TYPE srvb_name.
    service_bnd = service_binding.
    TRANSLATE service_bnd TO UPPER CASE.

    CLEAR name_c.

    " Step 1: Get Service Definition from Service Binding
    SELECT SINGLE service_name
      FROM srvb_service_details
      INTO @service_definition
      WHERE srvb_name = @service_bnd
        AND version = 'A'.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Step 2: Get Service Definition source code
    SELECT SINGLE source
      FROM srvdsrc_src
      INTO @source
      WHERE srvdname = @service_definition
        AND version = 'A'.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Step 3: Split source into lines
    SPLIT source AT cl_abap_char_utilities=>newline INTO TABLE source_lines.

    " Step 4: Search for the entity alias mapping
    LOOP AT source_lines INTO DATA(line).
      " Remove leading/trailing spaces
      line = condense( line ).

      " Check if line contains 'expose' and matches the entity alias
      IF line CS 'expose' AND line CS 'as' AND line CS entity.

        " Extract the real entity name between 'expose' and 'as'
        DATA(pattern) = |expose\\s+(\\S+)\\s+as\\s+{ entity }|.

        FIND REGEX pattern IN line SUBMATCHES name_c IGNORING CASE.

        IF sy-subrc = 0.
          " Remove trailing semicolon if present
          REPLACE ALL OCCURRENCES OF ';' IN name_c WITH ''.
          name_c = condense( name_c ).
          EXIT.
        ENDIF.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_input_csv_from_string.
    DATA: lines       TYPE TABLE OF string,
          line        TYPE string,
          cols        TYPE string_table,
          idx_id      TYPE i VALUE 0,
          idx_appname TYPE i VALUE 0,
          idx_link    TYPE i VALUE 0,
          idx_bsp     TYPE i VALUE 0,
          name        TYPE string,
          z_bom       TYPE string,
          z_zwsp      TYPE string,
          line_num    TYPE i VALUE 0.

    " Split content into lines
    SPLIT csv_content AT cl_abap_char_utilities=>newline INTO TABLE lines.

    IF lines IS INITIAL.
      RETURN.
    ENDIF.

    " Read header
    READ TABLE lines INDEX 1 INTO line.
    IF sy-subrc <> 0 OR line IS INITIAL.
      RETURN.
    ENDIF.

    cols = split_csv_line( line ).
    IF cols IS INITIAL.
      RETURN.
    ENDIF.

    z_bom  = cl_abap_conv_in_ce=>uccp( 'FEFF' ).
    z_zwsp = cl_abap_conv_in_ce=>uccp( '200B' ).

    " Parse header columns
    DO lines( cols ) TIMES.
      name = cols[ sy-index ].
      TRANSLATE name TO UPPER CASE.

      REPLACE ALL OCCURRENCES OF z_bom  IN name WITH ''.
      REPLACE ALL OCCURRENCES OF z_zwsp IN name WITH ''.
      REPLACE ALL OCCURRENCES OF REGEX '[[:cntrl:]]' IN name WITH ''.

      CASE name.
        WHEN 'FIORIID'.
          idx_id = sy-index.
        WHEN 'APPNAME'.
          idx_appname = sy-index.
        WHEN 'LINK'.
          idx_link = sy-index.
        WHEN 'BSPNAME'.
          idx_bsp = sy-index.
      ENDCASE.
    ENDDO.

    " Process data lines
    LOOP AT lines INTO line FROM 2.
      IF line IS INITIAL.
        CONTINUE.
      ENDIF.

      cols = split_csv_line( line ).

      IF cols IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(id) = COND string(
        WHEN idx_id > 0 AND idx_id <= lines( cols )
        THEN cols[ idx_id ]
        ELSE '' ).
      DATA(app_name) = COND string(
        WHEN idx_appname > 0 AND idx_appname <= lines( cols )
        THEN cols[ idx_appname ]
        ELSE '' ).
      DATA(link) = COND string(
        WHEN idx_link > 0 AND idx_link <= lines( cols )
        THEN cols[ idx_link ]
        ELSE '' ).
      DATA(bsp) = COND string(
        WHEN idx_bsp > 0 AND idx_bsp <= lines( cols )
        THEN cols[ idx_bsp ]
        ELSE '' ).

      IF id IS INITIAL.
        CONTINUE.
      ENDIF.

      IF ids IS SUPPLIED AND ids IS NOT INITIAL.
        READ TABLE ids WITH KEY table_line = id
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      APPEND VALUE app(
        fiori_id     = id
        app_name     = app_name
        library_link = link
        bsp_name     = bsp ) TO result.
    ENDLOOP.
  ENDMETHOD.


  METHOD run_collect_from_string.
    DATA(id_list) = split_ids( ids ).
    DATA(apps)    = read_input_csv_from_string(
                      csv_content = csv_content
                      ids = id_list ).
    result = VALUE #( ).
    LOOP AT apps ASSIGNING FIELD-SYMBOL(<a>).
      DATA(analyzed) = zcl_fiori_model_analyzer=>analyze_app( <a> ).
      IF analyzed IS NOT INITIAL.
        INSERT analyzed INTO TABLE result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD extract_entity_from_routing.
    DATA: entity_temp   TYPE string,
          all_patterns  TYPE string_table,
          pattern       TYPE string,
          best_match    TYPE string,
          match_score   TYPE i,
          generic_names TYPE string_table.

    CLEAR entity_name.
    CHECK manifest_json IS NOT INITIAL.

    " List of generic route names to exclude (extensible)
    APPEND 'worklist' TO generic_names.
    APPEND 'list' TO generic_names.
    APPEND 'master' TO generic_names.
    APPEND 'main' TO generic_names.
    APPEND 'object' TO generic_names.
    APPEND 'detail' TO generic_names.
    APPEND 'item' TO generic_names.
    APPEND 'display' TO generic_names.
    APPEND 'edit' TO generic_names.
    APPEND 'change' TO generic_names.
    APPEND 'maintain' TO generic_names.
    APPEND 'create' TO generic_names.
    APPEND 'new' TO generic_names.
    APPEND 'show' TO generic_names.
    APPEND 'view' TO generic_names.
    APPEND 'overview' TO generic_names.
    APPEND 'home' TO generic_names.
    APPEND 'start' TO generic_names.
    APPEND 'landing' TO generic_names.

    " ========================================
    " STRATEGY 1: Search for entity with CDS naming convention (high priority)
    " Pattern: C_* or I_* followed by / or ( or { or ?
    " ========================================
    FIND ALL OCCURRENCES OF PCRE
      '"pattern"\s*:\s*"([CI]_[A-Za-z0-9_]+)(?:/|\(|\{|\?)'
      IN manifest_json
      RESULTS DATA(cds_matches)
      IGNORING CASE.

    LOOP AT cds_matches INTO DATA(match).
      DATA(match_text) = manifest_json+match-offset(match-length).
      FIND PCRE '([CI]_[A-Za-z0-9_]+)' IN match_text SUBMATCHES entity_temp.

      IF sy-subrc = 0 AND entity_temp IS NOT INITIAL.
        " CDS entity found, has maximum priority
        entity_name = entity_temp.
        RETURN.
      ENDIF.
    ENDLOOP.

    " ========================================
    " STRATEGY 2: Search in "object" or "detail" routes
    " ========================================
    DATA: target_routes TYPE string_table.
    APPEND 'object' TO target_routes.
    APPEND 'detail' TO target_routes.
    APPEND 'item' TO target_routes.

    LOOP AT target_routes INTO DATA(route_name).
      " Build search pattern dynamically
      DATA(search_pattern) = |"pattern"\\s*:\\s*"([^"]+)"[^| && |\}| && |\}]*"name"\\s*:\\s*"{ route_name }"|.

      FIND PCRE search_pattern
        IN manifest_json
        SUBMATCHES pattern
        IGNORING CASE.

      IF sy-subrc = 0 AND pattern IS NOT INITIAL.
        " Extract first part before / ( { or ?
        FIND PCRE '^([A-Za-z][A-Za-z0-9_]*)(?:/|\(|\{|\?)'
          IN pattern
          SUBMATCHES entity_temp.

        IF sy-subrc = 0 AND entity_temp IS NOT INITIAL.
          " Verify it's not a generic name
          READ TABLE generic_names WITH KEY table_line = entity_temp
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            entity_name = entity_temp.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " ========================================
    " STRATEGY 3: Search for any entity with parameters
    " Generic pattern: EntityName/{param} or EntityName({param})
    " ========================================
    FIND ALL OCCURRENCES OF PCRE
      '"pattern"\s*:\s*"([A-Z][A-Za-z0-9_]+)(?:/\{|\(\{)'
      IN manifest_json
      RESULTS DATA(generic_matches)
      IGNORING CASE.

    " Find best candidate (longest and not generic)
    match_score = 0.
    LOOP AT generic_matches INTO match.
      match_text = manifest_json+match-offset(match-length).
      FIND PCRE '([A-Z][A-Za-z0-9_]+)' IN match_text SUBMATCHES entity_temp.

      IF sy-subrc = 0 AND entity_temp IS NOT INITIAL.
        " Skip generic names
        READ TABLE generic_names WITH KEY table_line = to_lower( entity_temp )
          TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.

        " Prefer longer entities (more specific)
        IF strlen( entity_temp ) > match_score.
          match_score = strlen( entity_temp ).
          best_match = entity_temp.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF best_match IS NOT INITIAL.
      entity_name = best_match.
      RETURN.
    ENDIF.

    " ========================================
    " STRATEGY 4: Search in sap.copilot (if present)
    " ========================================
    FIND PCRE '"whitelistedEntityTypes"\s*:\s*\[([^\]]+)\]'
      IN manifest_json
      SUBMATCHES DATA(entity_list)
      IGNORING CASE.

    IF sy-subrc = 0 AND entity_list IS NOT INITIAL.
      " Search for first entity Type ending with "Type"
      FIND PCRE '([A-Z][A-Za-z0-9_\.]+)Type'
        IN entity_list
        SUBMATCHES entity_temp.

      IF sy-subrc = 0 AND entity_temp IS NOT INITIAL.
        " Remove service prefix if present
        " Example: "FAC_MANAGE_GLACCOUNT_SRV.C_GLAcctInChtAcctsTP"
        SPLIT entity_temp AT '.' INTO DATA(service_prefix) entity_name.
        IF entity_name IS INITIAL.
          entity_name = entity_temp.
        ENDIF.

        " Remove "TP" suffix if present (was in Type)
        IF entity_name CP '*TP'.
          " Entity already correct
        ENDIF.
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD extract_from_clause.
    " Extract view name from FROM clause or PROJECTION ON clause
    " Handles multiple patterns:
    " - as select from VIEW_NAME
    " - as projection on VIEW_NAME
    " - from VIEW_NAME
    " - projection on VIEW_NAME

    CLEAR view_name.

    " Pattern 1: as select from VIEW_NAME
    FIND PCRE 'as\s+select\s+from\s+([A-Z/_][A-Z0-9_/]*)'
         IN ddl_source
         SUBMATCHES view_name
         IGNORING CASE.

    IF sy-subrc = 0 AND view_name IS NOT INITIAL.
      RETURN.
    ENDIF.

    " Pattern 2: as projection on VIEW_NAME
    FIND PCRE 'as\s+projection\s+on\s+([A-Z/_][A-Z0-9_/]*)'
         IN ddl_source
         SUBMATCHES view_name
         IGNORING CASE.

    IF sy-subrc = 0 AND view_name IS NOT INITIAL.
      RETURN.
    ENDIF.

    " Pattern 3: from VIEW_NAME (without "as select")
    FIND PCRE 'from\s+([A-Z/_][A-Z0-9_/]*)'
         IN ddl_source
         SUBMATCHES view_name
         IGNORING CASE.

    IF sy-subrc = 0 AND view_name IS NOT INITIAL.
      RETURN.
    ENDIF.

    " Pattern 4: projection on VIEW_NAME (without "as")
    FIND PCRE 'projection\s+on\s+([A-Z/_][A-Z0-9_/]*)'
         IN ddl_source
         SUBMATCHES view_name
         IGNORING CASE.
  ENDMETHOD.
ENDCLASS.
