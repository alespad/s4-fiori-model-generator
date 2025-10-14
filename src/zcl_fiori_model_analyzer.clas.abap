class ZCL_FIORI_MODEL_ANALYZER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF app,
        fiori_id     TYPE string,
        app_name     TYPE string,
        library_link TYPE string,
        bsp_name     TYPE string,
      END OF app .
  types:
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
  types:
    app_table TYPE STANDARD TABLE OF app WITH EMPTY KEY .
  types:
    result_table TYPE STANDARD TABLE OF result WITH EMPTY KEY .
  types:
    string_table TYPE STANDARD TABLE OF string
                   WITH NON-UNIQUE KEY table_line .

  constants MODEL_RAP type STRING value 'RAP' ##NO_TEXT.
  constants MODEL_BOPF type STRING value 'BOPF' ##NO_TEXT.
  constants MODEL_NA type STRING value 'N/A' ##NO_TEXT.

  class-methods RUN_COLLECT
    importing
      !INFILE type STRING
      !IDS type STRING optional
    returning
      value(RESULT) type RESULT_TABLE .
  class-methods RUN_COLLECT_FROM_STRING
    importing
      !CSV_CONTENT type STRING
      !IDS type STRING optional
    returning
      value(RESULT) type RESULT_TABLE .
  class-methods WRITE_OUTPUT_CSV
    importing
      !RES type RESULT_TABLE
      !PATH type STRING .
  class-methods WRITE_OUTPUT_JSON
    importing
      !RES type RESULT_TABLE
      !PATH type STRING .
private section.

  class-methods READ_INPUT_CSV_FROM_STRING
    importing
      !CSV_CONTENT type STRING
      !IDS type STRING_TABLE optional
    returning
      value(RESULT) type APP_TABLE .
  class-methods SPLIT_IDS
    importing
      !IDS type STRING
    returning
      value(RESULT) type STRING_TABLE .
  class-methods READ_INPUT_CSV
    importing
      !PATH type STRING
      !IDS type STRING_TABLE optional
    returning
      value(RESULT) type APP_TABLE .
  class-methods ANALYZE_APP
    importing
      !APP type APP
    returning
      value(RESULT) type RESULT .
  class-methods GET_METADATA_XML
    importing
      !SERVICE_NAME type STRING
    returning
      value(XML) type STRING .
  class-methods DETECT_ODATA_VERSION
    importing
      !XML type STRING
    returning
      value(VERSION) type STRING .
  class-methods GUESS_CDS_C_FROM_METADATA
    importing
      !XML type STRING
    returning
      value(CDS_C) type STRING .
  class-methods C_TO_I
    importing
      !CDS_C type STRING
    returning
      value(CDS_I) type STRING .
  class-methods READ_DDL_SOURCE
    importing
      !DDLNAME type STRING
    returning
      value(SOURCE) type STRING .
  class-methods GET_MANIFEST_JSON
    importing
      !BSP_NAME type STRING
    returning
      value(JSON) type STRING .
  class-methods DETECT_FE_FPM
    importing
      !MANIFEST_JSON type STRING
    exporting
      !IS_FE type ABAP_BOOL
      !FE_VERSION type STRING
      !FPM_FLAG type STRING .
  class-methods CLASSIFY
    importing
      !ODATA_VERSION type STRING
      !NAME_C type STRING
      !NAME_I type STRING
      !SRC_C type STRING
      !SRC_I type STRING
    exporting
      !MODEL type STRING
      !BUSINESS_ENT type STRING .
  class-methods SPLIT_CSV_LINE
    importing
      !LINE type STRING
      !DELIM type C default ','
      !QUOTE type C default '"'
    returning
      value(RESULT) type STRING_TABLE .
  class-methods FIND_ALIAS
    importing
      !ENTITY type STRING
      !SERVICE_BINDING type STRING
    returning
      value(NAME_C) type STRING .
ENDCLASS.



CLASS ZCL_FIORI_MODEL_ANALYZER IMPLEMENTATION.


  METHOD analyze_app.
    DATA: manifest_json TYPE string,
          metadata_xml  TYPE string,
          manifest      TYPE REF TO zcl_fiori_model_manifest,
          datasource    TYPE zcl_fiori_model_manifest=>main_datasource_info,
          entity_set    TYPE string,
          name_c        TYPE string.

    result = VALUE result(
      fiori_id          = app-fiori_id
      app_name          = app-app_name
      library_link      = app-library_link
      main_service_name = ''
      service_uri       = ''
      programming_model = 'N/A'
      business_entity   = ''
      fpm_extended      = 'N/A' ).

    IF app-bsp_name IS NOT INITIAL.
      manifest_json = get_manifest_json( app-bsp_name ).
    ENDIF.

    IF manifest_json IS NOT INITIAL.
      TRY.
          CREATE OBJECT manifest
            EXPORTING
              manifest_json = manifest_json.

          datasource = manifest->get_main_datasource( ).

*          IF datasource-uri IS NOT INITIAL.
*            result-service_uri = datasource-uri.
*            IF result-service_uri CP '*/' AND strlen( result-service_uri ) > 1.
*              result-service_uri = substring( val = result-service_uri len = strlen( result-service_uri ) - 1 ).
*            ENDIF.
*            SPLIT result-service_uri AT ';' INTO result-service_uri DATA(lv_dummy).
*            FIND REGEX '([^/]+)$' IN result-service_uri SUBMATCHES result-main_service_name.
*          ENDIF.
          IF datasource-uri IS NOT INITIAL.
            result-service_uri = datasource-uri.

            " Remove trailing slash if present
            IF result-service_uri CP '*/' AND strlen( result-service_uri ) > 1.
              result-service_uri = substring( val = result-service_uri len = strlen( result-service_uri ) - 1 ).
            ENDIF.

            " Remove everything after semicolon
            SPLIT result-service_uri AT ';' INTO result-service_uri DATA(lv_dummy).

            " Split URI into segments
            SPLIT result-service_uri AT '/' INTO TABLE DATA(lt_segments).

            " Start from last segment and go backwards until finding a non-numeric one
            LOOP AT lt_segments INTO DATA(lv_segment) FROM lines( lt_segments ) TO 1 STEP -1.
              " Check if segment contains non-numeric characters
              IF lv_segment CN '0123456789'.
                result-main_service_name = lv_segment.
                EXIT.
              ENDIF.
            ENDLOOP.

          ENDIF.

          entity_set = manifest->get_primary_entityset( ).

        CATCH cx_root.
      ENDTRY.
    ELSE.
      "if manifest not found, the fiori app doesn't exist in this s4 system
      CLEAR result.
      RETURN.
    ENDIF.

    DATA: segw_project TYPE /iwbep/i_sbd_sv-project,
          service_name TYPE string,
          services     TYPE TABLE OF string.

    IF result-main_service_name IS NOT INITIAL.
      service_name = result-main_service_name.
      SELECT SINGLE project
        FROM /iwbep/i_sbd_sv
        WHERE technical_name = @service_name
        INTO @segw_project.

      IF sy-subrc = 0.
        result-odata_version = '2.0'.
        result-segw_project  = segw_project.
      ELSEIF datasource-odata_version = '2.0'.
        "check if it's a service binding
        SELECT SINGLE service_name
                      FROM srvb_service_details
                      INTO @DATA(service_definition)
                      WHERE srvb_name = @service_name
                        AND version = 'A'.
        result-odata_version = '2.0'.
        IF sy-subrc NE 0.
          "probably deprecated app
          RETURN.
        ENDIF.
      ELSE.
        result-odata_version = '4.0'.
      ENDIF.
    ENDIF.

    name_c = entity_set.

    " for analytical apps / cds paramametrized view
    "Check if it ends with 'Results' and remove it
    IF name_c CP '*Results'.
      REPLACE FIRST OCCURRENCE OF REGEX 'Results$' IN name_c WITH ''.
    ENDIF.

    DATA(name_i) = c_to_i( name_c ).
    DATA(src_i)  = read_ddl_source( name_i ).
    DATA(src_c)  = read_ddl_source( name_c ).

    "if sources not found, search "alias"
    IF src_i IS INITIAL AND src_c IS INITIAL.
      CLEAR name_c.
      name_c =  find_alias( EXPORTING service_binding = result-main_service_name entity = entity_set ).
      IF name_c IS NOT INITIAL.
        name_i = c_to_i( name_c ).
        src_i  = read_ddl_source( name_i ).
        src_c  = read_ddl_source( name_c ).
      ENDIF.
    ENDIF.

    IF name_c IS INITIAL.
      result-programming_model = COND string(
        WHEN result-odata_version = '4.0'
        THEN model_rap
        ELSE model_na ).
      RETURN.
    ENDIF.

    classify(
      EXPORTING
        odata_version = result-odata_version
        name_c        = name_c
        name_i        = name_i
        src_c         = src_c
        src_i         = src_i
      IMPORTING
        model         = result-programming_model
        business_ent  = result-business_entity ).

    IF manifest_json IS NOT INITIAL.
      DATA: is_fe4    TYPE abap_bool,
            fe_ver4   TYPE string,
            fpm_flag4 TYPE string.
      detect_fe_fpm(
        EXPORTING
          manifest_json = manifest_json
        IMPORTING
          is_fe         = is_fe4
          fe_version    = fe_ver4
          fpm_flag      = fpm_flag4 ).
      result-fpm_extended = fpm_flag4.
    ELSE.
      result-fpm_extended = 'N/A'.
    ENDIF.

  ENDMETHOD.


METHOD classify.
  DATA: business_entity TYPE string VALUE '',
        model_type      TYPE string,
        r_entity        TYPE string.

  IF odata_version = '4.0'.
    IF src_i CS 'define root view entity'.
      model_type      = model_rap.
      business_entity = name_i.
    ELSEIF src_c CS 'define root view entity'.
      model_type      = model_rap.
      business_entity = name_c.
    ELSE.
      business_entity = COND string(
        WHEN src_i IS NOT INITIAL
        THEN name_i
        ELSE name_c ).
    ENDIF.

    " Check if it's a projection on R_ entity (RAP BO)
    IF business_entity IS NOT INITIAL.
      DATA(src_to_check) = COND string(
        WHEN src_c IS NOT INITIAL
        THEN src_c
        ELSE src_i ).

      " Look for 'as projection on R_' or 'from R_' pattern
      FIND PCRE 'projection\s+on\s+(R_[A-Za-z0-9_]+)' IN src_to_check
        SUBMATCHES r_entity IGNORING CASE.

      IF sy-subrc <> 0.
        " Try alternative pattern 'from R_'
        FIND PCRE 'from\s+(R_[A-Za-z0-9_]+)' IN src_to_check
          SUBMATCHES r_entity IGNORING CASE.
      ENDIF.

      " If R_ entity found, that's the real RAP BO
      IF sy-subrc = 0 AND r_entity IS NOT INITIAL.
        business_entity = r_entity.
        model_type = model_rap.
      ENDIF.
    ENDIF.

  ELSE. " OData V2
    IF src_i CS '@ObjectModel.modelCategory: #BOPF'
      OR src_c CS '@ObjectModel.modelCategory: #BOPF'.
      model_type = model_bopf.
      business_entity = COND string(
        WHEN src_i IS NOT INITIAL
        THEN name_i
        ELSE name_c ).
    ELSEIF src_c CS 'transactionalProcessingDelegated'.
      IF name_c CP 'C_*' AND src_c IS NOT INITIAL.
        DATA i_ref TYPE string.
        FIND PCRE 'from\s+(I_[A-Za-z0-9_]+)' IN src_c
          SUBMATCHES i_ref IGNORING CASE.
        IF sy-subrc = 0 AND i_ref IS NOT INITIAL.
          DATA(src_i_ref) = read_ddl_source( i_ref ).
          IF src_i_ref CS 'transactionalProcessingEnabled'
            OR src_i_ref CS 'writeDraftPersistence'.
            model_type      = model_bopf.
            business_entity = i_ref.
            model           = model_type.
            business_ent    = business_entity.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSEIF src_i CS 'define root view entity'
      OR src_c CS 'define root view entity'.
      model_type = model_rap.
      business_entity = COND string(
        WHEN src_i IS NOT INITIAL
        THEN name_i
        ELSE name_c ).
    ELSEIF ( src_c CS '@VDM.viewType: #CONSUMPTION'
          OR src_i CS '@VDM.viewType: #CONSUMPTION' )
        OR ( ( src_c CS '@Metadata.allowExtensions'
            OR src_i CS '@Metadata.allowExtensions' )
         AND ( name_c CP 'C_*' OR name_i CP 'C_*' ) ).
      model_type = model_rap.
      business_entity = COND string(
        WHEN src_c IS NOT INITIAL
        THEN name_c
        ELSE name_i ).
    ELSE.
      model_type      = model_na.
      business_entity = name_c.
    ENDIF.
  ENDIF.

  model        = model_type.
  business_ent = business_entity.
ENDMETHOD.


  METHOD c_to_i.
    cds_i = cds_c.
    IF cds_i CP 'C_*'.
      REPLACE FIRST OCCURRENCE OF 'C_' IN cds_i WITH 'I_'.
    ENDIF.
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


  METHOD detect_odata_version.
    DATA: ver_root TYPE string,
          ns_edmx  TYPE string.

    FIND PCRE '<edmx:Edmx[^>]*Version="([^"]+)"[^>]*xmlns:edmx="([^"]+)"'
      IN xml
      SUBMATCHES ver_root ns_edmx.

    IF sy-subrc = 0.
      IF ver_root CP '4*'
        OR ns_edmx CS 'docs.oasis-open.org/odata/ns/edmx'.
        version = '4.0'.
        RETURN.
      ELSE.
        version = '2.0'.
        RETURN.
      ENDIF.
    ENDIF.

    IF xml CS 'm:DataServiceVersion="2.0"'
      OR xml CS 'DataServiceVersion="2.0"'.
      version = '2.0'.
      RETURN.
    ENDIF.

    FIND PCRE '<Schema[^>]*xmlns="([^"]+)"' IN xml
      SUBMATCHES ns_edmx.
    IF sy-subrc = 0.
      IF ns_edmx CS 'docs.oasis-open.org/odata/ns/edm'.
        version = '4.0'.
        RETURN.
      ELSEIF ns_edmx CS 'schemas.microsoft.com/ado/2008/09/edm'.
        version = '2.0'.
        RETURN.
      ENDIF.
    ENDIF.

    version = '2.0'.
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


  METHOD get_metadata_xml.
    DATA: root   TYPE string,
          client TYPE REF TO if_http_client,
          status TYPE i.

    root = ||.
    root = service_name.

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

    client->request->set_header_field(
      name  = '~request_method'
      value = 'GET' ).
    client->request->set_header_field(
      name  = '~request_uri'
      value = root && '$metadata' ).
    client->send( ).
    client->receive( ).

    client->response->get_status( IMPORTING code = status ).
    IF status = 200.
      xml = client->response->get_cdata( ).
    ENDIF.
  ENDMETHOD.


  METHOD guess_cds_c_from_metadata.
    DATA first TYPE string.

    FIND PCRE '<EntitySet[^>]*Name="([^"]+)"'
      IN xml
      SUBMATCHES first.

    IF sy-subrc = 0.
      IF first CP 'C_*' OR first CP '*TP'.
        cds_c = first.
        RETURN.
      ENDIF.
    ENDIF.

    FIND PCRE '<EntitySet[^>]*Name="(C_[A-Za-z0-9_]+)"'
      IN xml
      SUBMATCHES cds_c.

    IF sy-subrc <> 0.
      cds_c = first.
    ENDIF.
  ENDMETHOD.


  METHOD read_ddl_source.
    data: ddlname_to_upper type string.
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
          service_bnd type SRVB_NAME.
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
ENDCLASS.
