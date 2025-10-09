class ZCL_FIORI_MODEL_MANIFEST definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF main_datasource_info,
        datasource_id TYPE string,
        uri           TYPE string,
        odata_version TYPE string,
        is_v4         TYPE abap_bool,
      END OF main_datasource_info .
  types:
    entityset_table TYPE STANDARD TABLE OF string WITH EMPTY KEY .

  methods CONSTRUCTOR
    importing
      !MANIFEST_JSON type STRING .
  methods GET_MAIN_DATASOURCE
    returning
      value(RESULT) type MAIN_DATASOURCE_INFO .
  methods GET_PRIMARY_ENTITYSET
    returning
      value(ENTITYSET) type STRING .
  PRIVATE SECTION.
    DATA manifest_json TYPE string.

    METHODS find_value_after_key
      IMPORTING
        key           TYPE string
        source        TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS extract_json_object
      IMPORTING
        json          TYPE string
        start_key     TYPE string
        start_pos     TYPE i
      RETURNING
        VALUE(result) TYPE string.

    METHODS find_datasource_in_models
      IMPORTING
        models_section TYPE string
      RETURNING
        VALUE(result)  TYPE string.

    METHODS find_first_odata_datasource
      RETURNING
        VALUE(result) TYPE string.

    METHODS extract_datasource_section
      IMPORTING
        datasource_id TYPE string
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS ZCL_FIORI_MODEL_MANIFEST IMPLEMENTATION.


  METHOD constructor.
    me->manifest_json = manifest_json.
  ENDMETHOD.


  METHOD find_value_after_key.
    DATA: key_pattern  TYPE string,
          key_pos      TYPE i,
          value_start  TYPE i,
          value_end    TYPE i,
          quote_pos    TYPE i,
          length       TYPE i.

    key_pattern = '"' && key && '"'.

    FIND FIRST OCCURRENCE OF key_pattern IN source
         MATCH OFFSET key_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    value_start = key_pos + strlen( key_pattern ).

    FIND FIRST OCCURRENCE OF ':' IN SECTION OFFSET value_start OF source
         MATCH OFFSET value_start.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    value_start = value_start + 1.

    FIND FIRST OCCURRENCE OF '"' IN SECTION OFFSET value_start OF source
         MATCH OFFSET quote_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    value_start = quote_pos + 1.

    FIND FIRST OCCURRENCE OF '"' IN SECTION OFFSET value_start OF source
         MATCH OFFSET value_end.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    length = value_end - value_start.
    IF length > 0.
      result = substring( val = source off = value_start len = length ).
    ENDIF.
  ENDMETHOD.


  METHOD get_main_datasource.
    DATA: datasource_id  TYPE string,
          models_section TYPE string,
          ds_section     TYPE string,
          start_pos      TYPE i.

    " Try to find dataSource from models section
    FIND FIRST OCCURRENCE OF '"models"' IN manifest_json
         MATCH OFFSET start_pos.

    IF sy-subrc = 0.
      models_section = extract_json_object(
        json       = manifest_json
        start_key  = '"models"'
        start_pos  = start_pos ).

      IF models_section IS NOT INITIAL.
        datasource_id = find_datasource_in_models( models_section ).
      ENDIF.
    ENDIF.

    " Fallback: Look directly in dataSources
    IF datasource_id IS INITIAL.
      datasource_id = find_first_odata_datasource( ).
    ENDIF.

    IF datasource_id IS INITIAL.
      RETURN.
    ENDIF.

    " Extract datasource details
    ds_section = extract_datasource_section( datasource_id ).

    IF ds_section IS INITIAL.
      RETURN.
    ENDIF.

    " Parse properties
    result-datasource_id = datasource_id.
    result-uri = find_value_after_key(
      key    = 'uri'
      source = ds_section ).
    result-odata_version = find_value_after_key(
      key    = 'odataVersion'
      source = ds_section ).

    IF result-odata_version IS INITIAL.
      result-odata_version = '2.0'.
    ENDIF.

    result-is_v4 = xsdbool( result-odata_version = '4.0' ).
  ENDMETHOD.


METHOD get_primary_entityset.
  DATA: pos            TYPE i,
        generic_app    TYPE string.

  " Always search within "sap.ui.generic.app" section
  FIND FIRST OCCURRENCE OF '"sap.ui.generic.app"' IN manifest_json
       MATCH OFFSET pos.

  IF sy-subrc = 0.
    generic_app = substring( val = manifest_json off = pos ).
  ELSE.
    " Fallback to entire manifest
    generic_app = manifest_json.
  ENDIF.

  " Strategy: Find FIRST "entitySet" value in the generic app section
  " This works for both array and object format
  entityset = find_value_after_key(
    key    = 'entitySet'
    source = generic_app ).

  " If not found, try in entire manifest
  IF entityset IS INITIAL.
    entityset = find_value_after_key(
      key    = 'entitySet'
      source = manifest_json ).
  ENDIF.
ENDMETHOD.


  METHOD extract_datasource_section.
    DATA: search TYPE string,
          pos    TYPE i.

    search = '"' && datasource_id && '"'.

    FIND FIRST OCCURRENCE OF '"dataSources"' IN manifest_json
         MATCH OFFSET pos.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(datasources_section) = extract_json_object(
      json       = manifest_json
      start_key  = '"dataSources"'
      start_pos  = pos ).

    FIND FIRST OCCURRENCE OF search IN datasources_section
         MATCH OFFSET pos.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = extract_json_object(
      json       = datasources_section
      start_key  = search
      start_pos  = pos ).
  ENDMETHOD.


  METHOD extract_json_object.
    DATA: start_pos_local TYPE i,
          end_pos         TYPE i,
          brace_count     TYPE i,
          char            TYPE c LENGTH 1,
          in_string       TYPE abap_bool,
          json_len        TYPE i,
          length          TYPE i.

    json_len = strlen( json ).
    start_pos_local = start_pos.

    FIND FIRST OCCURRENCE OF start_key
         IN SECTION OFFSET start_pos_local OF json
         MATCH OFFSET start_pos_local.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    FIND FIRST OCCURRENCE OF '{'
         IN SECTION OFFSET start_pos_local OF json
         MATCH OFFSET start_pos_local.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    brace_count = 1.
    end_pos = start_pos_local + 1.
    in_string = abap_false.

    WHILE end_pos < json_len AND brace_count > 0.
      char = substring( val = json off = end_pos len = 1 ).

      IF char = '"' AND end_pos > 0.
        DATA prev TYPE c LENGTH 1.
        prev = substring( val = json off = end_pos - 1 len = 1 ).
        IF prev <> '\'.
          in_string = xsdbool( in_string = abap_false ).
        ENDIF.
      ENDIF.

      IF in_string = abap_false.
        IF char = '{'.
          brace_count = brace_count + 1.
        ELSEIF char = '}'.
          brace_count = brace_count - 1.
        ENDIF.
      ENDIF.

      end_pos = end_pos + 1.
    ENDWHILE.

    length = end_pos - start_pos_local.
    result = substring( val = json off = start_pos_local len = length ).
  ENDMETHOD.


  METHOD find_datasource_in_models.
    DATA: pos        TYPE i,
          section    TYPE string,
          datasource TYPE string.

    " Try unnamed default model ""
    FIND FIRST OCCURRENCE OF '""' IN models_section
         MATCH OFFSET pos.

    IF sy-subrc = 0.
      section = substring( val = models_section off = pos ).
      datasource = find_value_after_key(
        key    = 'dataSource'
        source = section ).

      IF datasource IS NOT INITIAL.
        result = datasource.
        RETURN.
      ENDIF.
    ENDIF.

    " Try any dataSource in models
    result = find_value_after_key(
      key    = 'dataSource'
      source = models_section ).
  ENDMETHOD.


METHOD find_first_odata_datasource.
  DATA: datasources_section TYPE string,
        pos                 TYPE i,
        uri_pos             TYPE i,
        start_pos           TYPE i,
        brace_start         TYPE i,
        candidate           TYPE string,
        length              TYPE i.

  " Find "dataSources" section
  FIND FIRST OCCURRENCE OF '"dataSources"' IN manifest_json
       MATCH OFFSET pos.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  datasources_section = extract_json_object(
    json       = manifest_json
    start_key  = '"dataSources"'
    start_pos  = pos ).

  IF datasources_section IS INITIAL.
    RETURN.
  ENDIF.

  " Strategy: Find first "uri" occurrence, then backtrack to find the datasource key
  FIND FIRST OCCURRENCE OF '"uri"' IN datasources_section
       MATCH OFFSET uri_pos.

  IF sy-subrc <> 0.
    RETURN. " No URI found
  ENDIF.

  " Backtrack from uri position to find the datasource key
  " Look for the pattern: "key": { ... "uri": ...
  " We need to find the opening brace before "uri"

  start_pos = uri_pos - 1.

  " Go back to find opening brace
  WHILE start_pos > 0.
    DATA(char) = substring( val = datasources_section
                            off = start_pos
                            len = 1 ).
    IF char = '{'.
      brace_start = start_pos.
      EXIT.
    ENDIF.
    start_pos = start_pos - 1.
  ENDWHILE.

  IF brace_start = 0.
    RETURN.
  ENDIF.

  " Now go back from the brace to find the key (quoted string before colon)
  start_pos = brace_start - 1.
  DATA(colon_pos) = 0.

  WHILE start_pos > 0.
    char = substring( val = datasources_section
                      off = start_pos
                      len = 1 ).
    IF char = ':'.
      colon_pos = start_pos.
      EXIT.
    ENDIF.
    start_pos = start_pos - 1.
  ENDWHILE.

  IF colon_pos = 0.
    RETURN.
  ENDIF.

  " Find closing quote before colon
  start_pos = colon_pos - 1.
  DATA(end_quote_pos) = 0.

  WHILE start_pos > 0.
    char = substring( val = datasources_section
                      off = start_pos
                      len = 1 ).
    IF char = '"'.
      end_quote_pos = start_pos.
      EXIT.
    ENDIF.
    start_pos = start_pos - 1.
  ENDWHILE.

  IF end_quote_pos = 0.
    RETURN.
  ENDIF.

  " Find opening quote
  start_pos = end_quote_pos - 1.
  DATA(start_quote_pos) = 0.

  WHILE start_pos > 0.
    char = substring( val = datasources_section
                      off = start_pos
                      len = 1 ).
    IF char = '"'.
      start_quote_pos = start_pos.
      EXIT.
    ENDIF.
    start_pos = start_pos - 1.
  ENDWHILE.

  IF start_quote_pos = 0.
    RETURN.
  ENDIF.

  " Extract the datasource key
  length = end_quote_pos - start_quote_pos - 1.
  IF length > 0.
    result = substring(
      val = datasources_section
      off = start_quote_pos + 1
      len = length ).
  ENDIF.
ENDMETHOD.
ENDCLASS.
