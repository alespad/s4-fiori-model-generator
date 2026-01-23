CLASS zcl_fiori_cust_model_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF result,
        bspname          TYPE c LENGTH 30,
        devclass         TYPE c LENGTH 30,
        author           TYPE c LENGTH 12,
        programmingmodel TYPE c LENGTH 10,
        odataversion     TYPE c LENGTH 5,
        businessentity   TYPE c LENGTH 80,
        mainservicename  TYPE c LENGTH 80,
        serviceuri       TYPE c LENGTH 255,
        segwproject      TYPE c LENGTH 40,
        fpmextended      TYPE c LENGTH 5,
        appname          TYPE c LENGTH 255,
      END OF result.

    TYPES result_table TYPE STANDARD TABLE OF result WITH EMPTY KEY.

    CLASS-METHODS get_bsp_list
      IMPORTING
        filter_bsp_name TYPE string OPTIONAL
        filter_author   TYPE string OPTIONAL
        filter_devclass TYPE string OPTIONAL
      RETURNING
        VALUE(result)   TYPE result_table.

    CLASS-METHODS analyze_bsp
      IMPORTING
        bsp_name      TYPE clike
        devclass      TYPE clike
        author        TYPE clike
      RETURNING
        VALUE(result) TYPE result.

ENDCLASS.



CLASS zcl_fiori_cust_model_query IMPLEMENTATION.


  METHOD if_rap_query_provider~select.

    DATA: results        TYPE result_table,
          filter_bsp     TYPE string,
          filter_auth    TYPE string,
          filter_package TYPE string.

    " Get filter conditions
    TRY.
        DATA(filter_conditions) = request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range.
        CLEAR filter_conditions.
    ENDTRY.

    " Extract filter values
    LOOP AT filter_conditions ASSIGNING FIELD-SYMBOL(<filter>).
      CASE <filter>-name.
        WHEN 'BSPNAME'.
          IF lines( <filter>-range ) > 0.
            filter_bsp = <filter>-range[ 1 ]-low.
          ENDIF.
        WHEN 'AUTHOR'.
          IF lines( <filter>-range ) > 0.
            filter_auth = <filter>-range[ 1 ]-low.
          ENDIF.
        WHEN 'DEVCLASS'.
          IF lines( <filter>-range ) > 0.
            filter_package = <filter>-range[ 1 ]-low.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    " Get BSP list and analyze
    results = get_bsp_list(
      filter_bsp_name = filter_bsp
      filter_author   = filter_auth
      filter_devclass = filter_package ).

    " Handle paging
    DATA(offset) = request->get_paging( )->get_offset( ).
    DATA(page_size) = request->get_paging( )->get_page_size( ).

    IF page_size > 0.
      DATA(max_index) = offset + page_size.
      IF max_index > lines( results ).
        max_index = lines( results ).
      ENDIF.
    ELSE.
      max_index = lines( results ).
    ENDIF.

    " Apply paging
    IF offset > 0 OR page_size > 0.
      DATA paged_results TYPE result_table.
      LOOP AT results ASSIGNING FIELD-SYMBOL(<res>) FROM ( offset + 1 ) TO max_index.
        APPEND <res> TO paged_results.
      ENDLOOP.
      results = paged_results.
    ENDIF.

    " Set total count if requested
    IF request->is_total_numb_of_rec_requested( ).
      response->set_total_number_of_records( lines( results ) ).
    ENDIF.

    " Return data
    response->set_data( results ).

  ENDMETHOD.


  METHOD get_bsp_list.

    DATA: tadir_entries TYPE STANDARD TABLE OF tadir,
          res           TYPE result.

    " Build dynamic WHERE clause
    DATA: where_clause TYPE string.

    where_clause = |PGMID = 'R3TR' AND OBJECT = 'WAPA'|.

    " If no filter specified, default to custom BSPs only (Z* and Y*)
    IF filter_bsp_name IS INITIAL AND filter_author IS INITIAL AND filter_devclass IS INITIAL.
      where_clause = where_clause && | AND ( OBJ_NAME LIKE 'Z%' OR OBJ_NAME LIKE 'Y%' )|.
    ENDIF.

    IF filter_bsp_name IS NOT INITIAL.
      DATA(bsp_upper) = to_upper( filter_bsp_name ).
      IF bsp_upper CS '*'.
        REPLACE ALL OCCURRENCES OF '*' IN bsp_upper WITH '%'.
        where_clause = where_clause && | AND OBJ_NAME LIKE '{ bsp_upper }'|.
      ELSE.
        where_clause = where_clause && | AND OBJ_NAME = '{ bsp_upper }'|.
      ENDIF.
    ENDIF.

    IF filter_author IS NOT INITIAL.
      DATA(author_upper) = to_upper( filter_author ).
      IF author_upper CS '*'.
        REPLACE ALL OCCURRENCES OF '*' IN author_upper WITH '%'.
        where_clause = where_clause && | AND AUTHOR LIKE '{ author_upper }'|.
      ELSE.
        where_clause = where_clause && | AND AUTHOR = '{ author_upper }'|.
      ENDIF.
    ENDIF.

    IF filter_devclass IS NOT INITIAL.
      DATA(devclass_upper) = to_upper( filter_devclass ).
      IF devclass_upper CS '*'.
        REPLACE ALL OCCURRENCES OF '*' IN devclass_upper WITH '%'.
        where_clause = where_clause && | AND DEVCLASS LIKE '{ devclass_upper }'|.
      ELSE.
        where_clause = where_clause && | AND DEVCLASS = '{ devclass_upper }'|.
      ENDIF.
    ENDIF.

    " Select BSP applications from TADIR
    SELECT obj_name, devclass, author
      FROM tadir
      WHERE (where_clause)
      INTO TABLE @DATA(bsp_list).

    " Analyze each BSP
    LOOP AT bsp_list ASSIGNING FIELD-SYMBOL(<bsp>).
      res = analyze_bsp(
        bsp_name = <bsp>-obj_name
        devclass = <bsp>-devclass
        author   = <bsp>-author ).

      " Only add if manifest was found (valid Fiori app)
      IF res-bspname IS NOT INITIAL.
        TRANSLATE res TO UPPER CASE.
        APPEND res TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD analyze_bsp.

    DATA: app             TYPE zcl_fiori_model_analyzer=>app,
          analyzer_result TYPE zcl_fiori_model_analyzer=>result.

    " Clear result
    CLEAR result.

    " Set up input for analyzer
    app-bsp_name = bsp_name.
    app-fiori_id = bsp_name.  " Use BSP name as ID for custom apps
    app-app_name = bsp_name.
    CLEAR app-library_link.   " No library link for custom apps

    " Call existing analyzer
    analyzer_result = zcl_fiori_model_analyzer=>analyze_app( app ).

    " Check if analysis was successful (manifest found)
    IF analyzer_result IS INITIAL.
      RETURN.
    ENDIF.

    " Map result
    result-bspname          = bsp_name.
    result-devclass         = devclass.
    result-author           = author.
    result-programmingmodel = analyzer_result-programming_model.
    result-odataversion     = analyzer_result-odata_version.
    result-businessentity   = analyzer_result-business_entity.
    result-mainservicename  = analyzer_result-main_service_name.
    result-serviceuri       = analyzer_result-service_uri.
    result-segwproject      = analyzer_result-segw_project.
    result-fpmextended      = analyzer_result-fpm_extended.
    result-appname          = analyzer_result-app_name.

  ENDMETHOD.

ENDCLASS.
