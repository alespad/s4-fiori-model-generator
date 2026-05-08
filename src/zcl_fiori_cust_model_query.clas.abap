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
    TYPES rng_bspname  TYPE RANGE OF tadir-obj_name.
    TYPES rng_author   TYPE RANGE OF tadir-author.
    TYPES rng_devclass TYPE RANGE OF tadir-devclass.

    CLASS-METHODS get_bsp_list
      IMPORTING
        it_rng_bspname  TYPE rng_bspname  OPTIONAL
        it_rng_author   TYPE rng_author   OPTIONAL
        it_rng_devclass TYPE rng_devclass OPTIONAL
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

    DATA: rng_bsp     TYPE rng_bspname,
          rng_auth    TYPE rng_author,
          rng_package TYPE rng_devclass.

    TRY.
        DATA(filter_conditions) = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range.
        CLEAR filter_conditions.
    ENDTRY.

    " Convert OData string ranges to typed ABAP range tables
    LOOP AT filter_conditions ASSIGNING FIELD-SYMBOL(<cond>).
      LOOP AT <cond>-range ASSIGNING FIELD-SYMBOL(<rng>).
        CASE <cond>-name.
          WHEN 'BSPNAME'.
            APPEND VALUE #( sign = <rng>-sign  option = <rng>-option
                            low  = to_upper( <rng>-low )
                            high = to_upper( <rng>-high ) ) TO rng_bsp.
          WHEN 'AUTHOR'.
            APPEND VALUE #( sign = <rng>-sign  option = <rng>-option
                            low  = to_upper( <rng>-low )
                            high = to_upper( <rng>-high ) ) TO rng_auth.
          WHEN 'DEVCLASS'.
            APPEND VALUE #( sign = <rng>-sign  option = <rng>-option
                            low  = to_upper( <rng>-low )
                            high = to_upper( <rng>-high ) ) TO rng_package.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    DATA(results) = get_bsp_list(
      it_rng_bspname  = rng_bsp
      it_rng_author   = rng_auth
      it_rng_devclass = rng_package ).

    " Handle paging
    DATA(offset)    = io_request->get_paging( )->get_offset( ).
    DATA(page_size) = io_request->get_paging( )->get_page_size( ).

    IF page_size > 0.
      DATA(max_index) = offset + page_size.
      IF max_index > lines( results ).
        max_index = lines( results ).
      ENDIF.
    ELSE.
      max_index = lines( results ).
    ENDIF.

    IF offset > 0 OR page_size > 0.
      DATA paged_results TYPE result_table.
      LOOP AT results ASSIGNING FIELD-SYMBOL(<res>) FROM ( offset + 1 ) TO max_index.
        APPEND <res> TO paged_results.
      ENDLOOP.
      results = paged_results.
    ENDIF.

    IF io_request->is_total_numb_of_rec_requested( ).
      io_response->set_total_number_of_records( lines( results ) ).
    ENDIF.

    io_response->set_data( results ).

  ENDMETHOD.


  METHOD get_bsp_list.

    DATA lrng_bsp      TYPE rng_bspname.
    DATA lrng_auth     TYPE rng_author.
    DATA lrng_devclass TYPE rng_devclass.

    lrng_bsp      = it_rng_bspname.
    lrng_auth     = it_rng_author.
    lrng_devclass = it_rng_devclass.

    " No BspName filter → always restrict to custom namespace to avoid full system scan
    IF lrng_bsp IS INITIAL.
      lrng_bsp = VALUE #(
        ( sign = 'I' option = 'CP' low = 'Z*' )
        ( sign = 'I' option = 'CP' low = 'Y*' ) ).
    ENDIF.

    SELECT obj_name, devclass, author
      FROM tadir
      WHERE pgmid    = 'R3TR'
        AND object   = 'WAPA'
        AND obj_name IN @lrng_bsp
        AND author   IN @lrng_auth
        AND devclass IN @lrng_devclass
      INTO TABLE @DATA(bsp_list).

    LOOP AT bsp_list ASSIGNING FIELD-SYMBOL(<bsp>).
      DATA(res) = analyze_bsp(
        bsp_name = <bsp>-obj_name
        devclass = <bsp>-devclass
        author   = <bsp>-author ).

      IF res-bspname IS NOT INITIAL.
        TRANSLATE res TO UPPER CASE.
        APPEND res TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD analyze_bsp.

    DATA: app             TYPE zcl_fiori_model_analyzer=>app,
          analyzer_result TYPE zcl_fiori_model_analyzer=>result.

    CLEAR result.

    app-bsp_name = bsp_name.
    app-fiori_id = bsp_name.
    app-app_name = bsp_name.
    CLEAR app-library_link.

    analyzer_result = zcl_fiori_model_analyzer=>analyze_app( app ).

    IF analyzer_result IS INITIAL.
      RETURN.
    ENDIF.

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
