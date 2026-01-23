INTERFACE if_rap_query_paging PUBLIC.
  METHODS get_offset
    RETURNING
      VALUE(result) TYPE int8.
  METHODS get_page_size
    RETURNING
      VALUE(result) TYPE int8.
ENDINTERFACE.
