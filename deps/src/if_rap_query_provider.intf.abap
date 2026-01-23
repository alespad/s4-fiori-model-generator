INTERFACE if_rap_query_provider PUBLIC.
  METHODS select
    IMPORTING
      request  TYPE REF TO if_rap_query_request
      response TYPE REF TO if_rap_query_response
    RAISING
      cx_rap_query_provider.
ENDINTERFACE.
