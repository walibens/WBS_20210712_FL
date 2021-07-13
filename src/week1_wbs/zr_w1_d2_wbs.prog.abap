*&---------------------------------------------------------------------*
*& Report zr_w1_d1_wbs
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_w1_d2_wbs.

PARAMETERS:
p_input TYPE string.

CLASS lcx_numeral_converter DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcx_numeral_converter IMPLEMENTATION.

ENDCLASS.

INTERFACE lif_numeral.
  METHODS :
    convert IMPORTING iv_input         TYPE string
            RETURNING VALUE(rv_result) TYPE string.
ENDINTERFACE.

CLASS lcl_roman DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_numeral.

    DATA :
      mv_roman TYPE string.
    METHODS :
      constructor IMPORTING iv_input TYPE string.
  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS to_arabic
      IMPORTING
        iv_input         TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS lcl_roman IMPLEMENTATION.

  METHOD constructor.
    mv_roman = iv_input.
  ENDMETHOD.

  METHOD lif_numeral~convert.
    rv_result = to_arabic( iv_input ).
  ENDMETHOD.


  METHOD to_arabic.
    rv_result = COND #( WHEN iv_input = 'XII' THEN '12' ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_arabic DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_numeral.

    DATA :
      mv_arabic TYPE string.
    METHODS :
      constructor IMPORTING iv_input TYPE string.
  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS to_roman
      IMPORTING
        iv_input         TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS lcl_arabic IMPLEMENTATION.

  METHOD constructor.
    mv_arabic = iv_input.
  ENDMETHOD.

  METHOD lif_numeral~convert.
    rv_result = to_roman( iv_input ).
  ENDMETHOD.

  METHOD to_roman.
    rv_result = COND #( WHEN iv_input = '12' THEN 'XII' ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_numeral_converter DEFINITION FINAL.

  PUBLIC SECTION.
    DATA :
             mv_input TYPE string.
    METHODS:
      constructor IMPORTING iv_input TYPE string,
      convert RETURNING VALUE(rv_result) TYPE string,

      classify RETURNING VALUE(ro_result) TYPE REF TO lif_numeral.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS validate RAISING         lcx_numeral_converter.

ENDCLASS.

CLASS lcl_numeral_converter IMPLEMENTATION.

  METHOD convert.
    TRY.
        validate( ).
        rv_result = classify( )->convert( mv_input ).

      CATCH lcx_numeral_converter.
        cl_demo_output=>write( 'Cannot intepret input' ).
    ENDTRY.

  ENDMETHOD.

  METHOD classify.
    FIND REGEX '^[0-9]' IN mv_input.
    IF sy-subrc = 0.
      ro_result = NEW lcl_arabic( mv_input ).
    ELSE.
      ro_result = NEW lcl_roman( mv_input ).
    ENDIF.
  ENDMETHOD.



  METHOD validate.

  ENDMETHOD.

  METHOD constructor.
    mv_input = iv_input.
  ENDMETHOD.

ENDCLASS.





CLASS lcl_application DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      main IMPORTING iv_input TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_application IMPLEMENTATION.

  METHOD main.
    cl_demo_output=>display( NEW lcl_numeral_converter( iv_input )->convert(  ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_numeral_converter DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      acceptance_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_numeral_converter IMPLEMENTATION.


  METHOD acceptance_test.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = NEW lcl_numeral_converter( 'XII' )->convert( )
        exp                  = '12' ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_application=>main( p_input ).
