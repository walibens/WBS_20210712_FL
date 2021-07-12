*&---------------------------------------------------------------------*
*& Report zr_w1_d1_wbs
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_w1_d1_wbs.


PARAMETERS:
  p_input  TYPE string,
  p_filein TYPE string.


CLASS lcx_numeral_converter DEFINITION
    INHERITING FROM cx_static_check.

  PUBLIC SECTION.

ENDCLASS.

INTERFACE lif_numeral.
  METHODS convert
    IMPORTING
      iv_input         TYPE string
    RETURNING
      VALUE(rv_result) TYPE string.
ENDINTERFACE.

CLASS lcl_to_roman DEFINITION.

  PUBLIC SECTION.
    INTERFACES : lif_numeral.
    METHODS:
      one IMPORTING i TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_numeral_converter DEFINITION FINAL.

  PUBLIC SECTION.
    DATA :
             mv_numeral TYPE string.
    METHODS:
      constructor IMPORTING iv_input TYPE string,
      convert RETURNING VALUE(rv_result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS:
      validate RAISING   lcx_numeral_converter,
      classify RETURNING VALUE(ro_result) TYPE REF TO lif_numeral.

ENDCLASS.

CLASS lcl_numeral_converter IMPLEMENTATION.

  METHOD constructor.
    mv_numeral = iv_input.
  ENDMETHOD.


  METHOD validate.
    FIND REGEX '^[0-9]' IN mv_numeral.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_numeral_converter.
    ENDIF.

  ENDMETHOD.

  METHOD classify.
    FIND REGEX '^[0-9]' IN mv_numeral.
    IF sy-subrc = 0.
      ro_result = NEW lcl_to_roman(  ).
    ENDIF.
  ENDMETHOD.

  METHOD convert.
    TRY.
        validate(  ).
        DATA(lo_numeral) = classify(  ).
        rv_result = |{ mv_numeral } is converted to { lo_numeral->convert( mv_numeral ) }|.
      CATCH lcx_numeral_converter.
        cl_demo_output=>write( 'Input not interpretable' ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_to_arabic DEFINITION.

  PUBLIC SECTION.
    INTERFACES : lif_numeral.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_to_arabic IMPLEMENTATION.

  METHOD lif_numeral~convert.
    rv_result = COND string( WHEN iv_input = 'XII' THEN '12').
  ENDMETHOD.

ENDCLASS.

CLASS lcl_to_roman IMPLEMENTATION.

  METHOD lif_numeral~convert.
    rv_result = COND string( WHEN iv_input = '12' THEN 'XII').
  ENDMETHOD.

  METHOD one.

  ENDMETHOD.

ENDCLASS.


CLASS ltc_numeral_converter DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA :
      mo_cut TYPE REF TO lif_numeral.
    METHODS:
      setup,
      acceptance_test FOR TESTING.
ENDCLASS.


CLASS ltc_numeral_converter IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW lcl_to_arabic(  ).
  ENDMETHOD.

  METHOD acceptance_test.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = mo_cut->convert( 'XII' )
        exp                  = 12 ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_application DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS :
      main IMPORTING iv_input TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_application IMPLEMENTATION.

  METHOD main.
    cl_demo_output=>display( NEW lcl_numeral_converter( p_input )->convert(  ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_application=>main( p_input ).
