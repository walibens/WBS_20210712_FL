REPORT zr_w1_d4_wbs.

SELECTION-SCREEN BEGIN OF BLOCK b1.
PARAMETERS :
  p_input TYPE string,
  p_file  TYPE string.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcx_numeral_converter DEFINITION INHERITING FROM cx_static_check.

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF lcx_numeral_converter,
        msgid TYPE symsgid VALUE 'ZACIS',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE 'MV_MSGV4',
      END OF lcx_numeral_converter .
    DATA:mv_msgv1 TYPE msgv1,
         mv_msgv2 TYPE msgv2,
         mv_msgv3 TYPE msgv3,
         mv_msgv4 TYPE msgv4.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !iv_msgv1 TYPE msgv1 OPTIONAL
        !iv_msgv2 TYPE msgv2 OPTIONAL
        !iv_msgv3 TYPE msgv3 OPTIONAL
        !iv_msgv4 TYPE msgv4 OPTIONAL .

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcx_numeral_converter IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    mv_msgv1 = iv_msgv1 .
    mv_msgv2 = iv_msgv2 .
    mv_msgv3 = iv_msgv3 .
    mv_msgv4 = iv_msgv4 .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = lcx_numeral_converter .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

INTERFACE lif_file_port.
  METHODS:
    read_file IMPORTING iv_filepath           TYPE string
              RETURNING VALUE(et_filecontent) TYPE table_of_strings,
    write_file IMPORTING iv_filepath     TYPE string
                         et_filecontent  TYPE string
               RETURNING VALUE(rv_is_ok) TYPE abap_bool.
ENDINTERFACE.

CLASS lcl_file_adapter DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_file_port.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_file_adapter IMPLEMENTATION.

  METHOD lif_file_port~read_file.

    DATA lt_file_content TYPE table_of_strings.
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = space            " Name of file
      CHANGING
        data_tab                = lt_file_content                 " Transfer table for file contents
      EXCEPTIONS
        file_open_error         = 1                " File does not exist and cannot be opened
        file_read_error         = 2                " Error when reading file
        no_batch                = 3                " Cannot execute front-end function in background
        gui_refuse_filetransfer = 4                " Incorrect front end or error on front end
        invalid_type            = 5                " Incorrect parameter FILETYPE
        no_authority            = 6                " No upload authorization
        unknown_error           = 7                " Unknown error
        bad_data_format         = 8                " Cannot Interpret Data in File
        header_not_allowed      = 9                " Invalid header
        separator_not_allowed   = 10               " Invalid separator
        header_too_long         = 11               " Header information currently restricted to 1023 bytes
        unknown_dp_error        = 12               " Error when calling data provider
        access_denied           = 13               " Access to File Denied
        dp_out_of_memory        = 14               " Not enough memory in data provider
        disk_full               = 15               " Storage medium is full.
        dp_timeout              = 16               " Data provider timeout
        not_supported_by_gui    = 17               " GUI does not support this
        error_no_gui            = 18               " GUI not available
        OTHERS                  = 19
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD lif_file_port~write_file.

  ENDMETHOD.

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
    rv_result = SWITCH #( iv_input
                            WHEN  'I' THEN '1'
                            WHEN  'II' THEN '2'
                            WHEN  'III' THEN '3'
                            WHEN  'IV' THEN '4'
                            ELSE 'Not handled' ).
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
    rv_result = SWITCH #( iv_input
                            WHEN  '1' THEN 'I'
                            WHEN  '2' THEN 'II'
                            WHEN  '3' THEN 'III'
                            WHEN  '4' THEN 'IV'
                            ELSE 'Not handled' ).
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
    METHODS get_stream importing iv_filename type string
    RETURNING VALUE(rt_file_content) type table_of_strings.

ENDCLASS.

CLASS lcl_numeral_converter IMPLEMENTATION.

  METHOD get_stream.
    DATA lo_file_port TYPE REF TO lif_file_port.
    lo_file_port = NEW lcl_file_adapter(  ).
    rt_file_content = lo_file_port->read_file( iv_filename ).
  ENDMETHOD.

  METHOD convert.
    TRY.

        validate( ).
        rv_result = classify( )->convert( mv_input ).

      CATCH lcx_numeral_converter INTO DATA(lx_error).
        MESSAGE lx_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
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

    DATA: lo_matcher   TYPE REF TO cl_abap_matcher,
          lv_msg_input TYPE char50.
    lo_matcher = cl_abap_matcher=>create( pattern     = '^[0-9 ]+$'
                                          ignore_case = abap_true
                                          text        = mv_input ).

    IF NOT lo_matcher->match( ).

      lv_msg_input = mv_input.
      RAISE EXCEPTION TYPE lcx_numeral_converter
        EXPORTING
          iv_msgv1 = |Cannot interpret|
          iv_msgv2 = lv_msg_input.

    ENDIF.
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
    cl_demo_output=>display( |{ p_input } is converted to { NEW lcl_numeral_converter( iv_input )->convert(  ) }| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_numeral_converter DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      acceptance_test FOR TESTING,
      test1_3_iii FOR TESTING.
ENDCLASS.


CLASS ltc_numeral_converter IMPLEMENTATION.


  METHOD acceptance_test.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = NEW lcl_numeral_converter( 'MMMCMXCIX' )->convert( )
        exp                  = '3999' ).
  ENDMETHOD.

  METHOD test1_3_iii.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = NEW lcl_numeral_converter( '3' )->convert( )
        exp                  = 'III' ).
  ENDMETHOD.


ENDCLASS.

START-OF-SELECTION.
  lcl_application=>main( p_input ).
