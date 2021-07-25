REPORT zr_w2_d2_wbs.

PARAMETERS:
    p_grid TYPE i.

CLASS lcl_cell DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS :
      constructor IMPORTING iv_coord_x TYPE i
                            iv_coord_y TYPE i
                            iv_status  TYPE abap_bool OPTIONAL,
      status RETURNING VALUE(rv_status) TYPE abap_bool,
      kill,
      give_birth,
      position
        EXPORTING
          ev_coord_x TYPE i
          ev_coord_y TYPE i.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA : mv_coord_x TYPE i,
           mv_coord_y TYPE i,
           mv_status  TYPE abap_bool.
ENDCLASS.

CLASS lcl_cell IMPLEMENTATION.

  METHOD constructor.
    mv_coord_x = iv_coord_x.
    mv_coord_y = iv_coord_y.
    mv_status  = iv_status.
  ENDMETHOD.

  METHOD give_birth.
    mv_status = abap_true.
  ENDMETHOD.

  METHOD kill.
    mv_status = abap_false.
  ENDMETHOD.

  METHOD position.
    ev_coord_x = mv_coord_x.
    ev_coord_y = mv_coord_y.
  ENDMETHOD.

  METHOD status.
    rv_status = mv_status.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_gol DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES :
      ty_cell TYPE REF TO lcl_cell,
      BEGIN OF ty_coord,
        coord_x TYPE i,
        coord_y TYPE i,
      END OF ty_coord,
      tyt_coord TYPE STANDARD TABLE OF ty_coord WITH DEFAULT KEY.

    DATA :
      mv_grid_size TYPE i,
      mt_gol_grid  TYPE STANDARD TABLE OF ty_cell WITH DEFAULT KEY.

    METHODS :
      constructor IMPORTING iv_grid_size TYPE i,
      init_grid,
      play.

    METHODS get_neighbors IMPORTING lo_cell             TYPE REF TO lcl_cell
                          RETURNING VALUE(rt_neighbors) TYPE tyt_coord.
  PROTECTED SECTION.


  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_gol IMPLEMENTATION.

  METHOD constructor.
    mv_grid_size = iv_grid_size.
    init_grid(  ).
  ENDMETHOD.

  METHOD init_grid.
    DATA :
      lv_x TYPE i,
      lv_y TYPE i.

    DO mv_grid_size TIMES.
      DO mv_grid_size TIMES.
        APPEND NEW lcl_cell( iv_coord_x = lv_x iv_coord_y = lv_y iv_status = abap_false ) TO mt_gol_grid.
        lv_y = lv_y + 1.
      ENDDO.
      FREE lv_y.
      lv_x = lv_x + 1.
    ENDDO.
    FREE lv_x.
  ENDMETHOD.

  METHOD play.
    LOOP AT mt_gol_grid INTO DATA(lo_gol_grid).
      DATA(lt_neighbors) = get_neighbors( lo_gol_grid ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_neighbors.

    lo_cell->position(
      IMPORTING
        ev_coord_x = DATA(lv_coord_x)
        ev_coord_y = DATA(lv_coord_y) ).

    IF ( lv_coord_x = 1 ) AND ( lv_coord_y = 1 ).
      APPEND VALUE #( coord_x = lv_coord_x - 1 coord_y = lv_coord_y - 1 ) TO rt_neighbors.
      APPEND VALUE #( coord_x = lv_coord_x     coord_y = lv_coord_y - 1 ) TO rt_neighbors.
      APPEND VALUE #( coord_x = lv_coord_x + 1 coord_y = lv_coord_y - 1 ) TO rt_neighbors.
    ENDIF.

    IF ( lv_coord_x > 1 AND lv_coord_x < mv_grid_size ) AND ( lv_coord_y > 1 AND lv_coord_y < mv_grid_size ).
      APPEND VALUE #( coord_x = lv_coord_x - 1 coord_y = lv_coord_y - 1 ) TO rt_neighbors.
      APPEND VALUE #( coord_x = lv_coord_x     coord_y = lv_coord_y - 1 ) TO rt_neighbors.
      APPEND VALUE #( coord_x = lv_coord_x + 1 coord_y = lv_coord_y - 1 ) TO rt_neighbors.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS ltc_gol DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FRIENDS lcl_gol.

  PRIVATE SECTION.
    DATA :
      mo_cut TYPE REF TO lcl_gol.
    METHODS:
      acceptance_test FOR TESTING.
ENDCLASS.


CLASS ltc_gol IMPLEMENTATION.

  METHOD acceptance_test.
    DATA(lo_cell) = NEW lcl_cell( iv_coord_x = 1
                                  iv_coord_y = 1
                                  iv_status  = abap_true ).

    DATA lt_neighbors TYPE lcl_gol=>tyt_coord.

    lt_neighbors = VALUE #( ( coord_x = 1 coord_y = 2 )
                            ( coord_x = 2 coord_y = 1 )
                            ( coord_x = 2 coord_y = 2 ) ).

    cl_abap_unit_assert=>assert_equals( exp = lt_neighbors
                                        act = mo_cut->get_neighbors( lo_cell ) ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_cell DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA :
      mo_cut TYPE REF TO lcl_cell.
    METHODS:
      acceptance_test FOR TESTING.
ENDCLASS.


CLASS ltc_cell IMPLEMENTATION.

  METHOD acceptance_test.
    mo_cut = NEW #( iv_coord_x = 0
                    iv_coord_y = 1
                    iv_status  = abap_true ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true act = mo_cut->status(  ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_application DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS :
      main IMPORTING iv_grid_size TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_application IMPLEMENTATION.

  METHOD main.
    NEW lcl_gol( iv_grid_size )->play(  ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_application=>main( p_grid ).
