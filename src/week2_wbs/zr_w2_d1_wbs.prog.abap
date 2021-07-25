REPORT zr_w2_d1_wbs.

PARAMETERS:
  p_grid TYPE i.

CLASS lcl_cell DEFINITION FINAL.

  PUBLIC SECTION.
    DATA :
      mv_x      TYPE i,
      mv_y      TYPE i,
      mv_status TYPE abap_bool.
    METHODS :
      constructor IMPORTING iv_x TYPE i
                            iv_y TYPE i,
      position    EXPORTING rv_x TYPE i
                            rv_y TYPE i,
      status RETURNING VALUE(rv_status) TYPE abap_bool,
      die,
      live.
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_cell IMPLEMENTATION.

  METHOD constructor.
    mv_x = iv_x.
    mv_y = iv_y.
  ENDMETHOD.

  METHOD die.
    mv_status = abap_false.
  ENDMETHOD.

  METHOD live.
    mv_status = abap_true.
  ENDMETHOD.

  METHOD position.
    rv_x = mv_x.
    rv_y = mv_y.
  ENDMETHOD.

  METHOD status.
    rv_status = mv_status.
  ENDMETHOD.

ENDCLASS.



CLASS lcl_gameoflife DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_cell  TYPE REF TO lcl_cell,
           tyt_cell TYPE TABLE OF ty_cell,

           BEGIN OF ty_coord,
             coord_x TYPE i,
             coord_y TYPE i,
           END OF ty_coord,

           tyt_coord TYPE STANDARD TABLE OF ty_coord WITH DEFAULT KEY.

    DATA :
      mv_grid_size  TYPE i,
      mt_gol_matrix TYPE tyt_cell.

    METHODS :
      constructor IMPORTING iv_matrix_size TYPE i,
      init_grid IMPORTING iv_grid_size TYPE i,
      play,
      get_neighbors IMPORTING io_cell             TYPE REF TO lcl_cell
                    RETURNING VALUE(rt_neighbors) TYPE tyt_coord.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_gameoflife IMPLEMENTATION.

  METHOD constructor.
    init_grid( iv_matrix_size ).
    mv_grid_size = iv_matrix_size.
  ENDMETHOD.

  METHOD get_neighbors.
    rt_neighbors = VALUE #( ( coord_x = io_cell->mv_x - 1  coord_y = io_cell->mv_y - 1 )
                            ( coord_x = io_cell->mv_x   coord_y = io_cell->mv_y - 1 )
                            ( coord_x = io_cell->mv_x + 1  coord_y = io_cell->mv_y - 1 )

                            ( coord_x = io_cell->mv_x - 1  coord_y = io_cell->mv_y )
                            ( coord_x = io_cell->mv_x + 1  coord_y = io_cell->mv_y )

                            ( coord_x = io_cell->mv_x - 1  coord_y = io_cell->mv_y + 1 )
                            ( coord_x = io_cell->mv_x   coord_y = io_cell->mv_y + 1 )
                            ( coord_x = io_cell->mv_x + 1  coord_y = io_cell->mv_y + 1 ) ).

  ENDMETHOD.

  METHOD init_grid.
    DATA: lv_y TYPE i VALUE 1,
          lv_x TYPE i VALUE 1.

    DO iv_grid_size TIMES.
      DO iv_grid_size TIMES.
        APPEND NEW lcl_cell( iv_x = lv_x  iv_y = lv_y ) TO mt_gol_matrix.
        lv_y = lv_y + 1.
      ENDDO.
      lv_x = lv_x + 1.
      FREE lv_y.
    ENDDO.
    FREE lv_x.
  ENDMETHOD.

  METHOD play.
    LOOP AT mt_gol_matrix ASSIGNING FIELD-SYMBOL(<lfo_gol_matrix>).
      get_neighbors( <lfo_gol_matrix> ).
    ENDLOOP.
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
    NEW lcl_gameoflife( iv_grid_size )->play(  ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_cell DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA :
      mo_cut TYPE REF TO lcl_cell.
    METHODS:
      setup,
      set_alive FOR TESTING,
      set_dead FOR TESTING.
ENDCLASS.


CLASS ltc_cell IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( iv_x = 1 iv_y = 1 ).
  ENDMETHOD.

  METHOD set_alive.

    mo_cut->live(  ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = mo_cut->status( ) ).
  ENDMETHOD.

  METHOD set_dead.

    mo_cut->die(  ).

    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = mo_cut->status( ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_application=>main( p_grid ).
