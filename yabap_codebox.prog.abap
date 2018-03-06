*&--------------------------------------------------------------------------------*
*& MIT License
*&
*& Copyright (c) 2018 Raphael Pacheco (#Pacheco)
*&
*& Permission is hereby granted, free of charge, to any person obtaining a copy
*& of this software and associated documentation files (the "Software"), to deal
*& in the Software without restriction, including without limitation the rights
*& to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*& copies of the Software, and to permit persons to whom the Software is
*& furnished to do so, subject to the following conditions:
*&
*& The above copyright notice and this permission notice shall be included in all
*& copies or substantial portions of the Software.
*&
*& THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*& IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*& FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*& AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*& LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*& OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*& SOFTWARE.
*&
*& Git Project URL: https://www.github.com/pacheco7/abapcodebox
*&--------------------------------------------------------------------------------*
REPORT abap_codebox MESSAGE-ID yabapcodebox_msg.

DATA: ok_code    TYPE sy-ucomm,
      splitter   TYPE REF TO cl_gui_splitter_container,
      tree_box   TYPE REF TO cl_gui_alv_tree,
      editor_box TYPE REF TO cl_gui_abapedit,
      statements TYPE stringtab.

CLASS abap_code_box DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor.

    CLASS-METHODS:
      run_source,

      set_default_source IMPORTING codebox TYPE REF TO cl_gui_abapedit
                                   source  TYPE stringtab,

      get_changed_source IMPORTING codebox TYPE REF TO cl_gui_abapedit
                         EXPORTING source  TYPE stringtab.

  PRIVATE SECTION.
    TYPES: BEGIN OF y_tree,
             node TYPE string,
           END OF y_tree,

           ty_tree TYPE STANDARD TABLE OF y_tree WITH EMPTY KEY.

    DATA: t_tree TYPE ty_tree.

    CONSTANTS: c_progname TYPE progname VALUE 'YEMPTYBOX'.

    METHODS:
      check_if_emptybox_exists RETURNING VALUE(exists) TYPE abap_bool,
      security_checks RETURNING VALUE(allowed) TYPE abap_bool,
      initialize_codebox,
      load_tree_box IMPORTING parent         TYPE REF TO cl_gui_container,
      load_editor_box IMPORTING parent         TYPE REF TO cl_gui_container
                      RETURNING VALUE(codebox) TYPE REF TO cl_gui_abapedit.

    CLASS-METHODS:
      set_global_data CHANGING VALUE(source) TYPE stringtab,
      set_parameters CHANGING VALUE(source) TYPE stringtab,
      set_definition CHANGING VALUE(source) TYPE stringtab,
      set_implementation CHANGING VALUE(source) TYPE stringtab,
      set_statements CHANGING VALUE(source) TYPE stringtab,
      verify_allowed_statments RETURNING VALUE(subrc) TYPE sy-subrc,
      set_annotations_to_node,
      set_all_syntax_to_node,
      set_unit_tests_to_node,
      add_node IMPORTING VALUE(i_relat_node_key) TYPE lvc_nkey
                         VALUE(i_node_text)      TYPE lvc_value
               RETURNING VALUE(e_new_node_key)   TYPE lvc_nkey.
ENDCLASS.

INCLUDE: yabap_codebox_pbo, yabap_codebox_pai.

CLASS abap_code_box IMPLEMENTATION.
  METHOD constructor.
    IF check_if_emptybox_exists( ) = abap_true.
      IF security_checks( ) = abap_true.
        initialize_codebox( ).
        CALL SCREEN 100.
      ENDIF.
    ELSE.
      MESSAGE s001 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD run_source.
    DATA: source   TYPE stringtab,
          progname TYPE progname.

    IF verify_allowed_statments( ) IS NOT INITIAL.
      RETURN.
    ENDIF.

    get_changed_source( EXPORTING codebox = editor_box
                        IMPORTING source  = statements
    ).

    TRY.
        READ REPORT c_progname INTO source.

        set_global_data( CHANGING source = source ).
        set_parameters( CHANGING source = source ).
        set_definition( CHANGING source = source ).
        set_implementation( CHANGING source = source ).
        set_statements( CHANGING source = source ).

        DATA(source_name) = 'SOURCE'.
        FIELD-SYMBOLS <source> TYPE STANDARD TABLE.
        ASSIGN (source_name) TO <source>.

        TRY.
            GENERATE SUBROUTINE POOL <source> NAME progname.
          CATCH cx_sy_generate_subpool_full.
            MESSAGE TEXT-srf TYPE 'I' DISPLAY LIKE 'E'.
            RETURN.
        ENDTRY.

        DATA(class) = `\PROGRAM=` && progname && `\CLASS=ABAP_EMPTYBOX`.
        CALL METHOD (class)=>run.

      CATCH cx_sy_read_src_line_too_long.
      CATCH cx_root INTO DATA(err_root) ##CATCH_ALL.
        MESSAGE err_root->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD set_default_source.
    codebox->set_text( source ).
  ENDMETHOD.

  METHOD get_changed_source.
    codebox->get_text( EXPORTING name  = sy-repid
                       IMPORTING table = source
                       EXCEPTIONS error_dp               = 1
                                  error_cntl_call_method = 2
    ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD check_if_emptybox_exists.
    SELECT COUNT( * ) FROM reposrc INTO @DATA(count) WHERE progname = @c_progname.

    exists = COND #( WHEN count > 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD security_checks.
*   First check: Check if the system is not production type
    IF cl_abap_demo_services=>is_production_system( ).
      MESSAGE s002 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

*   Second check: Only users who are allowed to use the Standard ABAP Editor
    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'SE38'
      EXCEPTIONS
        ok     = 1
        not_ok = 2
        OTHERS = 3.

*   Third check: Only users who are allowed to create and run local programs
    IF sy-subrc < 2.
      AUTHORITY-CHECK OBJECT 'S_DEVELOP'
        ID 'DEVCLASS' FIELD '$TMP'
        ID 'OBJTYPE'  FIELD 'PROG'
        ID 'OBJNAME'  DUMMY
        ID 'P_GROUP'  DUMMY
        ID 'ACTVT'    FIELD '02'.

      IF sy-subrc IS INITIAL.
        allowed = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    MESSAGE s003 DISPLAY LIKE 'E'.
  ENDMETHOD.

  METHOD initialize_codebox.
    DATA(main_container) = NEW cl_gui_custom_container( container_name = 'CC_MAIN_CONTAINER' ).

    CREATE OBJECT splitter
      EXPORTING
        align             = 15
        parent            = main_container
        rows              = 2
        columns           = 2
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    splitter->set_column_width( id = 1 width = 15 ).
    splitter->set_column_width( id = 2 width = 100 ).
    splitter->set_row_height( id = 2 height = 0 ).

    DATA(tree_container) = splitter->get_container( row = 1 column = 1 ).
    DATA(editor_container) = splitter->get_container( row = 1 column = 2 ).
    DATA(check_container) = splitter->get_container( row = 2 column = 2 ).

    statements = VALUE #( ( `" Your code here!` ) ).

    load_tree_box( tree_container ).
    editor_box = load_editor_box( editor_container ).
  ENDMETHOD.

  METHOD load_tree_box.
    DATA(fieldcatalog) = VALUE lvc_t_fcat( ( fieldname = 'NODE'
                                             tech      = abap_true ) ).

    DATA(header) = VALUE treev_hhdr( heading   = 'Objects'
                                     tooltip   = 'Objects'
                                     width     = 30
                                     width_pix = '' ).

    DATA(variant) = VALUE disvariant( report = sy-repid ).

    CREATE OBJECT tree_box
      EXPORTING
        parent              = parent
        no_html_header      = abap_true
        no_toolbar          = abap_true
        node_selection_mode = cl_gui_column_tree=>node_sel_mode_multiple.

    tree_box->set_table_for_first_display(
      EXPORTING
        is_hierarchy_header = header
        i_save              = 'A'
        is_variant          = variant
      CHANGING
        it_outtab           = t_tree
        it_fieldcatalog     = fieldcatalog ).

    set_annotations_to_node( ).
    set_all_syntax_to_node( ).
    set_unit_tests_to_node( ).
    tree_box->frontend_update( ).
  ENDMETHOD.

  METHOD load_editor_box.
    CREATE OBJECT codebox EXPORTING parent = parent.

    codebox->set_toolbar_mode( 0 ).
  ENDMETHOD.

  METHOD set_global_data.
    DATA end_line TYPE i.

    FIND '"@Global' IN TABLE statements MATCH LINE DATA(begin_line).

    IF sy-subrc IS INITIAL.
      FIND '"@Global' IN TABLE source MATCH LINE DATA(index).
      DELETE source INDEX index.

      FIND '"@Parameters' IN TABLE statements MATCH LINE end_line.

      IF end_line IS INITIAL.
        FIND '"@Definition' IN TABLE statements MATCH LINE end_line.

        IF end_line IS INITIAL.
          FIND '"@Statements' IN TABLE statements MATCH LINE end_line.

          IF end_line IS INITIAL.
            end_line = lines( statements ).
          ELSE.
            end_line = end_line - 1.
          ENDIF.
        ENDIF.
      ENDIF.

      DATA(string) = REDUCE #( INIT out TYPE stringtab
                                FOR i = begin_line UNTIL i > end_line
                               NEXT out = VALUE #( BASE out ( VALUE string( statements[ i ] OPTIONAL ) ) ) ).

      INSERT LINES OF string INTO source INDEX index.
    ENDIF.
  ENDMETHOD.

  METHOD set_parameters.
    DATA end_line TYPE i.

    FIND '"@Parameters' IN TABLE statements MATCH LINE DATA(begin_line).

    IF sy-subrc IS INITIAL.
      FIND '"@Parameters' IN TABLE source MATCH LINE DATA(index).
      DELETE source INDEX index.

      FIND '"@Definition' IN TABLE statements MATCH LINE end_line.

      IF end_line IS INITIAL.
        FIND '"@Statements' IN TABLE statements MATCH LINE end_line.

        IF end_line IS INITIAL.
          end_line = lines( statements ).
        ELSE.
          end_line = end_line - 1.
        ENDIF.
      ENDIF.

      DATA(string) = REDUCE #( INIT out TYPE stringtab
                                FOR i = begin_line UNTIL i > end_line
                               NEXT out = VALUE #( BASE out ( VALUE string( statements[ i ] OPTIONAL ) ) ) ).

      INSERT LINES OF string INTO source INDEX index.
    ENDIF.
  ENDMETHOD.

  METHOD set_definition.
    DATA end_line TYPE i.

    FIND '"@Definition' IN TABLE statements MATCH LINE DATA(begin_line).

    IF sy-subrc IS INITIAL.
      FIND '"@Definition' IN TABLE source MATCH LINE DATA(index).
      DELETE source INDEX index.

      FIND '"@Implementation' IN TABLE statements MATCH LINE end_line.

      IF end_line IS INITIAL.
        FIND '"@Statements' IN TABLE statements MATCH LINE end_line.

        IF end_line IS INITIAL.
          end_line = lines( statements ).
        ELSE.
          end_line = end_line - 1.
        ENDIF.
      ENDIF.

      DATA(string) = REDUCE #( INIT out TYPE stringtab
                                FOR i = begin_line UNTIL i > end_line
                               NEXT out = VALUE #( BASE out ( VALUE string( statements[ i ] OPTIONAL ) ) ) ).

      INSERT LINES OF string INTO source INDEX index.
    ENDIF.
  ENDMETHOD.

  METHOD set_implementation.
    DATA end_line TYPE i.

    FIND '"@Implementation' IN TABLE statements MATCH LINE DATA(begin_line).

    IF sy-subrc IS INITIAL.
      FIND '"@Implementation' IN TABLE source MATCH LINE DATA(index).
      DELETE source INDEX index.

      FIND '"@Statements' IN TABLE statements MATCH LINE end_line.

      IF end_line IS INITIAL.
        end_line = lines( statements ).
      ELSE.
        end_line = end_line - 1.
      ENDIF.

      DATA(string) = REDUCE #( INIT out TYPE stringtab
                                FOR i = begin_line UNTIL i > end_line
                               NEXT out = VALUE #( BASE out ( VALUE string( statements[ i ] OPTIONAL ) ) ) ).

      INSERT LINES OF string INTO source INDEX index.
    ENDIF.
  ENDMETHOD.

  METHOD set_statements.
    DATA end_line TYPE i.

    FIND '"@Statements' IN TABLE statements MATCH LINE DATA(begin_line).

    IF sy-subrc IS INITIAL.
      FIND '"@Statements' IN TABLE source MATCH LINE DATA(index).
      DELETE source INDEX index.

      end_line = lines( statements ).

      DATA(string) = REDUCE #( INIT out TYPE stringtab
                                FOR i = begin_line UNTIL i > end_line
                               NEXT out = VALUE #( BASE out ( VALUE string( statements[ i ] OPTIONAL ) ) ) ).

      INSERT LINES OF string INTO source INDEX index.
    ENDIF.
  ENDMETHOD.

  METHOD verify_allowed_statments.
    DATA: message  TYPE string,
          line     TYPE i,
          word     TYPE string,
          warnings TYPE STANDARD TABLE OF rslinlmsg WITH EMPTY KEY.

*   First check: Normal check for types
    DATA(code) = VALUE stringtab( ( |PROGRAM.| ) ).
    APPEND LINES OF statements TO code.

    SYNTAX-CHECK FOR code MESSAGE message LINE line WORD word
                 ID 'MGS' TABLE warnings
                 PROGRAM sy-repid.

    subrc = sy-subrc.
    IF subrc IS NOT INITIAL.
      cl_demo_output=>display( message ).
      RETURN.
    ENDIF.

    IF warnings IS NOT INITIAL.
      cl_demo_output=>display( warnings ).
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD set_annotations_to_node.
    DATA: i_relat_node_key TYPE lvc_nkey,
          new_key          TYPE lvc_nkey,
          is_node_layout   TYPE lvc_s_layn.

    DATA(parent_key) = add_node( i_relat_node_key = i_relat_node_key i_node_text = 'Annotations' ).
    new_key = add_node( i_relat_node_key = parent_key i_node_text = '@Global' ).
    new_key = add_node( i_relat_node_key = parent_key i_node_text = '@Parameters' ).
    new_key = add_node( i_relat_node_key = parent_key i_node_text = '@Definition' ).
    new_key = add_node( i_relat_node_key = parent_key i_node_text = '@Implementation' ).
    new_key = add_node( i_relat_node_key = parent_key i_node_text = '@Statements' ).
  ENDMETHOD.

  METHOD set_all_syntax_to_node.
    DATA: i_relat_node_key TYPE lvc_nkey,
          new_key          TYPE lvc_nkey,
          is_node_layout   TYPE lvc_s_layn,
          keywords         TYPE sana_keyword_tab.

    DATA(parent_key) = add_node( i_relat_node_key = i_relat_node_key i_node_text = 'Syntaxes' ).

    CALL FUNCTION 'RS_GET_ABAP_PATTERN'
      IMPORTING
        keywords = keywords.

    DATA(keys) = VALUE stringtab( FOR keyword IN keywords WHERE ( word(1) <> '+' AND
                                                                  word(1) <> '$' AND
                                                                  word(4) <> 'SYS$' )
                                  ( add_node( i_relat_node_key = parent_key i_node_text = CONV lvc_value( keyword-word ) ) ) ).
  ENDMETHOD.

  METHOD set_unit_tests_to_node.
    DATA: i_relat_node_key TYPE lvc_nkey,
          new_key          TYPE lvc_nkey,
          is_node_layout   TYPE lvc_s_layn.

    DATA(parent_key) = add_node( i_relat_node_key = i_relat_node_key i_node_text = 'Unit Tests Examples' ).
    new_key = add_node( i_relat_node_key = parent_key i_node_text = 'TDD (Based on ABAP Koans)' ).
    new_key = add_node( i_relat_node_key = parent_key i_node_text = 'BDD (ABAP Behave)' ).
  ENDMETHOD.

  METHOD add_node.
    DATA is_node_layout TYPE lvc_s_layn.

    DATA(it_item_layout) = VALUE lvc_t_layi( ( fieldname = tree_box->c_hierarchy_column_name
                                               style     = cl_gui_column_tree=>style_intensifd_critical ) ).

    tree_box->add_node(
      EXPORTING
        i_relat_node_key = i_relat_node_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = i_node_text
        is_node_layout   = is_node_layout
        it_item_layout   = it_item_layout
      IMPORTING
        e_new_node_key   = e_new_node_key
      EXCEPTIONS
        relat_node_not_found = 1
        node_not_found       = 2
    ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW abap_code_box( ).
