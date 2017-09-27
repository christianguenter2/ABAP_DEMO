*&---------------------------------------------------------------------*
*& Report z_mouseless_eclipse
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_mouseless_eclipse.



CLASS mouseless_eclipse DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.

    CONSTANTS:
      normal_mode  TYPE string VALUE '',
      insert_mode  TYPE string VALUE '',
      visual_mode  TYPE string VALUE '',
      command_mode TYPE string VALUE ''.

    DATA: tadir_tab TYPE STANDARD TABLE OF tadir
                         WITH NON-UNIQUE EMPTY KEY.

    METHODS:
      quick_launch,

      access
        IMPORTING
          almost_all_eclipse_actions TYPE abap_bool
          and_elements               TYPE abap_bool
          and_menu_items             TYPE abap_bool
          views                      TYPE abap_bool
          perspectives               TYPE abap_bool
          tabs                       TYPE abap_bool,

      customizable_key_bindings,

      quick_access_key_bindings,

      templates,

      quick_fixes,

      refactorings,

      fix_errors,

      create_new_objects,

      start_refactorings,

      open_development_objects,

      outline,

      code_completion,

      annotations,

      vrapper,

      the_power_of_vim,

      inner,

      relative_line_numbers,

      basic_motions,

      advanced_motions,

      visual_block_mode,

      registers,

      how_to_improve_your_vim_skills,

      vim_adventures,

      vim_golf,

      vim_tutor,

      sort,

      substitute_brackets.

ENDCLASS.

CLASS mouseless_eclipse IMPLEMENTATION.

  METHOD start.

    code_completion( ).

    quick_launch( ).

    quick_access_key_bindings( ).

    customizable_key_bindings( ).

    templates( ).

    quick_fixes( ).

    refactorings( ).

    annotations( ).

    open_development_objects( ).

    outline( ).

    vrapper( ).

  ENDMETHOD.


  METHOD quick_launch.

    "     _______ _________ _______  _        _   ______
    "    (  ____ \\__   __/(  ____ )( \      ( ) / ___  \
    "    | (    \/   ) (   | (    )|| (      | | \/   \  \
    "    | |         | |   | (____)|| |    __| |__  ___) /
    "    | |         | |   |     __)| |   (__   __)(___ (
    "    | |         | |   | (\ (   | |      | |       ) \
    "    | (____/\   | |   | ) \ \__| (____/\| | /\___/  /
    "    (_______/   )_(   |/   \__/(_______/(_) \______/

    access(
      almost_all_eclipse_actions             = abap_true
      and_elements                           = abap_true
      and_menu_items                         = abap_true
      views                                  = abap_true
      perspectives                           = abap_true
      tabs                                   = abap_true ).

  ENDMETHOD.


  METHOD access.

  ENDMETHOD.


  METHOD customizable_key_bindings.

  ENDMETHOD.


  METHOD quick_access_key_bindings.

  ENDMETHOD.


  METHOD templates.

  ENDMETHOD.


  METHOD quick_fixes.

    "     _______ _________ _______  _        _   __
    "    (  ____ \\__   __/(  ____ )( \      ( ) /  \
    "    | (    \/   ) (   | (    )|| (      | | \/) )
    "    | |         | |   | (____)|| |    __| |__ | |
    "    | |         | |   |     __)| |   (__   __)| |
    "    | |         | |   | (\ (   | |      | |   | |
    "    | (____/\   | |   | ) \ \__| (____/\| | __) (_
    "    (_______/   )_(   |/   \__/(_______/(_) \____/

    fix_errors( ).
    create_new_objects( ).
    start_refactorings( ).

  ENDMETHOD.


  METHOD refactorings.

  ENDMETHOD.


  METHOD fix_errors.

  ENDMETHOD.


  METHOD create_new_objects.

  ENDMETHOD.


  METHOD start_refactorings.

  ENDMETHOD.


  METHOD open_development_objects.

  ENDMETHOD.


  METHOD outline.

  ENDMETHOD.


  METHOD code_completion.

  ENDMETHOD.


  METHOD annotations.

  ENDMETHOD.

  DEFINE there_are_four_different_modes.
    IF &1 = &2 OR &3 = &4. ENDIF.
  END-OF-DEFINITION.


  METHOD vrapper.

    the_power_of_vim( ).

    there_are_four_different_modes:

      normal_mode
      insert_mode
      visual_mode
      command_mode.

    basic_motions( ).

    advanced_motions( ).

    relative_line_numbers( ).

    inner( ).

    visual_block_mode( ).

    substitute_brackets( ).

    sort( ).

    registers( ).

    how_to_improve_your_vim_skills( ).

  ENDMETHOD.


  METHOD the_power_of_vim.

  ENDMETHOD.


  METHOD inner.

    cl_demo_output=>write( |Test| ).

    cl_demo_output=>write(
      EXPORTING
        data = |test|
        name = |name| ).

    cl_demo_output=>write( |today: { sy-datum } time: { sy-uzeit }| ).

  ENDMETHOD.


  METHOD relative_line_numbers.

  ENDMETHOD.


  METHOD basic_motions.

    "         ^
    "         |
    "         k
    "   <-- h   l -->
    "         j
    "         |
    "         ˅

    "      _ _ _ _     _  _  _  _   _     _     _     _       _    _    _    _
    "     | | | | |   (_)(_)(_)(_) | |   | |   | |   | |     | |  | |  | |  | |
    "     | | | | |    _  _  _  _  | |__ | |__ | |__ | |__   | | _| | _| | _| | __
    "     | | | | |   | || || || | | '_ \| '_ \| '_ \| '_ \  | |/ / |/ / |/ / |/ /
    "     | | | | |   | || || || | | | | | | | | | | | | | | |   <|   <|   <|   <
    "     |_|_|_|_|   | || || || | |_| |_|_| |_|_| |_|_| |_| |_|\_\_|\_\_|\_\_|\_\
    "                _/ |/ |/ |/ |
    "               |__/__/__/__/

    DATA(title) = |This is Vrapper!!! |
               && |Manipulate your sourcecode like never before. |
               && |VIM-like editing in eclipse. |
               && |Basic motion is hjkl. |
               && |They are all based on the home row, and are accessed with your right hand.|.
    "     ___ _     ___ _     ___ _         ___ _
    "    /   | |   /   (_)   /   | |       /   | |
    "   / /| | |  / /| |_   / /| | |__    / /| | | __
    "  / /_| | | / /_| | | / /_| | '_ \  / /_| | |/ /
    "  \___  | | \___  | | \___  | | | | \___  |   <
    "      |_/_|     |_/ |     |_/_| |_|     |_/_|\_\
    "                 _/ |
    "                |__/
  ENDMETHOD.


  METHOD advanced_motions.

  ENDMETHOD.


  METHOD visual_block_mode.

  ENDMETHOD.


  METHOD registers.

  ENDMETHOD.


  METHOD how_to_improve_your_vim_skills.

    vim_tutor( ).

    vim_adventures( ).

    vim_golf( ).

  ENDMETHOD.


  METHOD vim_adventures.

    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url    = 'http://vim-adventures.com'
      EXCEPTIONS
        OTHERS = 6.

  ENDMETHOD.


  METHOD vim_golf.

    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url    = 'http://www.vimgolf.com'
      EXCEPTIONS
        OTHERS = 6.

  ENDMETHOD.


  METHOD vim_tutor.

  ENDMETHOD.


  METHOD sort.

    DATA(shopping_list) = VALUE stringtab(
        ( `apple`   )
        ( `milk`    )
        ( `beer`    )
        ( `oranges` )
        ( `beer`    )
        ( `chips`   )
        ( `beer`    )
        ( `pizza`   )
        ( `beer`    )
        ( `beer`    )
        ( `pizza`   )
     ).

    SELECT
            pgmid
            object
            obj_name
            korrnum
            srcsystem
            author
            srcdep
            devclass
            genflag
            edtflag
            cproject
            masterlang
            versid
            paknocheck
            objstablty
            component
            crelease
            delflag
            translttxt
            created_on
            check_date
            check_cfg

            FROM tadir
            UP TO 10 ROWS
            INTO CORRESPONDING FIELDS OF TABLE tadir_tab.

  ENDMETHOD.


  METHOD substitute_brackets.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW mouseless_eclipse( )->start( ).
