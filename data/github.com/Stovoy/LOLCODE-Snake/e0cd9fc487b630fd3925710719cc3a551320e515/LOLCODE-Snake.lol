HAI 1.2

I HAS A ll_head_sigil ITZ "__linked_list_head__"
HOW IZ I create_ll
    I HAS A ll ITZ A BUKKIT
    ll HAS A SRS ll_head_sigil ITZ ""
    FOUND YR ll
IF U SAY SO

HOW IZ I get_ll_head YR ll
    FOUND YR ll'Z SRS ll_head_sigil
IF U SAY SO

HOW IZ I get_ll_tail YR ll
    I HAS A head ITZ I IZ get_ll_head YR ll MKAY
    BOTH SAEM head AN "", O RLY?
        YA RLY
            FOUND YR ""
        NO WAI
            I HAS A pointer ITZ head
            IM IN YR traversal_loop
                I HAS A next_pointer ITZ ...
                ll'Z SRS pointer

                BOTH SAEM next_pointer AN "", O RLY?
                    YA RLY
                        FOUND YR pointer
                    NO WAI
                        pointer R next_pointer
                OIC
            IM OUTTA YR traversal_loop
    OIC
IF U SAY SO

HOW IZ I get_ll_next YR ll AN YR pointer
    FOUND YR ll'Z SRS pointer
IF U SAY SO

HOW IZ I append_ll YR ll AN YR item
    I HAS A head ITZ I IZ get_ll_head YR ll MKAY
    BOTH SAEM head AN "", O RLY?
        YA RLY
            ll'Z SRS ll_head_sigil R item
        NO WAI
            I HAS A tail ITZ ...
            I IZ get_ll_tail YR ll MKAY

            ll'Z SRS tail R item
    OIC

    ll'Z SRS item R ""
IF U SAY SO

HOW IZ I pop_ll YR ll
    I HAS A previous_pointer ITZ ll_head_sigil
    I HAS A pointer ITZ I IZ get_ll_head YR ll MKAY
    IM IN YR ll_loop
        I HAS A next_pointer ITZ I IZ get_ll_next YR ll AN YR pointer MKAY
        BOTH SAEM next_pointer AN "", O RLY?
            YA RLY
                ll'Z SRS previous_pointer R ""
                FOUND YR pointer
                GTFO
        OIC
        previous_pointer R pointer
        pointer R next_pointer
    IM OUTTA YR ll_loop
IF U SAY SO

HOW IZ I prepend_ll YR ll AN YR item
    I HAS A old_head ITZ I IZ get_ll_head YR ll MKAY
    ll'Z SRS ll_head_sigil R item
    ll'Z SRS item R old_head
IF U SAY SO

HOW IZ I splice_value_from_ll YR ll AN YR value
    I HAS A previous_pointer ITZ ll_head_sigil
    I HAS A pointer ITZ I IZ get_ll_head YR ll MKAY
    IM IN YR ll_loop
        BOTH SAEM pointer AN "", O RLY?
            YA RLY
                GTFO
        OIC
        I HAS A next_pointer ITZ I IZ get_ll_next YR ll AN YR pointer MKAY
        BOTH SAEM pointer AN value, O RLY?
            YA RLY
                ll'Z SRS previous_pointer R next_pointer
                GTFO
        OIC
        previous_pointer R pointer
        pointer R next_pointer
    IM OUTTA YR ll_loop
IF U SAY SO

HOW IZ I get_ll_length YR ll
    I HAS A length ITZ 0
    I HAS A pointer ITZ I IZ get_ll_head YR ll MKAY
    IM IN YR ll_loop
        BOTH SAEM pointer AN "", O RLY?
            YA RLY
                FOUND YR length
        OIC
        I HAS A next_pointer ITZ I IZ get_ll_next YR ll AN YR pointer MKAY
        pointer R next_pointer
        length R SUM OF length AN 1
    IM OUTTA YR ll_loop
IF U SAY SO

VISIBLE "WELCOM 2 LOLSNAKE!!"

I HAS A board ITZ A BUKKIT
I HAS A board_width ITZ 5
I HAS A board_height ITZ 4

HOW IZ I initialize_bukkit YR bukkit
    IM IN YR y_loop ...
    UPPIN YR y TIL BOTH SAEM y AN board_height
        IM IN YR x_loop ...
        UPPIN YR x TIL BOTH SAEM x AN board_width
            I HAS A key ITZ ":{x},:{y}"
            bukkit HAS A SRS key ITZ " "
        IM OUTTA YR x_loop
    IM OUTTA YR y_loop
IF U SAY SO

I HAS A snake ITZ I IZ create_ll MKAY
I IZ initialize_bukkit YR snake MKAY
I HAS A snake_head_x ITZ 0
I HAS A snake_head_y ITZ 0
I IZ append_ll YR snake AN YR ":{snake_head_x},:{snake_head_y}" MKAY
I HAS A snake_direction ITZ "east"

HOW IZ I try_move_snake
    I HAS A new_x ITZ snake_head_x
    I HAS A new_y ITZ snake_head_y

    snake_direction, WTF?
        OMG "north"
            new_y R SUM OF snake_head_y AN -1
            GTFO
        OMG "west"
            new_x R SUM OF snake_head_x AN -1
            GTFO
        OMG "south"
            new_y R SUM OF snake_head_y AN 1
            GTFO
        OMG "east"
            new_x R SUM OF snake_head_x AN 1
            GTFO
    OIC

    I HAS A will_die ITZ ...
    I IZ is_deadly YR new_x AN YR new_y MKAY

    BOTH SAEM will_die AN WIN, O RLY?
        YA RLY
            FOUND YR FAIL
    OIC

    I HAS A ate_apple ITZ ...
    I IZ is_apple YR new_x AN YR new_y MKAY

    BOTH SAEM ate_apple AN WIN, O RLY?
        YA RLY
            I HAS AN item ITZ SMOOSH new_x AN "," AN new_y MKAY
            I IZ splice_value_from_ll YR apples AN YR item MKAY
        NO WAI
            I IZ pop_ll YR snake MKAY
    OIC

    snake_head_x R new_x
    snake_head_y R new_y

    I IZ prepend_ll YR snake AN YR ":{snake_head_x},:{snake_head_y}" MKAY

    FOUND YR WIN
IF U SAY SO

I HAS A apple_tick ITZ 0
I HAS A apple_interval ITZ 5
I HAS A apples ITZ I IZ create_ll MKAY

HOW IZ I generate_apples
    BOTH SAEM apple_tick AN 0, O RLY?
        YA RLY
            apple_tick R apple_interval

            I HAS A apple_x ITZ 5
            I HAS A apple_y ITZ 23

            IM IN YR y_loop ...
            UPPIN YR y TIL BOTH SAEM y AN board_height
                IM IN YR x_loop ...
                UPPIN YR x TIL BOTH SAEM x AN board_width
                    I HAS A location_value ITZ ...
                    I IZ get_location_value YR x AN YR y MKAY

                    BOTH SAEM location_value AN "S", O RLY?
                        YA RLY
                            apple_x R SUM OF apple_x AN SUM OF x AN 3
                            apple_y R SUM OF apple_y AN SUM OF y AN 4
                    OIC

                    BOTH SAEM location_value AN "¤", O RLY?
                        YA RLY
                            apple_x R SUM OF apple_x AN SUM OF x AN 5
                            apple_y R SUM OF apple_y AN SUM OF y AN 6
                    OIC
                IM OUTTA YR x_loop
            IM OUTTA YR y_loop

            apple_x R MOD OF apple_x AN board_width
            apple_y R MOD OF apple_y AN board_height

            I HAS A iter_count ITZ 0
            IM IN YR apple_gen_loop
                I HAS A location_value ITZ ...
                I IZ get_location_value YR apple_x AN YR apple_y MKAY

                ANY OF ...
                BOTH SAEM location_value AN "¤" AN ...
                BOTH SAEM location_value AN "s" AN ...
                BOTH SAEM location_value AN "S" MKAY, O RLY?
                    YA RLY
                        apple_x R SUM OF apple_x AN 1
                        BOTH SAEM apple_x AN board_width, O RLY?
                            YA RLY
                                apple_x R 0
                                apple_y R SUM OF apple_y AN 1
                                BOTH SAEM apple_y AN board_height, O RLY?
                                    YA RLY
                                        apple_y R 0
                                OIC
                        OIC
                    NO WAI
                        GTFO
                OIC

                iter_count R SUM OF iter_count AN 1
                BOTH SAEM iter_count AN 25, O RLY?
                    YA RLY
                        GTFO
                OIC
            IM OUTTA YR apple_gen_loop

            BOTH SAEM iter_count AN 25, O RLY?
                YA RLY
                    GTFO
            OIC

            apple_x R MOD OF apple_x AN board_width
            apple_y R MOD OF apple_y AN board_height

            I IZ append_ll YR apples AN YR ":{apple_x},:{apple_y}" MKAY
    OIC

    apple_tick R DIFF OF apple_tick AN 1
IF U SAY SO

HOW IZ I get_location_value YR x AN YR y
    ALL OF ...
    BOTH SAEM snake_head_x AN x AN ...
    BOTH SAEM snake_head_y AN y MKAY, O RLY?
        YA RLY
            FOUND YR "S"
    OIC

    I HAS A apple_pointer ITZ I IZ get_ll_head YR apples MKAY
    IM IN YR apple_loop
        BOTH SAEM apple_pointer AN "", O RLY?
            YA RLY
                GTFO
        OIC

        I HAS A current_location_yarn ITZ SMOOSH x AN "," AN y MKAY
        BOTH SAEM apple_pointer AN current_location_yarn, O RLY?
            YA RLY
                FOUND YR "¤"
        OIC

        apple_pointer R I IZ get_ll_next YR apples AN YR apple_pointer MKAY
    IM OUTTA YR apple_loop

    I HAS A snake_pointer ITZ I IZ get_ll_head YR snake MKAY
    IM IN YR snake_loop
        BOTH SAEM snake_pointer AN "", O RLY?
            YA RLY
                GTFO
        OIC

        I HAS A current_location_yarn ITZ SMOOSH x AN "," AN y MKAY
        BOTH SAEM snake_pointer AN current_location_yarn, O RLY?
            YA RLY
                FOUND YR "s"
        OIC

        snake_pointer R I IZ get_ll_next YR snake AN YR snake_pointer MKAY
    IM OUTTA YR snake_loop

    FOUND YR " "
IF U SAY SO

HOW IZ I update_board
    IM IN YR y_loop ...
    UPPIN YR y TIL BOTH SAEM y AN board_height
        IM IN YR x_loop ...
        UPPIN YR x TIL BOTH SAEM x AN board_width
            I HAS A key ITZ ":{x},:{y}"

            board'Z SRS key R ...
            I IZ get_location_value ...
            YR x AN YR y MKAY
        IM OUTTA YR x_loop
    IM OUTTA YR y_loop
IF U SAY SO

HOW IZ I print_board
    I HAS A top_line ITZ "/"

    IM IN YR x_loop ...
    UPPIN YR x TIL BOTH SAEM x AN board_width
        top_line R SMOOSH top_line AN "--" MKAY
    IM OUTTA YR x_loop

    top_line R SMOOSH top_line AN "\" MKAY

    VISIBLE top_line

    IM IN YR y_loop ...
    UPPIN YR y TIL BOTH SAEM y AN board_height
        I HAS A line ITZ "|"
        IM IN YR x_loop ...
        UPPIN YR x TIL BOTH SAEM x AN board_width
            I HAS A key ITZ ":{x},:{y}"
            line R SMOOSH line AN board'Z SRS key AN " " MKAY
        IM OUTTA YR x_loop
        line R SMOOSH line AN "|" MKAY
        VISIBLE line
    IM OUTTA YR y_loop

    I HAS A bottom_line ITZ "\"

    IM IN YR x_loop ...
    UPPIN YR x TIL BOTH SAEM x AN board_width
        bottom_line R SMOOSH bottom_line AN "--" MKAY
    IM OUTTA YR x_loop

    bottom_line R SMOOSH bottom_line AN "/" MKAY

    VISIBLE bottom_line
IF U SAY SO

HOW IZ I user_input
    I HAS A command ITZ A YARN
    IM IN YR input_loop
        GIMMEH command
        I HAS A valid_input ITZ WIN
        command, WTF?
            OMG "w"
                snake_direction R "north"
                GTFO
            OMG "a"
                snake_direction R "west"
                GTFO
            OMG "s"
                snake_direction R "south"
                GTFO
            OMG "d"
                snake_direction R "east"
                GTFO
            OMG "q"
                FOUND YR WIN
                GTFO
            OMGWTF
                valid_input R FAIL
        OIC

        BOTH SAEM valid_input AN WIN, O RLY?
            YA RLY
                GTFO
            NO WAI
                VISIBLE "wtf! thatz not wasd!!? (q to kthxbye)"
        OIC
    IM OUTTA YR input_loop
IF U SAY SO

HOW IZ I is_deadly YR x AN YR y
    ALL OF ...
        BOTH SAEM ...
            SMALLR OF x AN 0 ...
        AN x AN ...
        DIFFRINT x AN 0 ...
    MKAY, O RLY?
        YA RLY
            FOUND YR WIN
    OIC

    ALL OF ...
        BOTH SAEM ...
            BIGGR OF x AN DIFF OF board_width AN 1 ...
        AN x AN ...
        DIFFRINT x AN DIFF OF board_width AN 1 ...
    MKAY, O RLY?
        YA RLY
            FOUND YR WIN
    OIC

    ALL OF ...
        BOTH SAEM ...
            SMALLR OF y AN 0 ...
        AN y AN ...
        DIFFRINT y AN 0 ...
    MKAY, O RLY?
        YA RLY
            FOUND YR WIN
    OIC

    ALL OF ...
        BOTH SAEM ...
            BIGGR OF y AN DIFF OF board_height AN 1 ...
        AN y AN ...
        DIFFRINT y AN DIFF OF board_height AN 1 ...
    MKAY, O RLY?
        YA RLY
            FOUND YR WIN
    OIC

    I HAS A location_value ...
    ITZ I IZ get_location_value ...
    YR x AN ...
    YR y MKAY

    BOTH SAEM location_value AN "s", O RLY?
        YA RLY
            FOUND YR WIN
        NO WAI
            FOUND YR FAIL
    OIC
IF U SAY SO

HOW IZ I is_apple YR x AN YR y
    I HAS A location_value ...
    ITZ I IZ get_location_value ...
    YR x AN ...
    YR y MKAY

    BOTH SAEM location_value AN "¤", O RLY?
        YA RLY
            FOUND YR WIN
        NO WAI
            FOUND YR FAIL
    OIC
IF U SAY SO

I IZ initialize_bukkit YR apples MKAY
I IZ initialize_bukkit YR board MKAY

IM IN YR game_loop
    I IZ generate_apples MKAY
    I IZ update_board MKAY
    I IZ print_board MKAY
    I HAS A leaving ITZ I IZ user_input MKAY
    BOTH SAEM leaving AN WIN, O RLY?
        YA RLY
            VISIBLE "ok bye ::("
            GTFO
    OIC

    I HAS A successfully_moved ITZ I IZ try_move_snake MKAY
    BOTH SAEM successfully_moved AN FAIL, O RLY?
        YA RLY
            I HAS A snake_length ITZ I IZ get_ll_length YR snake MKAY
            I HAS A points ITZ DIFF OF snake_length AN 1
            BOTH SAEM snake_length AN PRODUKT OF board_width AN board_height, O RLY?
                YA RLY
                    VISIBLE ""
                    VISIBLE ""
                    VISIBLE ""
                    VISIBLE "           /^\/^\                                     "
                    VISIBLE "         _|__|  O|                                    "
                    VISIBLE "\/     /~     \_/ \                                   "
                    VISIBLE " \____|__________/  \                                 "
                    VISIBLE "        \_______      \                               "
                    VISIBLE "                `\     \                 \            "
                    VISIBLE "                  |     |                  \          "
                    VISIBLE "                 /      /                    \        "
                    VISIBLE "                /     /                       \\      "
                    VISIBLE "              /      /                         \ \    "
                    VISIBLE "             /     /                            \  \  "
                    VISIBLE "           /     /             _----_            \   \"
                    VISIBLE "          /     /           _-~      ~-_         |   |"
                    VISIBLE "         (      (        _-~    _--_    ~-_     _/   |"
                    VISIBLE "          \      ~-____-~    _-~    ~-_    ~-_-~    / "
                    VISIBLE "            ~-_           _-~          ~-_       _-~  "
                    VISIBLE "               ~--______-~                ~-___-~     "
                    VISIBLE ""
                    VISIBLE ""
                    VISIBLE ""
                    VISIBLE "        U IZ WIN WITH :{snake_length} POINTZ!!        "
                NO WAI
                    VISIBLE "U IZ DIED ::("
                    VISIBLE "U SCORED :{points} POINTZ!!"
            OIC

            GTFO
    OIC
IM OUTTA YR game_loop

KTHXBYE
