trained

shh brainfuck interpreter written in dogescript

shh such global variables wow much clean code

shh required to open the code and input files
so fs as fs

shh brainfuck input from the user
very input_code is ''
very input_data is ''

shh map from each bracket to the corresponding one
very pair_map is {}

shh index of the implicit byte pointer
very pointer_index is 0

shh array of bytes initialized to zero
very array is [0]

shh print text
such bark much text
    console dose loge with text
wow

shh print an error
such cry much error
    plz bark with 'ERROR'
    plz bark with error
wow

shh populate the pair_map
such populate_pair_map
    very open_brackets is []
    very index is 0
    much next index smaller input_code.length next index more 1
        very command is input_code[index]
        rly command is '['
            plz open_brackets.push with index
        but rly command is ']'
            rly open_brackets.length more 0
                very last_position is open_brackets.pop()
                pair_map[last_position] is index
                pair_map[index] is last_position
            but
                plz cry with 'Mismatched bracket'
            wow
        wow
    wow
wow

shh increment the pointer
such increment_pointer
    very next_index is pointer_index + 1
    rly next_index is array.length
        plz array.push with 0
    wow
    pointer_index is next_index
wow

shh decrement the pointer
such decrement_pointer
    rly pointer_index not 0
        pointer_index is pointer_index - 1
    but
        plz cry with 'Tried to decrement from index 0'
    wow
wow

shh increment the byte at the pointer
such increment_byte
    ++array[pointer_index]
wow

shh decrement the byte at the pointer
such decrement_byte
    --array[pointer_index]
wow

shh output the byte at the pointer
such output_byte
    shh get the value at the pointer
    very value_at_pointer is array[pointer_index]
    shh convert the integer to a character
    very output is String.fromCharCode(value_at_pointer)
    plz process.stdout.write with output
wow

shh input a byte and store it in the byte at the pointer
shh no return statement in language so cannot return early
such input_byte
    rly input_data.length bigger 0
        array[pointer_index] = input_data.charCodeAt(0)
        shh pop off the first element of the input
        input_data = input_data.slice(1)
    but
        plz cry with 'Attempted to input when no input was available'
    wow
wow

shh interpret the input
such interpret much current_index
    many current_index smaller input_code.length
        very command is input_code[current_index]
        rly command is '['
            rly array[pointer_index] is 0
                current_index is pair_map[current_index] + 1
            but
                ++current_index
            wow
        but rly command is ']'
            rly array[pointer_index] is 0
                ++current_index
            but
                current_index is pair_map[current_index]
            wow
        but
            rly command is '>'
                plz increment_pointer
            but rly command is '<'
                plz decrement_pointer
            but rly command is '+'
                plz increment_byte
            but rly command is '-'
                plz decrement_byte
            but rly command is '.'
                plz output_byte
            but rly command is ','
                plz input_byte
            wow
            ++current_index
        wow
    wow
wow

shh get the code and the input then start interpreting it
shh doge wants promises
such start
    plz fs.readFile with 'code', 'utf8' much err data
        rly err
            plz cry with err
        but
            input_code is data
            plz fs.readFile with 'input', 'utf8' much err data
                rly err
                    plz cry with err
                but
                    input_data is data
                    plz populate_pair_map
                    plz interpret with 0
                wow
             wow&
        wow
    wow&
wow

plz start
