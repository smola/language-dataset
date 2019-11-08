class TermViNormalMode is InputMode
  let win: TermWindow tag
  let input: TermInput tag

  new iso create(input': TermInput tag, w: TermWindow tag) =>
    win = w
    input = input'

  fun name(): String => "normal"

  fun ref apply(data: U8) =>
    match data
    // | 0x04 => input.quit() // ctrl-d
    // | 0x05 => end_key(input) // ctrl-e
    // | 0x06 => right(input) // ctrl-f
    // | 0x08 => _backspace(input) // ctrl-h
    // | 0x0E => _win.down() // ctrl-n
    // | 0x10 => _win.up() // ctrl-p
    // | 0x1B => esc(input)
    | 'h' => win.left()
    | 'j' => win.down()
    | 'k' => win.up()
    | 'l' => win.right()
    | 'i' => input.switch_mode_edit()
    | 'o' => win.insert_line(); input.switch_mode_edit()
    | 'O' => win.insert_line_above(); input.switch_mode_edit()
    | if data < 0x20 => None // unknown control character
    else
      // Insert.
      let inp: Array[U8] val = recover val [data] end
      // win.insert(inp)
    end

  fun ref up(ctrl: Bool, alt: Bool, shift: Bool) => win.up()
  fun ref down(ctrl: Bool, alt: Bool, shift: Bool) => win.down()
  fun ref left(ctrl: Bool, alt: Bool, shift: Bool) => win.left()
  fun ref right(ctrl: Bool, alt: Bool, shift: Bool) => win.right()
  fun ref home(ctrl: Bool, alt: Bool, shift: Bool) => win.home()
