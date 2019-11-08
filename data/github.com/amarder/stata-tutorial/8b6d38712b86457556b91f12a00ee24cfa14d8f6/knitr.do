capture program drop knit
program define knit
  args in

  _knit_with_code_tags "`in'"
  _make_code_blocks "`in'"
end

capture program drop _knit_with_code_tags
program define _knit_with_code_tags
  args in

  set more off

  file open f using "`in'", read

  local out = subinstr("`in'", ".domd", ".md1", 1)
  log using "`out'", text replace

  local in_code_block = 0
  file read f line
  while r(eof) == 0 {
    if substr("`line'", 1, 4) == "    " {
      if !`in_code_block' {
        display "<code>"
        local in_code_block = 1
      }
      else {
        display ""
      }
      display ". `=ltrim("`line'")'"
      `line'
    }
    else {
      if `in_code_block' {
        display "</code>"
        local in_code_block = 0
      }
      display "`line'"
    }
    file read f line
  }

  log close
  file close f
end


capture program drop _make_code_blocks
program define _make_code_blocks
  args in

  local out = subinstr("`in'", ".domd", ".md", 1)
  file open f_in using "`out'1", read
  file open f_out using "`out'", write replace

  local in_code_block = 0
  local footer = 0

  file read f_in line
  local line_no = 1
  while r(eof) == 0 {
    local header = `line_no' <= 5
    local footer = ("`line'" == "      name:  <unnamed>" & !`header') | `footer'
    if "`line'" == "<code>" {
      local in_code_block = 1
    }
    else if "`line'" == "</code>" {
      local in_code_block = 0
    }
    else {
      if `in_code_block' {
        file write f_out "    `line'" _n
      }
      else {
        if !`header' & !`footer' {
          file write f_out "`line'" _n
        }
      }
    }
    file read f_in line
    local line_no = `line_no' + 1
  }

  file close f_in
  file close f_out
end


knit "clustered-standard-errors.domd"
