#include "slick.sh"

_command make_func() {
  _str line;
  _str return_type;
  _str func_name;
  _str class_name;
  _str func_definition;

  int interface_file = 0;
  if ( !strcmp(_get_extension(p_buf_name), 'h') )
    interface_file = 1;

  get_line( line );
  class_name = current_class(false);
  beautify(true);

  int left_paren = pos('(', line);
  int last_space = lastpos(' ', substr(line, 1, left_paren));

  return_type = substr(line, 1, last_space - 1);
  func_name = substr(line, last_space + 1);
  func_name = strip(func_name, 'T', ';'); 

  if ( interface_file ) {
    class_name = substr(class_name, pos('/', class_name) + 1);
    edit_counterpart(); 
    bottom_of_buffer();
    find('^}', 'U-');
    cursor_up();
    _insert_text("\n" return_type " " class_name "::" func_name " {\n\n}\n"); 
    beautify(true);
    cursor_up();
  }

  prev_func();
  cursor_down();
  cursor_down();
  _insert_text("  ");
}
