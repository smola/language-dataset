note
	description : "Objects that ..."
	author      : "$Author: jfiat $"
	date        : "$Date: 2006-03-10 11:17:08 +0100 (Fri, 10 Mar 2006) $"
	revision    : "$Revision: 16 $"

deferred
class
	MODIFIED_RAW_FILE_I

inherit
	RAW_FILE
		rename
			off as old_off,
			move as old_move,
			go as old_go,
			count as old_count,
			position as old_position,
			back as old_back,
			forth as old_forth,
			readable  as old_readable,
			read_line as old_read_line,
			read_stream as old_read_stream,
			file_readable  as old_file_readable,
			read_character as old_read_character
		end

end

