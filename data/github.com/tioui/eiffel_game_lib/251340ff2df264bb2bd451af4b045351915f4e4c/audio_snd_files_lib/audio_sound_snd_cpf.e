note
	description: "Summary description for {AUDIO_SOUND_CPF}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	AUDIO_SOUND_SND_CPF

inherit
	AUDIO_SOUND_SND_FILE
	rename
		make as make_file
	undefine
		restart
	redefine
		dispose,
		fill_buffer
	end
	CPF_RESSOURCE

create
	make

feature {NONE} -- Initialization

	make(a_cpf:CPF_PACKAGE_FILE;a_index:INTEGER)
			-- Initialization for `Current'.
		require
			Sound_Cpf_Index_Valid:a_index>0 and then a_index<=a_cpf.sub_files_count
		do
			cpf:=a_cpf
			cpf_index:=a_index
			cpf.mutex_lock
			cpf.select_sub_file (cpf_index)
			last_offset:=cpf.current_offset
			virtual_io:=virtual_io.memory_alloc ({AUDIO_SND_FILES_EXTERNAL}.c_sizeof_snd_file_virtual_io)
			{AUDIO_SND_FILES_EXTERNAL}.set_snd_file_virtual_io(virtual_io)
			file_info:={AUDIO_SND_FILES_EXTERNAL}.c_sf_info_struct_allocate
			snd_file_ptr:={AUDIO_SND_FILES_EXTERNAL}.sf_open_virtual(virtual_io,{AUDIO_SND_FILES_EXTERNAL}.SFM_READ,file_info,a_cpf.get_current_cpf_infos_ptr)
			check not snd_file_ptr.is_default_pointer end
			last_offset:=cpf.current_offset
			cpf.mutex_unlock
		end

feature {GAME_AUDIO_SOURCE}

	fill_buffer(a_buffer:POINTER;a_max_length:INTEGER)
		do
			cpf.mutex_lock
			if cpf.current_file_index/=cpf_index or else not cpf.current_offset_is_in_selected_file  then
				cpf.select_sub_file (cpf_index)
			end
			if cpf.current_offset/=last_offset then
				cpf.seek_from_begining (last_offset)
			end
			precursor(a_buffer,a_max_length)
			if cpf.current_offset_is_in_selected_file then
				last_offset:=cpf.current_offset
			else
				last_offset:=0
			end
			cpf.mutex_unlock
		end

feature-- Access

	restart
		local
			error: INTEGER_64
		do
			cpf.mutex_lock
			cpf.select_sub_file (cpf_index)
			if is_seekable then
				error := {AUDIO_SND_FILES_EXTERNAL}.sf_seek (snd_file_ptr, 0, {AUDIO_SND_FILES_EXTERNAL}.seek_set)
				check
					error /= -1
				end
			else
				cpf.select_sub_file (cpf_index)
				cpf.seek_from_begining (0)
			end
			last_offset:=cpf.current_offset
			cpf.mutex_unlock
		end


feature {NONE} -- Implementation - Routines

	dispose
		do
			Precursor
			virtual_io.memory_free
		end
feature {NONE} -- Implementation - Variables

	virtual_io:POINTER
	cpf:CPF_PACKAGE_FILE
	cpf_index:INTEGER
	last_offset:INTEGER

end
