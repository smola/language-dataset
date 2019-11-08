note
	description: "An image from a custom package file."
	author: "Louis Marchand"
	date: "Thu, 02 Apr 2015 03:46:04 +0000"
	revision: "2.0"

class
	IMG_IMAGE_CPF

inherit
	IMG_IMAGE_FILE
		rename
			make as make_file
		redefine
			open, is_cur, is_ico, is_bmp, is_pnm, is_xpm,
			is_xcf, is_pcx, is_gif, is_jpg, is_tif, is_png, is_lbm, is_xv
		end
	CPF_RESSOURCE_MANAGER
		undefine
			default_create
		redefine
			make
		end

create
	make

feature {NONE} -- Initialization

	make(a_cpf:CPF_PACKAGE_FILE;a_cpf_index:INTEGER)
			-- make `Current' from the image in the custom package file `a_cpf' at `a_cpf_index'.
		do
			Precursor {CPF_RESSOURCE_MANAGER}(a_cpf, a_cpf_index)
			rwop:={GAME_SDL_EXTERNAL}.SDL_AllocRW
			cpf.lock_mutex
			cpf.select_sub_file (cpf_index)
			{GAME_SDL_EXTERNAL}.setSDLRWops(rwop,cpf.internal_pointer)
			last_position:=cpf.position
			cpf.unlock_mutex
		end

feature -- Access

	is_cur:BOOLEAN
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Result:=Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

	is_ico:BOOLEAN
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Result:=Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

	is_bmp:BOOLEAN
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Result:=Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

	is_pnm:BOOLEAN
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Result:=Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

	is_xpm:BOOLEAN
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Result:=Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

	is_xcf:BOOLEAN
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Result:=Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

	is_pcx:BOOLEAN
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Result:=Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

	is_gif:BOOLEAN
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Result:=Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

	is_jpg:BOOLEAN
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Result:=Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

	is_tif:BOOLEAN
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Result:=Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

	is_png:BOOLEAN
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Result:=Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

	is_lbm:BOOLEAN
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Result:=Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

	is_xv:BOOLEAN
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Result:=Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

	open
			-- <Precursor>
		do
			cpf.lock_mutex
			cpf.go (last_position)
			Precursor {IMG_IMAGE_FILE}
			last_position:=cpf.position
			cpf.unlock_mutex
		end

feature {NONE}

	last_position:INTEGER

end
