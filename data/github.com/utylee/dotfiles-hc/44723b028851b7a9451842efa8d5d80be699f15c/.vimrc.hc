set nocompatible
"source $VIMRUNTIME/vimrc_example.vim
"source $VIMRUNTIME/mswin.vim
"behave mswin

set iskeyword+=-

" ctrlp가 ag를 사용하게 합니다
"set grepprg=ag\ --nogroup\ --nocolor
set grepprg=rg\ --color=never
" Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
"let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
let g:ctrlp_user_command = 'rg %s --files --color=never --no-ignore'
" " ag is fast enough that CtrlP doesn't need to cache
let g:ctrlp_use_caching = 0
"if executable('ag')
"endif

let g:simple_todo_map_normal_mode_keys = 0

set rtp+=~/.fzf
let g:fzf_history_dir = '~/.fzf/fzf-history'
command! -bang -nargs=* Ag call fzf#vim#ag(<q-args>, '--hidden', <bang>0)

function! s:find_git_root()
  return system('git rev-parse --show-toplevel 2> /dev/null')[:-2]
endfunction

command! ProjectFiles execute 'Files' s:find_git_root()

"command! -bang -nargs=* Rg
  "\ call fzf#vim#grep(
  "\   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  "\   <bang>0 ? fzf#vim#with_preview('up:60%')
  "\           : fzf#vim#with_preview('right:50%:hidden', '?'),
  "\   <bang>0)
"command! -bang -nargs=* Ag call fzf#vim#ag(<q-args>, '--hidden --path-to-ignore ~/.ignore', <bang>0)
""let g:fzf_ag_raw =1
"command! -bang -nargs=* Ag call fzf#vim#ag(<q-args>, '--ignore "*json"', <bang>0)
"command! -bang -nargs=+ -complete=dir Rag call fzf#vim#ag_raw(<q-args>, {'options': '--delimiter : --nth 4..'}, <bang>0)
"command! -bang -nargs=+ -complete=dir Rag call fzf#vim#ag_raw(<q-args>, {}, <bang>0)
"set term=screen-256color
set backspace=indent,eol,start

" esc 누를시 딜레이를 없애줍니다
" 참고사이트 : https://www.johnhawthorn.com/2012/09/vi-escape-delays/
set timeoutlen=1000 ttimeoutlen=10

""기본 자동완성 기능
""http://vim.wikia.com/wiki/Make_Vim_completion_popup_menu_work_just_like_in_an_IDE
""참고
""
"" 이걸 빼고 아래omni complete를 기본 ctrl n 으로 동작하게 바꿔버렸습니다.
"" 첫번째 항목 선택이 이상해서말이죠
""
"set completeopt=longest,menuone,preview
"inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
"
""inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
"
"inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
  "\ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
"inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
  "\ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
""
"" complete 완성후 :pclose 로 프리뷰윈도우 닫는 명령
"" If you prefer the Omni-Completion tip window to close when a selection is
"" made, these lines close it on movement in insert mode or when leaving
"" insert mode
"autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
"autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" 변경된 버퍼를 저장하지 않고도 버퍼간 이동을 가능하게끔합니다
set hidden
set tags=tags;/

" bashrc 의 alias를 읽기 위한 설정입니다
"let $BASH_ENV = "~/.bashrc"
let $BASH_ENV = "~/.bash_functions"

"if exists('$TMUX')
  "let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  "let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
"else
  "let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  "let &t_EI = "\<Esc>]50;CursorShape=0\x7"
"endif
"osx 터미널 상에서의 인서트모드 커서를 변경합니다.
"let &t_SI = "\<Esc>]50;CursorShape=1\x7"
"let &t_EI = "\<Esc>]50;CursorShape=0\x7"
"osx + tmux 상에서의 인서트모드 커서를 변경합니다.
"let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
"let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"


""""""""""""""""""
" 일반 ubuntu - linux 상에서의 커서설정
"

"에디트(이동)모드커서
let &t_EI .= "\<Esc>[1 q"
"수정(인서트)모드커서
let &t_SI .= "\<Esc>[5 q"

" solid block
" let &t_EI .= "\<Esc>[1 q"
  " 1 or 0 -> blinking block
  " 3 -> blinking underscore
  "	4 -> solid underscore
  " Recent versions of xterm (282 or above) also support
  " 5 -> blinking vertical bar
  " 6 -> solid vertical bar

let &t_8f="\e[38;2;%ld;%ld;%ldm"
let &t_8b="\e[48;2;%ld;%ld;%ldm"

"set guicolors  "vim 8.0에서 true color 적용문법 번경
set termguicolors
" backround column erase 문제 수정 문법
set t_ut=

"set t_Co=256
"set t_Co=16
"let g:solarized_termcolors=256
"let g:solarized_termcolors=16
"let g:solarized_termtrans=0


"set diffexpr=MyDiff()
"function MyDiff()
  "let opt = '-a --binary '
  "if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
  "if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
  "let arg1 = v:fname_in
  "if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
  "let arg2 = v:fname_new
  "if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
  "let arg3 = v:fname_out
  "if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
  "let eq = ''
  "if $VIMRUNTIME =~ ' '
    "if &sh =~ '\<cmd'
      "let cmd = '""' . $VIMRUNTIME . '\diff"'
      "let eq = '"'
    "else
      "let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
    "endif
  "else
    "let cmd = $VIMRUNTIME . '\diff'
  "endif
  "silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3 . eq
"endfunction

"set runtimepath=~/.vim
"==================================================================
"set nocompatible
"filetype off
"set rtp+=~/vimfiles/bundle/vundle
"call vundle#rc('~/vimfiles/bundle')
"Plugin 'gmarik/vundle'
""Plugin 'The-NERD-tree'
"Plugin 'jistr/vim-nerdtree-tabs' "닫을때tree도같이닫아줌
"
"Plugin 'AutoComplPop' "단어자동완성
""Plugin 'SuperTab'
"Plugin 'SuperTab-continued.'
"Plugin 'a.vim' "헤더-소스 전환
"Plugin 'bufexplorer.zip' "파일의 History
""Plugin 'qtpy.vim'
"Plugin 'flazz/vim-colorschemes'
""Plugin 'Python-mode-klen'
""Plugin 'cqml.vim'
""Plugin 'number-marks'
""Plugin 'qt2vimsyntax'
"filetype plugin indent on
"syntax on
"
""autocmd VimEnter * if &filetype !=# 'gitcommit'| NERDTree | wincmd p | endif 
""autocmd BufEnter * if &filetype !=# 'gitcommit'| NERDTree | wincmd p | endif
"autocmd VimEnter * NERDTree
"autocmd BufEnter * NERDTreeMirror
"
"autocmd VimEnter * wincmd w
"
"let g:NERDTreeWinPos = "right"
"let g:NERDTreeWinSize = 36
"let g:nerdtree_tabs_open_on_gui_startup=1

"==================================================================

execute pathogen#infect()

filetype plugin indent on
syntax on

"for ncm2
autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect
set nocompatible

"python에서 $2 $1 이런게 나와서 일단 아래 vim lsp를 사용하기로 변경
"let g:LanguageClient_serverCommands = {
	"\ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    "\ }

	"\ 'css': ['css-languageserver', '--stdio'],
	"\ 'python': ['~/.pyenv/shims/pyls'],
" ternjs 를 사용하므로 제거
"\ 'javascript.jsx': ['tcp://127.0.0.1:2089'],
"\ 'javascript': ['javascript-typescript-stdio'],

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
" Or map each action separately
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>


"let g:asyncomplete_smart_completion = 1
"let g:asyncomplete_auto_popup = 1
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"
"imap <c-space> <Plug>(asyncomplete_force_refresh)
"set completeopt+=preview
"autocmd CompleteDone * if pumvisible() == 0 | pclose | endif
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
    " pip install python-language-server
au User lsp_setup call lsp#register_server({
	\ 'name': 'css-lc',
	\ 'cmd': {server_info->[&shell, &shellcmdflag, 'css-languageserver --stdio']},
	\ 'whitelist': ['css'],
	\ })
if executable('pyls')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
		\ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
endif
if executable('rls')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'rls',
        \ 'cmd': {server_info->['rustup', 'run', 'stable', 'rls']},
        \ 'whitelist': ['rust'],
        \ })
endif
let g:lsp_signs_enabled = 1         " enable signs
let g:lsp_diagnostics_echo_cursor = 1 " enable echo under cursor when in normal mode
""\ 'cmd': {server_info->['pyls']},

" Use deoplete.
"let g:python3_host_prog='/home/odroid/.pyenv/shims/python3'
"let g:deoplete#enable_at_startup = 0

""let g:deoplete#enable_at_startup = 1
"let g:deoplete#enable_at_startup = 0
"autocmd InsertEnter * call deoplete#enable()
"if !exists('g:deoplete#omni#input_patterns')
  "let g:deoplete#omni#input_patterns = {}
"endif
"autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif


"let g:virtualenv_directory = '/home/utylee/00-Projects/venv-tyTrader'
set laststatus=2
let g:airline_powerline_fonts = 1
let g:airline#extensions#virtualenv#enabled = 0
let g:airline#extensions#tagbar#flags = 'f'

"let g:airline_section_a = airline#sections#create(['mode', %{airline#extensions#branch#get_head()}''branch'])

"function! AirlineWrapper(ext)
	"let head = airline#extensions#branch#head()
	"return empty(head) ? '' : printf(' %s', airline#extensions#branch#get_head())
"endfunction

"let g:airline_section_c = airline#section#create(['%f'])

"이 두개가 실제로 사용되고 있었습니다. 일단 제거
"let g:airline_section_a = airline#section#create(['mode', ' ', '%{airline#extensions#branch#get_head()}'])
"let g:airline_section_b = airline#section#create(['%{virtualenv#statusline()}'])
"let g:airline_section_b = airline#section#create(['%f'])
"let g:airline_section_b = ''
let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#whitespace#enabled = 0
let g:airline_section_c = '%t'
" tagbar 업데이트가 너무 느려서 확인해보니 기본 4000이었습니다
set updatetime=1000
au VimEnter * let g:airline_section_x = airline#section#create(['tagbar']) | :AirlineRefresh

"au VimEnter * let g:airline_section_x = airline#section#create_right(['tagbar']) | :AirlineRefresh
"let g:airline_section_x = airline#section#create_right(['tagbar']) 
"skip section을 하니 tagbar가 동작을 안했습니다
"let g:airline_skip_empty_sections = 1
"let g:airline_section_y=''
"let g:airline_section_z=''
let g:airline_section_warning=''
let g:airline_section_error=''
let g:airline_section_statistics=''
let g:airline_mode_map = {
  \ '__' : '-',
  \ 'n'  : 'N',
  \ 'i'  : 'I',
  \ 'R'  : 'R',
  \ 'v'  : 'V',
  \ 'V'  : 'V-L',
  \ 'c'  : 'C',
  \ 's'  : 'S',
\ 'S' : 'S-L',
\ }

"let g:airline_section_a = airline#section#create(['mode', '%{AirlineWrapper()}'])
"let g:airline_section_b = airline#section#create([g:airline_symbols.branch, ' ', '%{fugitive#head()}', ' ', ' %{virtualenv#statusline()}'])
"let g:airline_section_b = airline#section#create(['%{airline#extensions#branch#get_head()}', ' %{virtualenv#statusline()}'])
"let g:airline_section_b = airline#section#create(['branch'])
"let g:airline_section_b = ['branch']
"let g:virtualenv_stl_format = '[%n]'
"let g:Powerline_symbols = 'fancy'

" Enable the list of buffers
let g:airline#extensions#tabline#enabled = 1

" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'

"function! MyOverride(...)
"	call a:l.add_section('StatusLine', 'all')
"	return l
"endfunction
"call airline#add_statusline_func('MyOverride')

"let g:jedi#auto_initialization = 1 
"let g:jedi#squelch_py_warning = 1


" emmet-vim 을 html과 css에서만 사용하는 설정

"let g:user_emmet_install_global = 0
"autocmd FileType html,css EmmetInstall

au BufRead,BufNewFile */etc/nginx/* set ft=nginx
au BufRead,BufNewFile */nginx/* set ft=nginx

set noundofile
set number
set cul
set ignorecase
set shiftwidth=4
set softtabstop=4
set nobackup
set noswapfile
"no equalalways or equalalways --> split 화면에서 사이즈 유즈 관련 세팅
set noea 

" 현재 파일의 디렉토리로 이동
set autochdir
" 만약 플러긴에서 문제가 생긴다면 아래대안을 사용할 것
"nnoremap ,cd :cd %:p:h<CR> 

if has("gui_running")
	"set lines=55
"	set columns=120
"	au GUIEnter * winpos 300 0
"
	"set guioptions -=T
	"set guioptions -=m
	set fullscreen
endif

set noshellslash
"map <F5> : !python3 %<CR>
"nmap <leader>e :!python3 %<CR>
"nmap <leader>e :!python3 '%:p'<CR>
"nmap <leader>e :set shellcmdflag=-ic <CR> :!ts python '%'<CR> <CR> :set shellcmdflag=-c<CR>
nmap <leader>e :!ts python '%:p' 2>/dev/null<CR> <CR>
nmap <leader>w :!ts cargo build --release<CR> <CR>
"nmap <leader>w :!ts rustc '%:t' 2>/dev/null<CR> <CR>
"nmap <leader>e :!ts python '%' 2>/dev/null<CR> <CR>
"현재 행을 실행하는 커맨드인데 공백제거가 안돼 아직 제대로 되지 않습니다
nmap <leader>r :Rooter<CR>
let g:rooter_manual_only = 1
"nmap <leader>w :exec '!ts python -c \"'getline('.')'\"'<CR>
nmap <leader>` :set fullscreen<CR>
nmap <leader>q :bd!<CR>
nmap <leader>Q :cclose<CR>
nmap <leader>c :!ts C-c<CR> <CR>
map <F7> :NERDTreeTabsToggle<CR>
map <F2> :NERDTreeToggle<CR>
nmap <leader>2 :NERDTreeToggle<CR>
"map <F1> :e $MYVIMRC<CR>
nmap <leader>1 :e $MYVIMRC<CR>
nmap <leader>5 :syntax sync fromstart<CR>
map <A-3> :tabnext<CR>
map <A-4> :tabprevious<CR>
"map <F3> :cn<CR>
"map <F4> :cp<CR>
"ex) :ccl<CR>       "Close the search result windows

"map <c-j> <c-w>j
"map <c-k> <c-w>k
"map <c-h> <c-w>h
"map <c-l> <c-w>l
"map <C-T> :tabnew<CR>:wincmd w<CR>

"let g:ctrlp_buftag_types = {
"\ 'css' : '--css-types=vcitm',
"\ }

" Setup some default ignores
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.(git|hg|svn)|\_site)$',
  \ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg|avi|mkv|mov|mp4|wma|xlsx|mp3|ini|doc|docx|un|bak)$',
\}

" 현재파일의 디렉토리로 변경 %->  상대경로파일명, :p-> 절대경로파일명, :h->
" 한마디전으로

nmap <leader>z :cd %:p:h<cr> :pwd<cr>



" Use the nearest .git directory as the cwd
" This makes a lot of sense if you are working on a project that is in version
" control. It also supports works with .svn, .hg, .bzr.
"let g:ctrlp_working_path_mode = 'r'

" Use a leader instead of the actual named binding
"nmap <leader>f :CtrlPCurWD<cr>
nmap <leader>v :Marks<cr>
nmap <leader>a :Rg<cr>
nmap <leader>l :BLines<cr>
nmap <leader>s :Tags<cr>
nmap <leader>d :BTags<cr>
nmap <leader>g :ProjectFiles<cr>
nmap <leader>f :Files<cr>
nmap <silent> <Leader>h :Rg <C-R><C-W><CR>
nmap <leader>x :Ag<cr>
nmap <leader>b :Buffers<cr>
nmap <leader>t :History<cr>		
nmap <leader>m :CtrlPMixed<cr>
"nmap <leader>d :CtrlPBufTagAll<cr>
"nmap <leader>a :CtrlPTag<cr>
"nmap <leader>b :CtrlPBuffer<cr>
"nmap <leader>t :CtrlPMRU<cr>

" Easy bindings for its various modes
"파일열기를 fzf를 사용해서 이것도 맞춰줘야합니다
"nmap <leader>bs :CtrlPMRU<cr>
let g:ctrlp_match_window = 'max:12'
let g:ctrlp_extensions = ['tag']

" Split size change
nmap <leader>- :resize -5<cr>
nmap <leader>= :resize +5<cr>

"d로 삭제시에 레지스터로 복사되는 것을 금지합니다
"noremap d "_d
"noremap dd "_dd
"noremap p "0p

"colorscheme molokai "oreilly jellybeans sweyla1 
"colorscheme oreilly "oreilly jellybeans sweyla1 
"colorscheme monokai "oreilly jellybeans sweyla1 
"set background=dark
"colorscheme solarized 
colorscheme solarized_sd_utylee

"let python_no_builtin_highlight = 1  
"let g:molokai_original = 1

"set air-line theme {dark, molokai, ...}
let g:airline_theme='molokai'
"let g:airline_theme='solarized'
"let g:airline_theme='dark'
"let g:airline_theme='tomorrow'
"let g:airline_theme='jellybeans'


"let g:jedi#completions_command = "<C-N>"

"autocmd BufNewFile,BufRead *.qml so c:\vim\vim74\ftplugin\qml.vim
autocmd BufNewFile,BufRead *.qml setf qml 


set langmenu=utf8
"lang mes en_US 
"set langmenu=en_US.UTF-8
set tabstop=4
"cd c:\_GoogleDrive\__시스템트레이딩\
set encoding=utf8
"set fileencodings=utf-8,cp949
set fileencodings=utf-8,cp949
"set langmenu=cp949
"set guifont=나눔고딕코딩:h12:cHANGEUL
"set guifontwide=나눔고딕코딩:h12:cHANGEUL
"set guifont=Lucida\ Console:h11:cDEFAULT
"set guifont=Consolas:h10.15:cDEFAULT
"set guifont=Consolas:h10.2:cANSI
"set guifont=Ubuntu\ Mono\ for\ Powerline:h15:cANSI
"set guifont=Ubuntu\ Mono:h15:cANSI
"set guifont=Ubuntu\ Mono\ derivative\ Powerline:h18.3
"set guifontwide=NanumGothicCoding:h23
set guifont=Ubuntu\ Mono\ derivative\ Powerline:h19
"set font=Ubuntu\ Mono\ derivative\ Powerline:h19
set guifontwide=NanumGothicCoding:h24
"set guifontwide=NanumGothicCoding:h15:cDEFAULT
"set guifontwide=Ubuntu:h15:cDEFAULT

"cd c:\_GoogleDrive\
"cd c:\Users\utylee\00-projects
"cd c:\Users\seoru\00-projects\00-python
"
"
"html tag % movement enable
"runtime macros/matchit.vim
