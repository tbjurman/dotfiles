"
" Setup NeoBundle - BEGIN
"
if has('vim_starting')
  if &compatible
    set nocompatible               " Be iMproved
  endif

  " Required:
  set runtimepath+=/Users/tbjurman/.nvim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('/Users/tbjurman/.nvim/bundle'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" Add or remove your Bundles here:
" NeoBundle 'Shougo/neosnippet.vim'
" NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'dhruvasagar/vim-table-mode'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'TaskList.vim'
NeoBundle 'klen/python-mode'
NeoBundle 'shougo/vimproc.vim'
NeoBundle 'christoomey/vim-tmux-navigator'
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'tpope/vim-vinegar'
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'bling/vim-airline'
NeoBundle 'vim-airline/vim-airline-themes'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'jceb/vim-orgmode'
NeoBundle 'tpope/vim-speeddating'
NeoBundle 'tbjurman/vim-yang'
NeoBundle 'tbjurman/vim-lux'
NeoBundle 'jlanzarotta/bufexplorer'
NeoBundle 'grep.vim'
NeoBundle 'neomake/neomake'
" NeoBundle 'tpope/vim-dispatch'
" NeoBundle 'radenling/vim-dispatch-neovim'
" NeoBundle 'shougo/deoplete.nvim'

" You can specify revision/branch/tag.
" NeoBundle 'Shougo/vimshell', { 'rev' : '3787e5' }

" Required:
call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"
" Setup NeoBundle - END
"
" style

set exrc
set secure

augroup indent
    au!
    " au BufNewFile,BufRead * set ts=4 sts=4 sw=4 et
    au BufNewFile,BufRead *.erl,*.escript set ts=4 sts=4 sw=4 et ft=erlang colorcolumn=80
    au BufNewFile,BufRead *.java set sts=4 sw=4 et
    au BufNewFile,BufRead *.cs,*.pl set sts=4 sw=4 et
    au BufNewFile,BufRead *.py set colorcolumn=80
    au BufNewFile,BufRead *.cpp,*.c,*.h set sts=4 sw=4 sts=4 et colorcolumn=80
    au BufNewFile,BufRead *.xsl,*.xml,*.wsdl,*.wsdd,*.conf,*.config,*.conf.sample set ft=xml ts=4 sts=4 sw=4 et
    au BufNewFile,BufRead *.html set ft=html sts=4 sw=4 et
    au BufNewFile,BufRead *.css set ft=css sts=4 sw=4 et
    au BufNewFile,BufRead *.js set ft=javascript sts=4 sw=4 et
    au BufNewFile,BufRead *.xsd set ft=xsd sts=4 sw=4 et
    au BufNewFile,BufRead *.txt set ft= sts=4 sw=4 et
    au BufNewFile,BufRead *.yaml,parameters.internaltest,parameters.pubtest,parameters.prod set ft=yaml ts=4 sts=4 sw=4 et
    au BufNewFile,BufRead *.rst set sts=4 sw=4 et textwidth=79
    au BufNewFile,BufRead Defines.*,Makefile*,*.mak set ft=make ts=8 sts=8 sw=8 noet
    au BufNewFile,BufRead setup.rules* set ft=hog ts=8 sts=8 sw=8 noet
    au BufNewFile,BufRead *.md set ft=markdown ts=4 sts=4 sw=4 et
    au BufNewFile,BufRead init.vim set ft=vim ts=2 sts=2 sw=2 et
    au FileType python set omnifunc=pythoncomplete#Complete
    au BufNewFile,BufRead ft=sh set ts=4 sts=4 sw=4 et
augroup END

augroup misc
    " When editing a file, always jump to the last known cursor position.
    " Don't do it when the position is invalid or when inside an event handler
    " (happens when dropping a file on gvim).
    au!
    au BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \   exe "normal g`\"" |
      \ endif
augroup END

autocmd QuickFixCmdPost *grep* cwindow

au Syntax * syntax sync maxlines=256

syntax on
" set t_Co=256
" au VimEnter * colorscheme babymate256
" colorscheme babymate256
" LAST ONE USED: colorscheme jellybeans
" colorscheme alduin
" set background=dark
"
" LIGHT MODE
colorscheme lightcolors
hi ColorColumn ctermbg=brown
hi LineNr ctermfg=lightgray

let g:SuperTabDefaultCompletionType = "context"
set completeopt=menuone,longest,preview

set hlsearch
set incsearch
set ignorecase
set smartcase
set showmatch
set autoindent
set smartindent
set vb t_vb=
set ruler
set cmdheight=2
set number
set wildmenu wildmode=full
set modeline
set backspace=indent,eol,start
set shiftround
set showbreak=↪
" set clipboard+=unnamedplus
match SpecialKey '\s\+$'
set listchars+=eol:$
set scrolloff=1

"set statusline=%<%f\ %y%m%r%=%{\"[\".(&enc).\":\".(&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"}%k\ %b\ 0x%B\ %-14.(%l(%L),%c%V%)\ %P
" set statusline=%f%m%r%=%y\ %{&enc}/%{&fenc}\ %03b(%2Bh)\ C:%3c\ L:%4l/%4L

set laststatus=2

" source $VIMRUNTIME/ftplugin/man.vim

" some nice keyboard mappings
nnoremap <S-K> :Man <cword><CR>
nnoremap <C-n> :bn<CR>
nnoremap <C-P> :bp<CR>
nnoremap <F3>  :cn<CR>
nnoremap <F2>  :cp<CR>
nnoremap <F5> :BufExplorer<CR>
nnoremap <leader>f :BufExplorer<CR>
" nnoremap <leader>m :Make<cr>
nnoremap <leader>m :Neomake!<cr>
nnoremap <S-F> :CtrlP<cr>
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

" fix j an k wrapped line movement
nnoremap j gj
nnoremap k gk
" Make last typed word uppercase (insert mode)
inoremap <c-u> <esc>viwUA
" Make word under cursor uppercase (normal mode)
nnoremap <c-u> gUiw
" Use jk or jj in insert mode as an escape key
" inoremap jk <esc>
inoremap jj <esc>
" Enclose visually selected text in "
vnoremap <leader>" <esc>`<i"<esc>`>la"<esc>
" Toggle hlsearch
nnoremap <silent> <leader>h :setlocal hlsearch!<cr>
" Disable ex mode
nnoremap Q <nop>
" grep for word under cursor
":nnoremap <leader>g :silent execute "grep! -R " . shellescape(expand("<cWORD>")) . " ."<cr>:copen<cr>

" search and replace word under cursor
nnoremap <Leader>s :%s/\<<C-r><C-w>\>//g<Left><Left>

" Grep (needs grep.vim) for word under cursor in all files in current dir
nnoremap <Leader>g :execute 'Grep -w '.expand('<cword>').' *' \| copen<Left><Left><Left><Left><Left><Left><Left><Left><Left>
" nnoremap <Leader>g :execute 'Grep /\<'.expand('<cword>').'\>/gj *' \| copen<Left><Left><Left><Left><Left><Left><Left><Left><Left>

" function to remove trailing whitespace
function! RemoveTrailingWhiteSpace()
  %s/\s\+$//e
endfunction
command! RemoveTrailingWhiteSpace :call RemoveTrailingWhiteSpace()

" function to switch background color
function! ToggleBgColor()
  if &background ==? "dark"
    set background=light
  else
    set background=dark
  endif
endfunction
command! ToggleBgColor :call ToggleBgColor()

iab TIME <C-R>=strftime("%c")<CR>

nnoremap <leader>liof yiwo?liof("<ESC>pa: ~p~n", [<ESC>pa]),<ESC>

" map tComment
nnoremap <leader>c :TComment<cr>
vnoremap <leader>c :TComment<cr>

" Make gf and gF resolve Makefile variables
set isfname+=(,)
set includeexpr=substitute(v:fname,\ '$(\\(\\w\\+\\))',\ '$\\1',\ 'g')

" netrw
let g:netrw_list_hide = '^.*\.d$,^.*\.swp$,^.*\.beam$,^.*\.o$'
let g:netrw_winsize = 20
let g:netrw_liststyle = 3

" vim-erlang-skeletons
" let g:erl_author = "Tomas Bjurman"
" let g:erl_company = "Cisco"

" tweek vim-table-mode corners
let g:table_mode_corner = '+'
let g:table_mode_header_fillchar = '='

" vim-multiple-cursors
let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_next_key='<C-i>'
let g:multi_cursor_prev_key='<C-o>'
let g:multi_cursor_skip_key='<C-x>'
let g:multi_cursor_quit_key='<Esc>'

" Tasklist
let g:tlTokenList = ['FIXME', 'TODO', 'XXX', '%!']
let g:tlWindowPosition = 1

let g:ctrlp_map = '<S-F>'
let g:ctrlp_working_path_mode = ''
let g:ctrlp_regexp_search = 1
set wildignore+=*.so,*.swp,*.zip,*.gz,*.tgz,*.dep,*.d,*.o,*.fxs,*.beam

" Python Mode
let g:pymode_folding = 0
let g:pymode_lint_on_write = 0
let g:pymode_trim_whitespaces = 0
let g:pymode_rope = 0
"Values may be chosen from: `pylint`, `pep8`, `mccabe`, `pep257`, `pyflakes`.
let g:pymode_lint_checkers = ['pyflakes', 'pep8', 'naming', 'mccabe']
nnoremap <F7> :PymodeLint<CR>

" GitGutter colors (LIGHT)
" hi SignColumn ctermbg=gray
" hi GitGutterAdd ctermbg=gray ctermfg=green
" hi GitGutterChange ctermbg=gray ctermfg=yellow
" hi GitGutterDelete ctermbg=gray ctermfg=red
" hi GitGutterChangeDelete ctermbg=gray ctermfg=blue

" Deoplete
" let g:deoplete#enable_at_startup = 1

" Airline themes
let g:airline_theme='tomorrow'

" Define command :R which acts like :r but reads the result into a new
" buffer
:command! -nargs=* -complete=shellcmd R new | setlocal buftype=nofile bufhidden=hide noswapfile | r !<args>

" Get the complete visual selection
function! s:get_visual_selection()
  " Why is this not a built-in Vim script function?!
  let [l:lnum1, l:col1] = getpos("'<")[1:2]
  let [l:lnum2, l:col2] = getpos("'>")[1:2]
  let l:lines = getline(l:lnum1, l:lnum2)
  let l:lines[-1] = l:lines[-1][: l:col2 - (&selection == 'inclusive' ? 1 : 2)]
  let l:lines[0] = l:lines[0][l:col1 - 1:]
  return l:lines
endfunction

function! s:get_valid_dpaste_lexer(filetype)
  let l:valid_lexers = [
        \    'text', 'plain', 'ragel-java', 'coffee-script', 'handlebars', 'less', 'antlr', 'lean', 'lsl', 'golo',
        \    'newspeak', 'abap', 'roboconf-instances', 'genshi', 'csharp', 'autoit', 'smali', 'moon', 'zephir',
        \    'camkes', 'lasso', 'ini', 'cython', 'console', 'gosu', 'arduino', 'praat', 'spec', 'opa', 'nimrod',
        \    'fortran', 'matlab', 'ada', 'vala', 'adl', 'erlang', 'bnf', 'limbo', 'redcode', 'psql', 'xtend',
        \    'ragel-cpp', 'rts', 'easytrieve', 'dpatch', 'swig', 'ps1con', 'rbcon', 'dart', 'jcl', 'fan', 'julia',
        \    'ebnf', 'myghty', 'vim', 'odin', 'aspectj', 'pov', 'chapel', 'maql', 'bat', 'lighty', 'matlabsession',
        \    'd', 'terraform', 'cobolfree', 'tcl', 'robotframework', 'splus', 'perl6', 'modelica', 'pan', 'terminfo',
        \    'clay', 'clojurescript', 'squidconf', 'jsp', 'cryptol', 'thrift', 'as3', 'termcap', 'live-script',
        \    'cfm', 'csound-score', 'rhtml', 'modula2', 'scheme', 'apacheconf', 'mysql', 'cfc', 'ec', 'tex', 'go',
        \    'prolog', 'qml', 'mxml', 'urbiscript', 'cirru', 'croc', 'php', 'haxeml', 'smarty', 'twig', 'tcsh', 'csound',
        \    'scala', 'tcshcon', 'applescript', 'boogie', 'cbmbas', 'ezhil', 'mason', 'rd', 'lua', 'ceylon', 'sml',
        \    'trac-wiki', 'rb', 'qbasic', 'racket', 'irc', 'jasmin', 'rconsole', 'red', 'mql', 'xml', 'bc', 'asy',
        \    'ecl', 'pacmanconf', 'basemake', 'js', 'i6t', 'x10', 'slim', 'jade', 'elixir', 'resource', 'java',
        \    'swift', 'properties', 'bash', 'monkey', 'coq', 'lagda', 'c', 'postgresql', 'kconfig', 'lcry', 'kotlin',
        \    'elm', 'ragel-em', 'ocaml', 'befunge', 'scilab', 'idl', 'cuda', 'smalltalk', 'boo', 'cucumber', 'pytb',
        \    'clojure', 'python3', 'rebol', 'sparql', 'fsharp', 'dylan-lid', 'octave', 'gooddata-cl', 'jsonld',
        \    'csound-document', 'pot', 'scaml', 'fancy', 'ts', 'bro', 'raw', 'treetop', 'haskell', 'puppet', 'apl',
        \    'ragel-ruby', 'diff', 'perl', 'cadl', 'rql', 'pycon', 'evoque', 'hexdump', 'jlcon', 'ioke', 'abnf',
        \    'foxpro', 'yaml', 'openedge', 'pkgconfig', 'nesc', 'shen', 'json', 'common-lisp', 'emacs', 'tea', 'groff',
        \    'numpy', 'fish', 'rsl', 'hx', 'dylan-console', 'ragel-d', 'ooc', 'pike', 'python', 'newlisp', 'factor',
        \    'gas', 'nixos', 'earl-grey', 'rexx', 'stan', 'xquery', 'rst', 'jags', 'turtle', 'parasail', 'genshitext',
        \    'groovy', 'logos', 'duel', 'cypher', 'bbcode', 'isabelle', 'cfengine3', 'ragel', 'j', 'inform6', 'dtd',
        \    'doscon', 'nemerle', 'erb', 'pypylog', 'erl', 'blitzmax', 'qvto', 'powershell', 'rust', 'inform7', 'control',
        \    'tap', 'iex', 'pawn', 'igor', 'fortranfixed', 'glsl', 'chai', 'cpsa', 'pig', 'postscript', 'as',
        \    'roboconf-graph', 'cobol', 'at', 'io', 'todotxt', 'mupad', 'haml', 'objective-j', 'objective-c', 'hylang',
        \    'mako', 'ssp', 'blitzbasic', 'cmake', 'gnuplot', 'tads3', 'mscgen', 'make', 'mathematica', 'minid',
        \    'sourceslist', 'snobol', 'html', 'plpgsql', 'vb.net', 'crmsh', 'cheetah', 'hybris', 'text', 'css', 'vgl',
        \    'llvm', 'http', 'protobuf', 'koka', 'componentpascal', 'eiffel', 'felix', 'kal', 'sc', 'py3tb', 'awk', 'sql',
        \    'ragel-c', 'idris', 'gap', 'agda', 'nit', 'lidr', 'scss', 'dylan', 'gst', 'sass', 'nginx', 'vctreestatus',
        \    'ragel-objc', 'sp', 'mask', 'bugs', 'django', 'nsis', 'logtalk', 'lhs', 'alloy', 'velocity', 'docker',
        \    'moocode', 'delphi', 'xslt', 'nas']

  if index(l:valid_lexers, a:filetype) >= 0
    return a:filetype
  else
    return 'plain'
  endif
endfunction

" Cisco Dpaste visual selection
function! Dpaste()
  let l:tempname = tempname()
  call writefile(s:get_visual_selection(), l:tempname)
  let l:lexer = s:get_valid_dpaste_lexer(&filetype)
  let l:cmd="curl -s -F 'lexer=" . l:lexer . "' -F 'content=<-' https://dpaste.cisco.com/api/ < " . l:tempname
  let l:raw = system(l:cmd)
  copen 2
  setlocal modifiable
  setlocal filetype=dpasteurl
  setlocal buftype=nofile
  call append(0, 'Snippet published to ' . l:raw[1:len(l:raw)-2])
  normal! dd
  setlocal nomodifiable
endfunction

command! -range Dpaste :call Dpaste()


" Neomake statusline Spinner
"
" let s:spinner_index = 0
" let s:active_spinners = 0
" let s:spinner_states = ['|', '/', '--', '\', '|', '/', '--', '\']
" let s:spinner_states = ['┤', '┘', '┴', '└', '├', '┌', '┬', '┐']
" let s:spinner_states = ['←', '↖', '↑', '↗', '→', '↘', '↓', '↙']
" let s:spinner_states = ['←', '↑', '→', '↓']
" let s:spinner_states = ['d', 'q', 'p', 'b']
" let s:spinner_states = ['.', 'o', 'O', '°', 'O', 'o', '.']
" let s:spinner_states = ['■', '□', '▪', '▫', '▪', '□', '■']
"
" function! StartSpinner()
"     let b:show_spinner = 1
"     let s:active_spinners += 1
"     if s:active_spinners == 1
"         let s:spinner_timer = timer_start(1000 / len(s:spinner_states), 'SpinSpinner', {'repeat': -1})
"     endif
" endfunction
"
" function! StopSpinner()
"     let b:show_spinner = 0
"     let s:active_spinners -= 1
"     if s:active_spinners == 0
"         :call timer_stop(s:spinner_timer)
"     endif
" endfunction
"
" function! SpinSpinner(timer)
"     let s:spinner_index = float2nr(fmod(s:spinner_index + 1, len(s:spinner_states)))
"     redraw
" endfunction
"
" function! SpinnerText()
"     if get(b:, 'show_spinner', 0) == 0
"         return " "
"     endif
"
"     return s:spinner_states[s:spinner_index]
" endfunction

augroup neomake_hooks
    au!
    " autocmd User NeomakeJobInit :call StartSpinner()
    autocmd User NeomakeJobInit :echom "Build started"
    " autocmd User NeomakeFinished :call StopSpinner()
    autocmd User NeomakeFinished :echom "Build complete"
augroup END
