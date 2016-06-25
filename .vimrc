" vim: foldmethod=marker

"Vundle {{{
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

"Bundle 'SuperTab'
Bundle 'surround.vim'
Bundle 'ervandew/supertab'
Bundle 'scrooloose/nerdtree'
Bundle 'majutsushi/tagbar'
"Bundle 'yegappan/mru'
Bundle 'nathanaelkane/vim-indent-guides.git'
Bundle 'kien/ctrlp.vim.git'
"Bundle 'pylint.vim'
Bundle 'mbbill/undotree'
Bundle 'TaskList.vim'
Bundle 'vim-scripts/renamer.vim'
Bundle 'jlanzarotta/bufexplorer'

Bundle 'Valloric/YouCompleteMe'
"Bundle 'davidhalter/jedi-vim.git'
"Bundle 'Shougo/neocomplcache.git'
Bundle 'SirVer/ultisnips'
Bundle 'honza/vim-snippets'

call vundle#end()
" }}}

augroup vimrcEx
au!
augroup END

""" General settings {{{

set nocompatible						" Don't be compatible with vi

set backup

"""" Searching and Patterns
set noignorecase
set incsearch							" show best match so far
set hlsearch							" Highlight matches to the search 

"""" Display
set background=dark						" I use dark background
colorscheme danny
set lazyredraw							" Don't repaint when scripts are running
set scrolloff=3							" Keep 3 lines below and above the cursor
set ruler								" line numbers and column the cursor is on
if version >= 703
	set relativenumber								" Show line numbering
endif
set numberwidth=1						" Use 1 col + 1 space for numbers

" tab labels show the filename without path(tail)
set guitablabel=%N/\ %t\ %M

" Remove scrollbars
set guioptions-=l
set guioptions-=L
set guioptions-=r
set guioptions-=R
" And remove the toolbar
set guioptions-=T
" Stop alt going to the menus
set winaltkeys=no
set mousemodel=popup

"""" Messages, Info, Status
set shortmess=
set showcmd								" Display what command is waiting for an operator
set ruler								" Show pos below the win if there's no status line
set laststatus=2						" Always show statusline, even if only 1 window
set report=0							" Notify me whenever any lines have changed
set confirm								" Y-N-C prompt if closing with unsaved changes
set modeline
" Turn this off so that one can see messages after inserting
set noshowmode 

"""" Editing
set backspace=2							" Backspace over anything! (Super backspace!)
set showmatch							" Briefly jump to the previous matching paren
set matchtime=2							" For .2 seconds
set tabstop=4							" Tab stop of 4
set shiftwidth=4						" sw 4 spaces (used on auto indent)
set softtabstop=4						" 4 spaces as a tab for bs/del

"""" Coding
set history=100							" 100 Lines of history
"set showfulltag							" Show more information while completing tags
filetype plugin on						" Enable filetype plugins
filetype plugin indent on				" Let filetype plugins indent for me
syntax on								" Turn on syntax highlighting
syntax sync fromstart
autocmd QuickFixCmdPost make cwindow

""""" Folding
set foldmethod=marker					" By default, use syntax to determine folds
"set foldlevelstart=99					" All folds open by default

"""" Command Line
set wildmenu							" Autocomplete features in the status bar
set wildmode=longest:full

" When really desperate
set printoptions=paper:a4,left:2pc,right:2pc,top:2pc,bottom:2pc,number:y,syntax:n
set printfont=courier:h8

set switchbuf=useopen,split

" }}}

" General editing {{{

"""" F7 Spelling
if v:version >= 700
function! <SID>ToggleSpell()
   if &spell != 1
	   setlocal spell spelllang=en_au
   else
	   setlocal spell!
   endif
endfunction
nnoremap <silent> <F7> <ESC>:call <SID>ToggleSpell()<CR>
endif

map <silent> <F8> :TagbarToggle<CR>
map <silent> <F6> :NERDTreeToggle<CR>
map <silent> <F7> :UndotreeToggle<CR>
let NERDTreeQuitOnOpen=1

"""" Autocommands
augroup vimrcEx
	" In all files, try to jump back to the last spot cursor was in before 
	" exiting
	au BufReadPost *
		\ if line("'\"") > 0 && line("'\"") <= line("$") |
		\   exe "normal g`\"" |
		\ endif

	" Switch to the directory of the current file, unless it's a help file.
	au BufEnter * if &ft != 'help' | silent! cd %:p:h | endif

	" Make folding not slow
	" http://vim.wikia.com/wiki/Keep_folds_closed_while_inserting_text
	autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
	autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif

	let g:rainbow         = 1
	"let g:rainbow_nested  = 1
	let g:rainbow_paren   = 1
	let g:rainbow_brace   = 1
	let g:rainbow_bracket = 1
	autocmd BufReadPost * source $HOME/.vim/rainbow_paren.vim
	autocmd BufNewFile  * source $HOME/.vim/rainbow_paren.vim

	autocmd FileType help nnoremap <CR> <C-]>
	autocmd FileType help nnoremap <BS> <C-T>
augroup END


"""" Key Mappings
" tab navigation (next tab) with alt left / alt right
nnoremap  <a-right>  gt
nnoremap  <a-left>   gT

" visual shifting (builtin-repeat)
vnoremap < <gv
vnoremap > >gv

" CTRL-g shows filename and buffer number, too.
nnoremap <C-g> 2<C-g>

" <C-l> redraws the screen and removes any search highlighting.
nnoremap <silent> <C-l> :nohl<CR><C-l>

" * and # search for next/previous of selected text when used in visual mode
vnoremap * y/<C-R>"<CR>
vnoremap # y?<C-R>"<CR>

" <space> toggles folds opened and closed
nnoremap <space> zA

" <space> in visual mode creates a fold over the marked range
vnoremap <space> zf

" Fix Y so it behaves as expected
map Y y$


""" Abbreviations
function! EatChar(pat)
	let c = nr2char(getchar(0))
	return (c =~ a:pat) ? '' : c
endfunc

iabbr _me Daniel Cocks (daniel.cocks@jcu.edu.au)<C-R>=EatChar('\s')<CR>
iabbr _t  <C-R>=strftime("%H:%M:%S")<CR><C-R>=EatChar('\s')<CR>
iabbr _d  <C-R>=strftime("%a, %d %b %Y")<CR><C-R>=EatChar('\s')<CR>
iabbr _dt <C-R>=strftime("%a, %d %b %Y %H:%M:%S %z")<CR><C-R>=EatChar('\s')<CR>

" alternative (not working)
iabbr _// //////////////////////////////////////////////////////////////////////<C-R>=EatChar('\s')<CR>
iabbr _/- //////////////////////////////////////////////////////////////////////<CR>//---															 ---//<CR>//////////////////////////////////////////////////////////////////////<C-R>=EatChar('\s')<CR><Esc>k^ElR

iabbr _## ######################################################################<C-R>=EatChar('\s')<CR>
iabbr _#- ######################################################################<CR>##---                                                            ---##<CR>######################################################################<C-R>=EatChar('\s')<CR><Esc>k^ElR

iabbr _skelpy from __future__ import print_function<CR>from std_imports import *<CR><CR>from pylab import *<CR>from numpy import *<CR>

let g:indent_guides_auto_colors = 0

set thesaurus+=~/.vim/mthesaur.txt

cmap w!! w !sudo tee > /dev/null %

let g:showmarks_enable=0

let g:ctrlp_cmd = 'CtrlPMRU'

if has("persistent_undo")
	set undodir=$HOME/.undodir/
	set undofile
endif

" }}}

" GLSL filetypes {{{
augroup vimrcEx
au BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl,*.fragment,*.vertex setf glsl
augroup END
" }}}

" Autocompletions {{{

set completeopt=menuone,longest,preview

"let g:jedi#completions_enabled = 0
let g:jedi#auto_initialization = 0

let g:ycm_key_detailed_diagnostics = ''
let g:ycm_semantic_triggers = { 'python' : ['.'] }
"let g:ycm_server_keep_logfiles = 1
"let g:ycm_server_log_level = 'debug'

augroup vimrcEx

"au FileType python nnoremap <buffer> <leader>d :call jedi#goto_definitions()<CR>
"au FileType python nnoremap <buffer> <leader>a :call jedi#goto_assignments()<CR>
"au FileType python nnoremap <buffer> <leader>r :call jedi#rename()<CR>
"au FileType python nnoremap <buffer> <leader>n :call jedi#usages()<CR>
""au FileType python inoremap <buffer> <leader>c <C-o>:python jedi_vim.show_call_signatures()<CR>
"" The next two don't work - <expr> blocks buffer modifications
""au FileType python inoremap <buffer> <expr> , ShowCallSigs()
""au FileType python inoremap <buffer> <expr> ) ClearCallSigs()
"au FileType python inoremap <buffer> , <C-R>=ShowCallSigs() ? ',' : ','<CR>
"au FileType python inoremap <buffer> ) <C-R>=ClearCallSigs() ? ')' : ')'<CR>
""au FileType python inoremap <buffer> , <C-R>=pyeval("jedi_vim.show_call_signatures()") ? ',' : ','<CR>
""au FileType python inoremap <buffer> ) <C-R>=pyeval("jedi_vim.clear_call_signatures()") ? ')' : ')'<CR>
"au FileType python au InsertLeave * python jedi_vim.clear_call_signatures()
""au FileType python inoremap <buffer> ) <C-o>:python jedi_vim.clear_call_signatures()<CR>)

"function! ClearCallSigs()
"	python jedi_vim.clear_call_signatures()
"endfunction
"function! ShowCallSigs()
"	python jedi_vim.show_call_signatures()
"endfunction

"au FileType python inoremap ( <C-x><C-o>(

au FileType cpp nnoremap <buffer> <leader>h :YcmCompleter GoToDeclaration<CR>
au FileType cpp nnoremap <buffer> <leader>d :YcmCompleter GoToDefinition<CR>
augroup END

let g:UltiSnipsExpandTrigger="<C-j>"
let g:UltiSnipsJumpForwardTrigger="<C-j>"
let g:UltiSnipsJumpBackwardTrigger="<C-b>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" }}}

" Python specific {{{

augroup vimrcEx
autocmd FileType python setl list listchars=tab:»·,trail:·

" prefer expand and detect what the python file is using
au FileType python let g:detectindent_preferred_expandtab = 1 | let g:detectindent_preferred_indent = 4 
" Make sure all syntax highlighting is enabled.
au FileType python let python_highlight_all = 1

au FileType python map <C-F12> :silent !ctags -R .<CR>

"au FileType python set comments=s:''',m:'''''',e:.''',:#*
au FileType python setlocal comments=:#*
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete

au FileType python setlocal formatoptions=tcrqaw21
au FileType python setlocal textwidth=79
au FileType python setlocal expandtab
au FileType python setlocal ts=4 sw=4 sts=4

"au FileType python compiler pylint

augroup END

" My own attempt to make a better solution without formatting normal code
" Unfortunately, this requires the line before the docstring to end with no
" space. {{{
function! PythonFormatExpr()
	if v:count > 1
		echomsg "Exiting format expr because it's a block"
		return 1
	endif

	"echomsg "In PythonFormatExpr"
    "let cur_syntax = synIDattr(synIDtrans(synID(line("."), col("."), 0)), "name")
	"let cur_syntax = synIDattr(synID(line("."), col("."), 0), "name")
    "let cur_syntax = synIDattr(synID(line("."), col("$"), 0), "name")
    let cur_syntax = synIDattr(synID(v:lnum, col([v:lnum,"$"])-1, 0), "name")
	"echomsg v:lnum
	"echomsg col([v:lnum,"$"])
    if cur_syntax == "pythonComment"
		" Normal formatting
		"echomsg "In a comment"
        " We should also test to see if this comment is plain (i.e. just 
        " begins with #) or it is a long comment for which we should wrap 
        " (this is #* for my code)
        if match(getline(v:lnum), "#\\*") > -1
			echomsg "In a #* comment"
            return 1
        endif
		"echomsg "In a # comment"
        return 0
    elseif cur_syntax == "pythonString" || cur_syntax == "pythonRawString"
        " Check to see if we're in a docstring
        let lnum = v:lnum
        "while lnum >= 1 && (synIDattr(synIDtrans(synID(lnum, col([lnum, "$"]) - 1, 0)), "name") == "String" || match(getline(lnum), '\v^\s*$') > -1)
        while lnum >= 1 && synIDattr(synID(lnum, col([lnum, "$"]) - 1, 0), "name") == cur_syntax
            if match(getline(lnum), "\\('''\\|\"\"\"\\)") > -1
                " Assume that any longstring is a docstring

				" This should make the formatting take place normally
				"echomsg "In a long string"
                return 1
            endif
            let lnum -= 1
        endwhile
		"echomsg "In a short string"
		return 0
    endif

	" This should stop any other formatting
	"echomsg "In normal code"
    return 0
endfunction

augroup vimrcEx
au FileType python setl formatexpr=PythonFormatExpr()
augroup END
" }}}
" Python specific }}}

" C specific {{{
"au FileType cpp map <C-F12> :silent !ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>
augroup vimrcEx
au FileType cpp map <C-F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>
au FileType cpp map <C-k> :cnext<CR>

autocmd FileType cpp setl formatoptions+=aw
autocmd FileType cpp setl foldmethod=syntax
autocmd FileType cpp setl list listchars=trail:·
augroup END

let OmniCpp_GlobalScopeSearch = 1
let OmniCpp_NamespaceSearch = 1
let OmniCpp_DisplayMode = 1
let OmniCpp_ShowScopeInAbbr = 1
let OmniCpp_ShowPrototypeInAbbr = 1
let OmniCpp_ShowAccess = 1
let OmniCpp_DefaultNamespaces = ["std"]
let OmniCpp_MayCompleteDot = 1
let OmniCpp_MayCompleteArrow = 1
let OmniCpp_MayCompleteScope = 1
let OmniCpp_SelectFirstItem = 0

let g:c_no_if0 = 1
"let g:c_no_if0_fold = 1

" }}}

" Fortran specific {{{
augroup vimrcEx
autocmd FileType fortran let fortran_indent_less=1
autocmd FileType fortran let fortran_do_enddo=1
autocmd FileType fortran set ts=3 sw=3 sts=3
augroup END
" }}}

" Latex specific {{{
augroup vimrcEx
au FileType tex setlocal tw=80 fo+=aw
augroup END
" }}}

" {{{ Vim specific

augroup vimrcEx

autocmd FileType vim setl foldmethod=marker

augroup END

" }}}
""""" Keybindings

map <C-F2> :cd ~/src/ninjaninja/src<CR>:TlistSessionLoad ninjaninja.tlist<CR>:TlistToggle<CR>


" Old stuff that I'm hiding here. {{{
if 0

" This should no longer be needed with YCM {{{
if 0
function! FindTagTest(tag_command)
	let tokens = omni#cpp#items#Get(omni#cpp#utils#TokenizeCurrentInstruction(''))
	let typeinfo = omni#cpp#items#ResolveItemsTypeInfo(omni#cpp#namespaces#GetContexts(),tokens)

	let thisword = expand("<cword>")
	if len(typeinfo) > 0
		let class = typeinfo.value
		execute ":" . a:tag_command . " " . class . "::" . thisword
	else
		execute ":" . a:tag_command . " " . thisword
	endif
endfunction
"map _u :echo expand("<cword>")<CR>
"map <C-L> :execute ":ptag " expand("<cword>")<CR>
au FileType cpp map <C-M> :call FindTagTest("ptjump")<CR>
au FileType python map <C-M> :execute ":ptjump " expand("<cword>")<CR>
map <C-K> :ptnext<CR>
"map <C-Insert> :execute ":ptjump " expand("<cword>")<CR>
map <C-j> :cnext<CR>
endif
" }}}

" Show when beyond 80 character width
"autocmd BufWinEnter *.py,*.c,*.cpp,*.h call matchadd('Search', '\%<81v.\%>77v', -1)
"autocmd BufWinEnter *.py,*.c,*.cpp,*.h call matchadd('ErrorMsg', '\%>80v.\+', -1)

"let g:SuperTabDefaultCompletionType = 'context'
"let g:SuperTabContextDefaultCompletionType = '<c-p>'
"let g:SuperTabLongestEnhanced = 1
"autocmd FileType *
"  \ if &omnifunc != '' |
"  \   call SuperTabChain(&omnifunc, "<c-p>") |
"  \ endif

" Make the tab key useful {{{
function! TabWrapper()
  "if strpart(getline('.'), 0, col('.')-1) =~ '^\s*$'
  if strpart(getline('.'), col('.')-2, col('.')-1) =~ '^\s*$'
    return "\<Tab>"
  elseif exists('&omnifunc') && &omnifunc != ''
    return "\<C-X>\<C-O>"
  else
    return "\<C-N>"
  endif
endfunction
"autocmd FileType cpp,python imap <expr> <Tab> TabWrapper()
autocmd FileType cpp imap <expr> <Tab> TabWrapper()
" }}}
endif
" }}}

