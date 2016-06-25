" vim: tw=0 ts=4 sw=4
" Vim color file

hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "danny"
set background=dark
hi Normal		  ctermfg=white ctermbg=black guifg=white  guibg=black
hi Scrollbar	  guifg=darkcyan guibg=cyan
hi Menu			  guifg=black guibg=cyan
hi SpecialKey	  term=bold  cterm=bold  ctermfg=darkred  guifg=Cyan
hi NonText		  term=bold  cterm=bold  ctermfg=darkred  gui=bold	guifg=Blue
hi Directory	  term=bold  cterm=bold  ctermfg=brown	guifg=DarkCyan
hi ErrorMsg		  term=standout  cterm=bold  ctermfg=grey  ctermbg=blue  guifg=White  guibg=Red
hi Search		  term=reverse	ctermfg=white  ctermbg=red	guifg=Red  guibg=Yellow
hi MoreMsg		  term=bold  cterm=bold  ctermfg=darkgreen	gui=bold  guifg=SeaGreen
hi ModeMsg		  term=bold  cterm=bold  guifg=White gui=None guibg=Blue
hi LineNr		  term=underline  cterm=bold  ctermfg=darkcyan	guibg=#101020 guifg=#60a0a0
hi Question		  term=standout  cterm=bold  ctermfg=darkgreen	gui=bold  guifg=Green
hi StatusLine	  term=bold,reverse  cterm=None ctermfg=black ctermbg=green gui=None  guifg=black guibg=#60ff60
hi StatusLineNC   term=reverse	ctermfg=lightblue ctermbg=white gui=reverse guibg=white guifg=blue
hi VertSplit      cterm=None ctermfg=lightgreen ctermbg=black gui=bold guifg=#60ff60 guibg=black
hi Title		  term=bold  cterm=bold  ctermfg=darkmagenta  gui=bold	guifg=Magenta
hi Visual		  term=reverse	cterm=reverse  gui=reverse
hi WarningMsg	  term=standout  cterm=bold  ctermfg=Red guifg=Yellow
hi Cursor		  guifg=bg	guibg=Green
hi Comment		  term=bold  cterm=bold ctermfg=cyan  guifg=#80a0ff
hi Constant		  term=underline  cterm=bold ctermfg=magenta  guifg=#ffa0a0 gui=bold
hi String		  term=underline  cterm=bold ctermfg=magenta  guifg=#ff8060
hi Special		  term=bold  cterm=bold ctermfg=red  guifg=Orange
hi Identifier	  term=underline   ctermfg=brown  guifg=#40ffff
hi Statement	  term=bold  cterm=bold ctermfg=yellow	gui=None  guifg=#ffff60
hi PreProc		  term=underline  ctermfg=lightblue	guifg=#ff80ff
"hi Type			  term=underline  cterm=bold ctermfg=lightgreen gui=NONE guifg=#60ff60
"hi link Type Statement
hi Type	  term=bold  cterm=bold ctermfg=yellow	gui=None  guifg=#ffffa0
hi Error		  term=reverse	ctermfg=darkcyan  ctermbg=black  guifg=Red	guibg=Black
hi Todo			  term=standout  ctermfg=black	ctermbg=darkcyan  guifg=Blue  guibg=Yellow
hi CursorLine	  term=underline  guibg=#555555
hi CursorColumn	  term=underline  guibg=#555555
hi MatchParen	  term=reverse  guibg=Blue
hi TabLine		  term=bold,reverse  cterm=bold ctermfg=lightblue ctermbg=white gui=bold guifg=blue guibg=white
hi TabLineFill	  term=bold,reverse  cterm=bold ctermfg=lightblue ctermbg=white gui=bold guifg=blue guibg=white
hi TabLineSel	  term=reverse	ctermfg=white ctermbg=lightblue guifg=white guibg=blue
hi Folded gui=bold guifg=#a0f0f0 guibg=#003030
hi FoldColumn gui=bold guifg=Orange guibg=Purple
hi Pmenu ctermbg=DarkBlue ctermfg=White guibg=DarkBlue guifg=White
hi PmenuSel ctermbg=DarkRed ctermfg=LightGreen gui=bold guibg=DarkRed guifg=LightGreen
hi PmenuSbar ctermbg=1 guibg=DarkBlue
hi PmenuThumb ctermfg=12 guifg=Red

hi link IncSearch		Visual
"hi link String			Constant
hi link Character		Constant
"hi link Number			Constant
hi Number			guifg=#60ff60 ctermfg=lightgreen
hi link Boolean			Constant
hi link Float			Number
hi link Function		Identifier
hi link Conditional		Statement
hi link Repeat			Statement
hi link Label			Statement
hi link Operator		Statement
hi link Keyword			Statement
hi link Exception		Statement
hi link Include			PreProc
hi link Define			PreProc
hi link Macro			PreProc
hi link PreCondit		PreProc
hi link StorageClass	Type
hi link Structure		Type
hi link Typedef			Type
hi link Tag				Special
hi link SpecialChar		Special
hi link Delimiter		Special
hi link SpecialComment	Special
hi link Debug			Special

hi IndentGuidesOdd ctermbg=darkblue guibg=#333333
hi IndentGuidesEven ctermbg=darkred guibg=#222222

