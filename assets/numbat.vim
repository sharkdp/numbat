" Numbat.vim - Vim syntax file for Numbat language

if exists("b:current_syntax")
    finish
endif

" Numbat Keywords
syn keyword numbatKeywords per to let fn where and dimension unit use struct long short both none if then else true false NaN inf print assert assert_eq type
highlight default link numbatKeywords Keyword

" Physical dimensions (every capitalized word)
syn match numbatDimensions "\<\u\w*\>"
highlight default link numbatDimensions Type

" Variables and units (lowercase words)
" syn match numbatIdentifier "\<\l\w*\>"
" highlight default link numbatIdentifier Identifier

" Comments
syn match numbatComments "#.*"
highlight default link numbatComments Comment

" Digits/Numbers
syn match numbatNumber '\v<0x[0-9a-fA-F]+>'
syn match numbatNumber '\v<0o[0-7]+>'
syn match numbatNumber '\v<0b[01]+>'
syn match numbatNumber '\v<[0-9]+(_[0-9]+)*([eE][+-]?[0-9]+)?>'
syn match numbatNumber '\v<([0-9]*\.[0-9]*|[0-9]*\.[0-9]+)([eE][+-]?[0-9]+)?>'
highlight default link numbatNumber Number

" Operators
syn match numbatOperators "->\|[+*^=/\-:·⋅×÷²³<>]"
highlight default link numbatOperators Operator

" Unit decorators
syn match numbatDecorator "@\w\+"
highlight default link numbatDecorator Constant

" Special units
syn match numbatSpecialUnits "[\u00B0]" " degree symbol
highlight default link numbatSpecialUnits SpecialChar

" Strings
syntax match numbatString /"\([^"]*\)"/
highlight link numbatString String

" Load the matchit.vim script for the "%" matching, if it's available
if exists(":DoMatchIt")
  DoMatchIt
endif

setlocal comments=b:#,fb:-
setlocal commentstring=#\ %s

let b:current_syntax = "numbat"
