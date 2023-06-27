" Numbat.vim - Vim syntax file for Numbat language

if exists("b:current_syntax")
    finish
endif

" Numbat Keywords
syn keyword numbatKeywords per to let fn dimension unit long short both none print assert_eq
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
syn match numbatDigits "\d\+\.\d\+\|\d\+"
highlight default link numbatDigits Number

" Operators
syn match numbatOperators "->\|[+*^=/\-:·×÷²³<>]"
highlight default link numbatOperators Operator

" Unit decorators
syn match numbatDecorator "@\w\+"
highlight default link numbatDecorator Constant

" Special units
syn match numbatSpecialUnits "[\u00B0]" " degree symbol
highlight default link numbatSpecialUnits SpecialChar

" Load the matchit.vim script for the "%" matching, if it's available
if exists(":DoMatchIt")
  DoMatchIt
endif

let b:current_syntax = "numbat"
