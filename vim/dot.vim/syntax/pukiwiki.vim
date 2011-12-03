" Vim syntax: pukiwiki
" Version: 0.0.0
" Copyright (C) 2011 emonkak <emonkak@gmail.com>
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
if exists('b:current_syntax')  "{{{1
  finish
endif

let b:current_syntax = 'pukiwiki'




" Block elements  "{{{1

syntax match pukiwikiSectionMarker '^\~'

syntax match pukiwikiBlockquoteMarker '^>\{1,3}'
syntax match pukiwikiBlockquoteMarker '^<\{1,3}'

syntax match pukiwikiUnorderedListMarker '^+\{1,3}'
syntax match pukiwikiOrderedListMarker '^-\{1,3}'

syntax region pukiwikiDefinitionList start='^:\{1,3}' end='$'
\ contains=pukiwikiDefinitionListMarker transparent
syntax match pukiwikiDefinitionListMarker '^:' contained
syntax match pukiwikiDefinitionListMarker '|' contained

syntax match pukiwikiPreFormattedText '^\s.*'

syntax region pukiwikiTable start='^|' end='$'
\ contains=pukiwikiTableMarker transparent
syntax match pukiwikiTableMarker '|' contained

syntax region pukiwikiCSVTable start='^,' end='$'
\ contains=pukiwikiCSVTableMarker transparent
syntax match pukiwikiCSVTableMarker ',' contained

syntax region pukiwikiHeadline matchgroup=Special
\ start='^\*\{1,3}' end='$' oneline

syntax match pukiwikiSharpe '^#' nextgroup=pukiwikiSharpeKeywords
syntax keyword pukiwikiSharpeKeywords nextgroup=pukiwikiArguments
\ contents hr br ref clear comment pcomment article vote
\ contained

syntax match pukiwikiAlignMarker '^LEFT:'
syntax match pukiwikiAlignMarker '^CENTER:'
syntax match pukiwikiAlignMarker '^RIGHT:'

syntax match pukiwikiHorizon '^-\+$'




" Inline elements  "{{{1

syntax match pukiwikiLineBreak '\~$'

syntax region pukiwikiBold start="''" end="''" oneline
syntax region pukiwikiItalic start="'''" end="'''" oneline
syntax region pukiwikiAnnotation start='((' end='))' oneline
syntax region pukiwikiStrike start='%%' end='%%' oneline

syntax match pukiwikiAnd '&' nextgroup=pukiwikiAndKeywords,pukiwikiAndNumber
syntax keyword pukiwikiAndKeywords nextgroup=pukiwikiArguments
\ br size color ref ruby aname counter online version t page fpage
\ date time now _date _time _now lastmod
\ heart smile bigsmile huh oh wink sad worried
\ contained
syntax match pukiwikiAndNumber '#\d\+' contained
syntax match pukiwikiAndNumber '#x\x\+' contained

syntax region pukiwikiDoubleBracket matchgroup=Special start='\[\[' end='\]\]'
\ contains=pukiwikiLink,pukiwikiAlias oneline keepend

syntax match pukiwikiPageName '.\+' contains=pukiwikiAnchor contained
syntax match pukiwikiAnchor '#[0-9A-Za-z_-]\+' contained

syntax match pukiwikiLink '[^:]\+:\?'he=e-1
\ nextgroup=pukiwikiPageName contained
syntax match pukiwikiAlias '[^>]\+>'he=e-1
\ nextgroup=pukiwikiAnchor,pukiwikiAliasWikiName,pukiwikiPageName contained
syntax match pukiwikiAliasWikiName '\(\u\l\+\)\+:'he=e-1
\ nextgroup=pukiwikiPageName contained




" Misc.   "{{{1

syntax region pukiwikiArguments nextgroup=pukiwikiBlock matchgroup=Special
\ start='(' end=')' contained
syntax region pukiwikiBlock matchgroup=Special start='{' end='}' contained

syntax match pukiwikiComment '^//.*'




" Highlight links  "{{{1

highlight default link pukiwikiSectionMarker  Statement

highlight default link pukiwikiBlockquoteMarker  Comment

highlight default link pukiwikiUnorderedListMarker  Statement
highlight default link pukiwikiOrderedListMarker  Statement
highlight default link pukiwikiDefinitionListMarker  Statement

highlight default link pukiwikiPreFormattedText  String

highlight default link pukiwikiTableMarker  Statement
highlight default link pukiwikiCSVTableMarker  Statement

highlight default link pukiwikiHeadline  Title

highlight default link pukiwikiSharpe  Identifier
highlight default link pukiwikiSharpeKeywords  Identifier

highlight default link pukiwikiAlignMarker  Statement

highlight default link pukiwikiHorizon PreProc


highlight default link pukiwikiLineBreak  Statement
highlight default link pukiwikiBold  Statement
highlight default link pukiwikiItalic  Statement
highlight default link pukiwikiAnnotation  Special
highlight default link pukiwikiStrike  NonText

highlight default link pukiwikiAnd  PreProc
highlight default link pukiwikiAndKeywords  PreProc
highlight default link pukiwikiAndNumber  PreProc

highlight default link pukiwikiDoublebracket  Special
highlight default link pukiwikiLink  Underlined
highlight default link pukiwikiAlias  Type
highlight default link pukiwikiAliasWikiName  Underlined
highlight default link pukiwikiPageName  String
highlight default link pukiWikiAnchor  Type


highlight default link pukiwikiComment  Comment




" __END__  "{{{1
" vim: foldmethod=marker
