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




" Block elements  "{{{1

syntax match pukiwikiSectionMarker '^\~'

syntax match pukiwikiBlockquoteMarker '^>\{1,3}'
syntax match pukiwikiBlockquoteMarker '^<\{1,3}'

syntax match pukiwikiUnorderedListMarker '^+\{1,3}'
syntax match pukiwikiOrderedListMarker '^-\{1,3}'

syntax region pukiwikiDefinitionList start='^:\{1,3}' end='$' contains=@pukiwikiInlineElements,pukiwikiDefinitionListMarker transparent
syntax match pukiwikiDefinitionListMarker '^:' contained
syntax match pukiwikiDefinitionListMarker '|' contained

syntax match pukiwikiPreFormattedText '^\s.*'

syntax region pukiwikiTable start='^|' end='$' contains=@pukiwikiInlineElements,pukiwikiTableMarker transparent
syntax match pukiwikiTableMarker '|' contained

syntax region pukiwikiCSVTable start='^,' end='$' contains=@pukiwikiInlineElements,pukiwikiCSVTableMarker transparent
syntax match pukiwikiCSVTableMarker ',' contained

syntax region pukiwikiHeadline matchgroup=Special start='^\*\{1,3}' end='$' contains=@pukiwikiInlineElements oneline

syntax match pukiwikiBlockPlugin '^#\w\+' nextgroup=pukiwikiArguments,pukiwikiBlock

syntax match pukiwikiAlignMarker '^LEFT:'
syntax match pukiwikiAlignMarker '^CENTER:'
syntax match pukiwikiAlignMarker '^RIGHT:'

syntax match pukiwikiHorizon '^-\+$'




" Inline elements  "{{{1

syntax cluster pukiwikiInlineElements contains=pukiwikiLineBreak,pukiwikiBold,pukiwikiItalic,pukiwikiAnnotation,pukiwikiStrike,pukiwikiInlinePlugin,pukiwikiDecimalReference,pukiwikiHexReference,pukiwikiBracket,pukiwikiDoubleBracket

syntax match pukiwikiLineBreak '\~$'

syntax region pukiwikiBold start="''" end="''" oneline
syntax region pukiwikiItalic start="'''" end="'''" oneline
syntax region pukiwikiAnnotation start='((' end='))' oneline
syntax region pukiwikiStrike start='%%' end='%%' oneline

syntax match pukiwikiInlinePlugin '&\w\+' nextgroup=pukiwikiArguments,pukiwikiBlock
syntax match pukiwikiDecimalReference '&#\d\+'
syntax match pukiwikiHexReference '&#x\x\+'

syntax region pukiwikiBracket matchgroup=Special start='\[' end='\]' contains=pukiwikiAnchor oneline keepend
syntax region pukiwikiDoubleBracket matchgroup=Special start='\[\[' end='\]\]' contains=pukiwikiLink,pukiwikiAlias oneline keepend

syntax match pukiwikiPageName '.\+' contains=pukiwikiAnchor contained
syntax match pukiwikiAnchor '#[0-9A-Za-z_-]\+' contained

syntax match pukiwikiLink '[^:]\+:\?'he=e-1 nextgroup=pukiwikiPageName contained
syntax match pukiwikiAlias '[^>]\+>'he=e-1 nextgroup=pukiwikiAnchor,pukiwikiAliasWikiName,pukiwikiPageName contained
syntax match pukiwikiAliasWikiName '\(\u\l\+\)\+:'he=e-1 nextgroup=pukiwikiPageName contained




" Misc.   "{{{1

syntax region pukiwikiArguments matchgroup=Special start='(' end=')' nextgroup=pukiwikiBlock contained transparent
syntax region pukiwikiBlock matchgroup=Special start='{' end='}' contains=@pukiwikiInlineElements contained transparent
syntax region pukiwikiBlock matchgroup=Special start='{{' end='}}' contains=@pukiwikiInlineElements contained transparent
syntax region pukiwikiBlock matchgroup=Special start='{{{' end='}}}' contains=@pukiwikiInlineElements contained transparent

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
highlight default link pukiwikiBlockPlugin  Identifier
highlight default link pukiwikiAlignMarker  Statement
highlight default link pukiwikiHorizon PreProc

highlight default link pukiwikiLineBreak  Statement
highlight default link pukiwikiBold  Statement
highlight default link pukiwikiItalic  Statement
highlight default link pukiwikiAnnotation  Special
highlight default link pukiwikiStrike  NonText

highlight default link pukiwikiInlinePlugin  PreProc

highlight default link pukiwikiBracket  Special
highlight default link pukiwikiDoublebracket  Special
highlight default link pukiwikiLink  Underlined
highlight default link pukiwikiAlias  Underlined
highlight default link pukiwikiAliasWikiName  Type
highlight default link pukiwikiPageName  String
highlight default link pukiwikiAnchor  Type

highlight default link pukiwikiComment  Comment




" Fin.  "{{{1

let b:current_syntax = 'pukiwiki'




" __END__  "{{{1
" vim: foldmethod=marker
