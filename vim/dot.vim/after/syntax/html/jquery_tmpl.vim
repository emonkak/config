" Vim additional syntax: html
" Version: 0.0.0
" Copyright (C) 2012 emonkak <emonkak@gmail.com>
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

" jQuery template
syntax keyword jQueryTmplKeyword contained html if else each tmpl wrap
syntax region jQueryTmplRegion matchgroup=Delimiter start="{{/\?" end="}}"
\ containedin=jQueryTmplTag,javaScriptStringS,javaScriptStringD,htmlString
\ contains=jQueryTmplKeyword,@htmlJavaScript
\ contained
\ keepend
syntax region jQueryTmplVariable matchgroup=Delimiter start="\${=\?" end="}"
\ containedin=jQueryTmplTag,javaScriptStringS,javaScriptStringD,htmlString
\ contains=@htmlJavaScript
\ contained
\ keepend
syntax region jQueryTmplTag start=+<script\s[^>]*type\s*=["']text/x-jquery-tmpl["'][^>]*>+ end=+</script>+me=s-1
\ contains=TOP,javaScript
\ keepend

highlight default link jQueryTmplKeyword Keyword

" __END__
" vim: foldmethod=marker
