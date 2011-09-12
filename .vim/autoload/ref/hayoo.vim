" ref source: hayoo
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
" Variables  "{{{1

let s:source = {'name': 'hayoo'}




" Interface  "{{{1
function! ref#hayoo#define()  " {{{2
  return copy(s:source)
endfunction




function! s:source.available()  " {{{2
  return executable('curl')
endfunction




function! s:source.complete(query)  " {{{2
  return []
endfunction




function! s:source.get_body(query)  " {{{2
  let _ = s:hayoo(a:query).functions
  call map(_, 'v:val.module . " " . v:val.name . " :: " . v:val.signature')
  return {'body': join(_, "\n")}
endfunction




function! s:source.opened(query)  " {{{2
  runtime syntax/ref-hoogle.vim
endfunction




" Misc.  "{{{1
function! s:hayoo(query)  "{{{2
  let api = 'http://holumbus.fh-wedel.de/hayoo/hayoo.json?query='
  let query = substitute(a:query,
  \                      '[^0-9A-za-z_.~/-]',
  \                      '\=printf("%%%x", char2nr(submatch(0)))',
  \                      'g')
  let response = ref#system(['curl', '-s', api . query])
  return eval(response.stdout)
endfunction




" __END__  "{{{1
" vim: foldmethod=marker
