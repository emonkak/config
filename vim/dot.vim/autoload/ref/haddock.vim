" ref source: haddock
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
" Variables  "{{{1

let s:source = {'name': 'haddock'}




" Interface  "{{{1
function! ref#haddock#define()  " {{{2
  return copy(s:source)
endfunction




function! s:source.available()  " {{{2
  return executable('ghc-mod') && executable('ghc-pkg') && executable('elinks')
endfunction




function! s:source.complete(query)  " {{{2
  return filter(split(system('ghc-mod list'), '\n'),
  \             'a:query ==# strpart(v:val, 0, len(a:query))')
endfunction




function! s:source.get_body(query)  " {{{2
  let package = ref#system(['ghc-pkg',
  \                         'find-module',
  \                         '--simple-output',
  \                         a:query]).stdout[:-2]
  let haddock_html = ref#system(['ghc-pkg',
  \                              'field',
  \                              package,
  \                              'haddock-html']).stdout[:-2]
  let haddock_html = matchstr(haddock_html, 'haddock-html:\s\zs.\+')
  let haddock_html .= '/'
  let haddock_html .= join(split(a:query, '\.'), '-') . '.html'
  let contents = map(readfile(haddock_html),
  \                  'substitute(v:val,
  \                              "<a\\s[^>]*class=\"link\">Source<\\/a>",
  \                              "",
  \                              "")')
  let body = ref#system(['elinks',
  \                      '-dump',
  \                      '-no-home',
  \                      '-no-numbering',
  \                      '-no-references'], join(contents, "\n")).stdout[:-2]
  return {'body': body}
endfunction




function! s:source.opened(query)  " {{{2
  runtime syntax/ref-haddock.vim
endfunction




" __END__  "{{{1
" vim: foldmethod=marker
