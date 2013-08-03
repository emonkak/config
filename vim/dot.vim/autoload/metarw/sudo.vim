" metarw scheme: sudo
" Version: 0.0.0
" Copyright (C) 2010-2011 emonkak <emonkak@gmail.com>
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
" Interface  "{{{1
function! metarw#sudo#complete(arglead, cmdline, cursorpos)  "{{{2
  let [scheme, path] = split(a:arglead, ':', !0)
  let [path_head; path_tail] = split(path, '/', !0)

  let candidates = map(split(glob(path . '*'), "\n"),
  \                    'scheme . ":" . v:val . (isdirectory(v:val) ? "/" : "")')
  let head = printf('%s:%s/', scheme, path_head)
  let tail = join(path_tail, '/')

  return [candidates, head, tail]
endfunction




function! metarw#sudo#read(fakepath)  "{{{2
  let fragments = split(a:fakepath, ':', !0)
  let scheme = fragments[0]
  let path = fnamemodify(fragments[1], ':p')
  if isdirectory(path)
    let result = []
    for wildcard in ['.?*', '*']
      for name in split(globpath(path, wildcard), "\n")
        call add(result, {
        \   'label': fnamemodify(name, ':t') . (isdirectory(name) ? '/' : ''),
        \   'fakepath': scheme . ':' . fnamemodify(simplify(name), ':p'),
        \ })
      endfor
    endfor
    return ['browse', result]
  else
    return ['read', '!sudo cat ' . shellescape(path)]
  endif
endfunction




function! metarw#sudo#write(fakepath, line1, line2, append_p)  "{{{2
  let path = split(a:fakepath, ':', !0)[1]
  let original_autoread = &l:autoread
  setlocal noautoread
  try
    let result = ['write', '!sudo tee >/dev/null ' . shellescape(path)]
    setlocal nomodified
  finally
    let &l:autoread = original_autoread
  endtry
  return result
endfunction




" __END__  "{{{1
" vim: foldmethod=marker
