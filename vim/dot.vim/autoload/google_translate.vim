" google_translate - {abstract}
" Version: 0.0.0
" Copyright (C) 2016 emonkak <emonkak@gmail.com>
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

let g:google_transale_tkk = get(g:, 'g:google_transale_tkk', '409266.1674047641')




" Interface  "{{{1
function! google_translate#translate(text, from, to) abort  "{{{2
  let url = 'https://translate.google.com/translate_a/single'
  let options = {
  \   'client': 't',
  \   'sl': a:from,
  \   'tl': a:to,
  \   'dt': 't',
  \   'q': a:text,
  \   'tk': google_translate#token(a:text),
  \ }
  let response = webapi#http#get(url, options)

  if response.status != 200
    throw printf("%d %s: %s", response.status, response.message, url)
  endif

  let result = webapi#json#decode(response.content)
  return type(result[0]) == type([])
  \ ? join(map(result[0], 'v:val[0]'), '')
  \ : ''
endfunction




function! google_translate#token(text) abort  "{{{2
  let a = split(a:text, '.\zs')
  let b = g:google_transale_tkk
  let c = split(b, '\.')
  let d = str2nr(c[0])
  let e = []
  let f = 0

  while f < len(a)
    let g = char2nr(a[f])
    if 128 > g
      call add(e, g)
    else
      if 2048 > g
        call add(e, s:i32_or(s:i32_signed_right_shift(g, 6), 192))
        call add(e, s:i32_or(s:i32_signed_right_shift(g, 6), 192))
      else
        if 55296 == s:i32_and(g, 64512)
        \  && f + 1 < len(a)
        \  && 56320 == s:i32_and(a[f + 1], 64512)
          let f += 1
          let g = 65536 + s:i32_left_shift(s:i32_and(g, 1023), 10) + s:i32_and(a[f], 1023)
          call add(e, s:i32_or(s:i32_signed_right_shift(g, 18), 240))
          call add(e, s:i32_or(s:i32_and(s:i32_signed_right_shift(g, 12), 63), 128))
        else
          call add(e, s:i32_or(s:i32_signed_right_shift(g, 12), 224))
          call add(e, s:i32_or(s:i32_and(s:i32_signed_right_shift(g, 6), 63), 128))
        endif
      endif
      call add(e, s:i32_or(s:i32_and(g, 63), 128))
    endif
    let f += 1
  endwhile

  let a = d
  for ff in e
    let a = s:xr(a + ff, '+-a^+6')
  endfor

  let a = s:xr(a, '+-3^+b+-f')
  let a = s:i32_xor(a, str2nr(c[1]))
  if a < 0
    let a = s:i32_and(a, 2147483647) + 2147483648
  endif
  let a = a % 1000000
  return a . '.' . s:i32_xor(a, d)
endfunction




" Misc.  "{{{1
function! s:i32_negative(num) abort  "{{{2
  return and(a:num, 0x80000000) != 0
endfunction




function! s:i32_truncate(num) abort  "{{{2
  let truncated = and(a:num, 0x7fffffff)
  return s:i32_negative(a:num) ? or(truncated, 0x80000000) : truncated
endfunction




function! s:i32_left_shift(lhs, rhs) abort  "{{{2
  return s:i32_truncate(a:lhs << a:rhs)
endfunction




function! s:i32_unsigned_right_shift(lhs, rhs) abort  "{{{2
  return s:i32_truncate(a:lhs) >> a:rhs
endfunction



function! s:i32_signed_right_shift(lhs, rhs) abort  "{{{2
  let bits = s:i32_truncate(a:lhs) >> a:rhs
  if s:i32_negative(a:lhs)
    let bits = or(bits, 0x80000000)
  endif
  return bits
endfunction



function! s:i32_and(lhs, rhs) abort  "{{{2
  return s:i32_truncate(and(a:lhs, a:rhs))
endfunction




function! s:i32_or(lhs, rhs)  abort "{{{2
  return s:i32_truncate(or(a:lhs, a:rhs))
endfunction




function! s:i32_xor(lhs, rhs)  abort "{{{2
  return s:i32_truncate(xor(a:lhs, a:rhs))
endfunction




function! s:xr(a, b)  abort "{{{2
  let a = a:a
  for c in range(0, strlen(a:b) - 2, 3)
    let d = a:b[c + 2]
    let d = d >= 'a' ? char2nr(d) - 87 : str2nr(d)
    let d = '+' == a:b[c + 1] ? s:i32_unsigned_right_shift(a, d) : s:i32_left_shift(a, d)
    let a = '+' == a:b[c] ? s:i32_and(a + d, 4294967295) : s:i32_xor(a, d)
  endfor
  return a
endfunction




" __END__  "{{{1
" vim: foldmethod=marker
