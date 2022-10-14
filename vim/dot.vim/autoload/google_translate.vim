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
function! google_translate#translate(text, from, to)  "{{{2
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




function! google_translate#token(text)  "{{{2
  let a = split(a:text, '.\zs')
  let b = g:google_transale_tkk

  let cD = split(b, '\.')
  let iB = str2nr(cD[0])

  let e = []
  let g = 0

  while g < len(a)
    let l = char2nr(a[g])
    if 128 > l
      call add(e, l)
    else
      if 2048 > l
        call add(e, s:or32(s:signed_right_shift32(l, 6), 192))
        call add(e, s:or32(s:signed_right_shift32(l, 6), 192))
      else
        if 55296 == s:and32(l, 64512)
        \  && g + 1 < len(a)
        \  && 56320 == s:and32(a[g + 1], 64512)
          let g += 1
          let l = 65536 + s:left_shift32(s:and32(l, 1023), 10) + s:and32(a[g], 1023)
          call add(e, s:or32(s:signed_right_shift32(l, 18), 240))
          call add(e, s:or32(s:and32(s:signed_right_shift32(l, 12), 63), 128))
        else
          call add(e, s:or32(s:signed_right_shift32(l, 12), 224))
          call add(e, s:or32(s:and32(s:signed_right_shift32(l, 6), 63), 128))
        endif
      endif
      call add(e, s:or32(s:and32(l, 63), 128))
    endif
    let g += 1
  endwhile

  let a = iB
  for ff in e
    let a = s:xr(a + ff, '+-a^+6')
  endfor

  let a = s:xr(a, '+-3^+b+-f')
  let a = s:xor32(a, str2nr(cD[1]))
  if a < 0
    let a = s:and32(a, 2147483647) + 2147483648
  endif
  let a = a % 1000000
  return a . '.' . s:xor32(a, iB)
endfunction




" Misc.  "{{{1
function! s:is_negative(n)  "{{{2
  return and(a:n, 2147483648) != 0
endfunction




function! s:int32(n)  "{{{2
  return s:is_negative(a:n) ? or(a:n, -2147483648) : and(a:n, 2147483647)
endfunction




function! s:left_shift32(n, i)  "{{{2
  let bs = (s:is_negative(a:n)) . printf('%032b', a:n)[-31:]
  let bs = bs[a:i:] . repeat('0', a:i)
  return s:int32(str2nr(bs, 2))
endfunction




function! s:right_shift32(n, i)  "{{{2
  let bs = (s:is_negative(a:n)) . printf('%032b', a:n)[-31:]
  let bs = repeat('0', a:i) . bs[:-(a:i + 1)]
  return s:int32(str2nr(bs, 2))
endfunction



function! s:signed_right_shift32(n, i)  "{{{2
  let bs = (s:is_negative(a:n)) . printf('%032b', a:n)[-31:]
  let bs = (s:is_negative(a:n)) . repeat('0', a:i - 1) . bs[:-(a:i + 1)]
  return s:int32(str2nr(bs, 2))
endfunction



function! s:and32(x, y)  "{{{2
  return s:int32(and(a:x, a:y))
endfunction




function! s:or32(x, y)  "{{{2
  return s:int32(or(a:x, a:y))
endfunction




function! s:xor32(x, y)  "{{{2
  return s:int32(xor(a:x, a:y))
endfunction




function! s:xr(a, b)  "{{{2
  let a = a:a
  for c in range(0, strlen(a:b) - 2, 3)
    let d = a:b[c + 2]
    let d = d >= 'a' ? char2nr(d) - 87 : str2nr(d)
    let d = '+' == a:b[c + 1] ? s:right_shift32(a, d) : s:left_shift32(a, d)
    let a = '+' == a:b[c] ? s:and32(a + d, 4294967295) : s:xor32(a, d)
  endfor
  return a
endfunction




" __END__  "{{{1
" vim: foldmethod=marker
