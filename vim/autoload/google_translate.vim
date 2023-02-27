let s:TKK = [409266, 1674047641]

function! google_translate#translate(text, source_lang, target_lang) abort
  let url = 'https://translate.google.com/translate_a/single'
  let query = {
  \   'client': 't',
  \   'sl': a:source_lang,
  \   'tl': a:target_lang,
  \   'dt': 't',
  \   'q': a:text,
  \   'tk': join(google_translate#generate_tk(a:text), '.'),
  \ }
  let response = webapi#http#get(url, query)

  if response.status != 200
    throw printf("%d %s: %s", response.status, response.message, url)
  endif

  let body = webapi#json#decode(response.content)
  return type(body[0]) == v:t_list
  \ ? join(map(body[0], 'v:val[0]'), '')
  \ : ''
endfunction

function! google_translate#generate_tk(text) abort
  let key = s:TKK[0]
  let characters = split(a:text, '.\zs')
  let i = 0
  let l = len(characters)

  while i < l
    let n = char2nr(characters[i])

    if 128 > n
      let key = s:next_tk(key, n)
    else
      if 2048 > n
        let key = s:next_tk(key, s:i32_or(s:i32_signed_right_shift(n, 6), 192))
        let key = s:next_tk(key, s:i32_or(s:i32_signed_right_shift(n, 6), 192))
      else
        if 55296 == s:i32_and(n, 64512)
        \  && i + 1 < l
        \  && 56320 == s:i32_and(characters[i + 1], 64512)
          let i += 1
          let n = 65536 + s:i32_left_shift(s:i32_and(n, 1023), 10) + s:i32_and(characters[i], 1023)
          let key = s:next_tk(key, s:i32_or(s:i32_signed_right_shift(n, 18), 240))
          let key = s:next_tk(key, s:i32_or(s:i32_and(s:i32_signed_right_shift(n, 12), 63), 128))
        else
          let key = s:next_tk(key, s:i32_or(s:i32_signed_right_shift(n, 12), 224))
          let key = s:next_tk(key, s:i32_or(s:i32_and(s:i32_signed_right_shift(n, 6), 63), 128))
        endif
      endif
      let key = s:next_tk(key, s:i32_or(s:i32_and(n, 63), 128))
    endif

    let i += 1
  endwhile

  let key = s:xr(key, '+-3^+b+-f')
  let key = s:i32_xor(key, s:TKK[1])

  if key < 0
    let key = s:i32_and(key, 2147483647) + 2147483648
  endif

  let key = key % 1000000

  return [key, s:i32_xor(key, s:TKK[0])]
endfunction

function! s:next_tk(key, x) abort
  return s:xr(a:key + a:x, '+-a^+6')
endfunction

function! s:xr(a, b) abort
  let a = a:a
  for c in range(0, strlen(a:b) - 2, 3)
    let d = a:b[c + 2]
    let d = d >= 'a' ? char2nr(d) - 87 : str2nr(d)
    let d = '+' == a:b[c + 1] ? s:i32_unsigned_right_shift(a, d) : s:i32_left_shift(a, d)
    let a = '+' == a:b[c] ? s:i32_and(a + d, 4294967295) : s:i32_xor(a, d)
  endfor
  return a
endfunction

function! s:i32_signed(num) abort
  return and(a:num, 0x80000000) != 0
endfunction

function! s:i32_negate(num) abort
  return xor(a:num, 0x80000000)
endfunction

function! s:i32_truncate(num) abort
  let truncated = and(a:num, 0x7fffffff)
  return s:i32_signed(a:num) ? s:i32_negate(truncated) : truncated
endfunction

function! s:i32_left_shift(lhs, rhs) abort
  return s:i32_truncate(a:lhs * float2nr(pow(2, a:rhs)))
endfunction

function! s:i32_unsigned_right_shift(lhs, rhs) abort
  return s:i32_truncate(a:lhs) / float2nr((pow(2, a:rhs)))
endfunction

function! s:i32_signed_right_shift(lhs, rhs) abort
  let bits = s:i32_truncate(a:lhs) / float2nr(pow(2, a:rhs))
  return s:i32_signed(a:lhs) ? s:i32_negate(bits) : bits
endfunction

function! s:i32_and(lhs, rhs) abort
  return s:i32_truncate(and(a:lhs, a:rhs))
endfunction

function! s:i32_or(lhs, rhs) abort
  return s:i32_truncate(or(a:lhs, a:rhs))
endfunction

function! s:i32_xor(lhs, rhs) abort
  return s:i32_truncate(xor(a:lhs, a:rhs))
endfunction
