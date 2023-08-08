let s:TKK = [409266, 1674047641]

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
        let key = s:next_tk(key, s:or_32(s:signed_right_shift_32(n, 6), 192))
        let key = s:next_tk(key, s:or_32(s:signed_right_shift_32(n, 6), 192))
      else
        if 55296 == s:and_32(n, 64512)
        \  && i + 1 < l
        \  && 56320 == s:and_32(characters[i + 1], 64512)
          let i += 1
          let n = 65536 + s:left_shift_32(s:and_32(n, 1023), 10) + s:and_32(characters[i], 1023)
          let key = s:next_tk(key, s:or_32(s:signed_right_shift_32(n, 18), 240))
          let key = s:next_tk(key, s:or_32(s:and_32(s:signed_right_shift_32(n, 12), 63), 128))
        else
          let key = s:next_tk(key, s:or_32(s:signed_right_shift_32(n, 12), 224))
          let key = s:next_tk(key, s:or_32(s:and_32(s:signed_right_shift_32(n, 6), 63), 128))
        endif
      endif
      let key = s:next_tk(key, s:or_32(s:and_32(n, 63), 128))
    endif

    let i += 1
  endwhile

  let key = s:xr(key, '+-3^+b+-f')
  let key = s:xor_32(key, s:TKK[1])
  if key < 0
    let key = s:and_32(key, 2147483647) + 2147483648
  endif
  let key = key % 1000000

  return [key, s:xor_32(key, s:TKK[0])]
endfunction

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
    echoerr printf("%d %s: %s", response.status, response.message, url)
    return ''
  endif

  let body = webapi#json#decode(response.content)
  return type(body[0]) == v:t_list
  \ ? join(map(body[0], 'v:val[0]'), '')
  \ : ''
endfunction

function! s:next_tk(key, x) abort
  return s:xr(a:key + a:x, '+-a^+6')
endfunction

function! s:xr(a, b) abort
  let a = a:a
  for c in range(0, strlen(a:b) - 2, 3)
    let d = a:b[c + 2]
    let d = d >= 'a' ? char2nr(d) - 87 : str2nr(d)
    let d = '+' == a:b[c + 1] ? s:unsigned_right_shift_32(a, d) : s:left_shift_32(a, d)
    let a = '+' == a:b[c] ? s:and_32(a + d, 4294967295) : s:xor_32(a, d)
  endfor
  return a
endfunction

function! s:and_32(lhs, rhs) abort
  return s:truncate_32(and(a:lhs, a:rhs))
endfunction

function! s:left_shift_32(lhs, rhs) abort
  return s:truncate_32(a:lhs * float2nr(pow(2, a:rhs)))
endfunction

function! s:negate_32(n) abort
  return xor(a:n, 0x80000000)
endfunction

function! s:or_32(lhs, rhs) abort
  return s:truncate_32(or(a:lhs, a:rhs))
endfunction

function! s:signed_32(n) abort
  return and(a:n, 0x80000000) != 0
endfunction

function! s:signed_right_shift_32(lhs, rhs) abort
  let bits = s:truncate_32(a:lhs) / float2nr(pow(2, a:rhs))
  return s:signed_32(a:lhs) ? s:negate_32(bits) : bits
endfunction

function! s:truncate_32(n) abort
  let truncated = and(a:n, 0x7fffffff)
  return s:signed_32(a:n) ? s:negate_32(truncated) : truncated
endfunction

function! s:unsigned_right_shift_32(lhs, rhs) abort
  return s:truncate_32(a:lhs) / float2nr((pow(2, a:rhs)))
endfunction

function! s:xor_32(lhs, rhs) abort
  return s:truncate_32(xor(a:lhs, a:rhs))
endfunction
