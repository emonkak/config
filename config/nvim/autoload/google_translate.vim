function! google_translate#translate(from, to, text) abort
  let url = 'https://translate.google.com/translate_a/single'
  let query = {
  \   'client': 'gtx',
  \   'dt': 't',
  \   'q': a:text,
  \   'sl': a:from,
  \   'tk': join(google_translate#generate_tk(a:text, [0, 0]), '.'),
  \   'tl': a:to,
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

function! google_translate#generate_tk(text, tkk) abort
  let tk = a:tkk[0]
  let i = 0
  let l = strlen(a:text)
  while i < l
    let n = char2nr(a:text[i])
    if n < 128
      let tk = s:trnasform1(tk + n)
    elseif n < 2048
      let tk = s:trnasform1(tk + s:or(s:sar(n, 6), 192))
    elseif s:and(n, 64512) == 55296 &&
    \      i + 1 < l && s:and(char2nr(a:text[i + 1]), 64512) == 56320
      let i += 1
      let n = 65536 + s:shl(s:and(n, 1023), 10)
      \     + s:and(char2nr(a:text[i]), 1023)
      let tk = s:trnasform1(tk + s:or(s:sar(n, 18), 240))
      let tk = s:trnasform1(tk + s:or(s:and(s:sar(n, 12), 63), 128))
    else
      let tk = s:trnasform1(tk + s:or(s:sar(n, 12), 224))
      let tk = s:trnasform1(tk + s:or(s:and(s:sar(n, 6), 63), 128))
      let tk = s:trnasform1(tk + s:or(s:and(n, 63), 128))
    endif
    let i += 1
  endwhile
  let tk = s:trnasform2(tk)
  let tk = s:xor(tk, a:tkk[1])
  if tk < 0
    let tk = s:and(tk, 2147483647) + 2147483648
  endif
  let tk = tk % 1000000
  return [tk, s:xor(tk, a:tkk[0])]
endfunction

function! s:and(lhs, rhs) abort
  return s:truncate(and(a:lhs, a:rhs))
endfunction

function! s:negate(n) abort
  return xor(a:n, 0x80000000)
endfunction

function! s:or(lhs, rhs) abort
  return s:truncate(or(a:lhs, a:rhs))
endfunction

function! s:sar(lhs, rhs) abort
  let result = s:truncate(a:lhs) / float2nr(pow(2, a:rhs))
  return s:signed(a:lhs) ? s:negate(result) : result
endfunction

function! s:shl(lhs, rhs) abort
  return s:truncate(a:lhs * float2nr(pow(2, a:rhs)))
endfunction

function! s:shr(lhs, rhs) abort
  return s:truncate(a:lhs) / float2nr((pow(2, a:rhs)))
endfunction

function! s:signed(n) abort
  return and(a:n, 0x80000000) != 0
endfunction

function! s:transform(tk, ops) abort
  let tk = a:tk
  for i in range(0, strlen(a:ops) - 2, 3)
    let n = a:ops[i + 2]
    let n = n >=# 'a' ? char2nr(n) - 87 : str2nr(n)
    let n = a:ops[i + 1] == '+' ? s:shr(tk, n) : s:shl(tk, n)
    let tk = a:ops[i] == '+' ? s:and(tk + n, 4294967295) : s:xor(tk, n)
  endfor
  return tk
endfunction

function! s:trnasform1(tk) abort
  return s:transform(a:tk, '+-a^+6')
endfunction

function! s:trnasform2(tk) abort
  return s:transform(a:tk, '+-3^+b+-f')
endfunction

function! s:truncate(n) abort
  let result = and(a:n, 0x7fffffff)
  return s:signed(a:n) ? s:negate(result) : result
endfunction

function! s:xor(lhs, rhs) abort
  return s:truncate(xor(a:lhs, a:rhs))
endfunction
