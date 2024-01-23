let s:last_job = 0

function! git_complete#completefunc(findstart, base) abort
  if a:findstart
    return len(matchstr(getline('.'), '^\s*'))
  endif
  if s:last_job isnot 0
    call s:last_job.abort()
  endif
  let s:last_job = s:start_completion(a:base, col('.'))
  " Collects candidates asynchronously, so returns a dummy empty list.
  return []
endfunction

let s:Handler = {}

function! s:Handler.new(tokens, col) abort
  let handler = copy(s:Handler)
  let handler._col = a:col
  let handler._entries = {}
  let handler._tokens = a:tokens
  return handler
endfunction

function! s:Handler.on_next(job, line) abort dict
  let [filepath; rest] = split(a:line, ':')
  let content = s:trim(join(rest, ''))

  if has_key(self._entries, content)
    let entry = self._entries[content]
    let entry.count += 1
  else
    let [head, tail, score, position] = s:match_tokens(content, self._tokens)
    let self._entries[content] = {
    \   'count': 1,
    \   'filepath': filepath,
    \   'head': head,
    \   'position': position,
    \   'score': score,
    \   'tail': tail,
    \ }
  endif
endfunction

function! s:Handler.on_complete(job, status) abort dict
  if a:job isnot s:last_job
    return
  endif
  if a:status == 0
    let candidates = s:summarize_entries(values(self._entries))
    if mode() =~# 'i'
      call complete(self._col, candidates)
    endif
  endif
  let s:last_job = 0
endfunction

function! s:compare_entries(first, second) abort
  return a:first.score != a:second.score
  \      ? a:second.score - a:first.score
  \      : a:first.position != a:second.position
  \      ? a:first.position - a:second.position
  \      : a:first.count != a:second.count
  \      ? a:second.count - a:first.count
  \      : a:first.tail < a:second.tail
  \      ? -1
  \      : a:first.tail > a:second.tail
  \      ? 1
  \      : 0
endfunction

function! s:match_tokens(content, tokens) abort
  let head = ''
  let tail = ''
  let tokens_len = len(a:tokens)

  for i in range(tokens_len)
    let tail_tokens = join(a:tokens[i:], '')
    let position = stridx(a:content, tail_tokens)
    if position >= 0
      let head_tokens = i > 0 ? join(a:tokens[:(i - 1)], '') : ''
      let tail_content = a:content[position:]
      " If the position is not on a keyword boundary, the score will be 0.
      let score = position > 0 && a:content[position - 1] =~# '\k'
      \ ? 0
      \ : tokens_len - i
      return [head_tokens, tail_content, score, position]
    endif
  endfor

  return ['', '', 0, 0]
endfunction

function! s:start_completion(query, col) abort
  let query = s:trim(a:query)
  let tokens = split(query, '\<')

  if empty(tokens)
    return 0
  endif

  let command = ['git', 'grep', '--fixed-strings', '-H']

  for i in range(len(tokens))
    call add(command, '-e')
    call add(command, join(tokens[i:], ''))
  endfor

  let handler = s:Handler.new(tokens, col('.'))

  return async_job#new(command, handler)
endfunction

function! s:summarize_entries(entries) abort
  let candidates = []
  let max_width = winwidth(0) / 2
  for entry in sort(a:entries, 's:compare_entries')
    if entry.score > 0
      call add(candidates, {
      \   'abbr': entry.tail[:max_width],
      \   'menu': entry.filepath . ' (' . entry.count . ')',
      \   'word': entry.head . entry.tail,
      \ })
    endif
  endfor
  return candidates
endfunction

function! s:trim(str) abort
  return exists('*trim')
  \  ? trim(a:str)
  \  : substitute(a:str, '^\s\+\|\s\+$', '', '')
endfunction

if expand('%:p') ==# expand('<sfile>:p')
  setlocal completefunc=git_complete#completefunc
endif
