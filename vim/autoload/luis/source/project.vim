function! luis#source#project#new(directory) abort
  let source = copy(s:Source)
  let source._directory = a:directory
  let source._cached_candidates = []
  return source
endfunction

function! s:action_open(kind, candidate) abort
  if has_key(a:candidate.user_data, 'project_path')
    let path = a:candidate.user_data.project_path
    let v:errmsg = ''
    tcd `=path`
    call luis#start(luis#source#file#new())
    return v:errmsg != '' ? v:errmsg : 0
  else
    return 'No such directory: ' . string(a:candidate.word)
  endif
endfunction

let s:Source = {
\   'name': 'project',
\   'default_kind': {
\     'name': 'project',
\     'action_table': {
\       'open': function('s:action_open'),
\     },
\     'key_table': {},
\     'prototype': g:luis#kind#common#export,
\   },
\   'matcher': g:luis#matcher#default,
\ }

function! s:Source.gather_candidates(args) abort dict
  return self._cached_candidates
endfunction

function! s:Source.on_source_enter() abort dict
  let candidates = []
  for path in globpath(self._directory, '*/', 1, 1)
    let path = path[-1:] == '/' ? path[:-2] : path
    call add(candidates, {
    \   'word': fnamemodify(path, ':t'),
    \   'user_data': {
    \     'project_path': path,
    \   },
    \ })
  endfor
  let self._cached_candidates = candidates
endfunction
