function! luis#source#project#new(directory) abort
  let source = copy(s:Source)
  let source.directory = a:directory
  let source.cached_candidates = []
  return source
endfunction

function! s:action_open(candidate, context) abort
  if !has_key(a:candidate.user_data, 'project_path')
    return 'No project chosen'
  endif
  let path = a:candidate.user_data.project_path
  try
    tcd `=path`
    let source = luis#source#file#new()
    let session = luis#new_session(source)
    call luis#start(session)
  catch
    return v:exception
  endtry
  return 0
endfunction

let s:Source = {
\   'name': 'project',
\   'default_kind': {
\     'name': 'project',
\     'action_table': {
\       'open': function('s:action_open'),
\     },
\     'key_table': {},
\     'prototype': g:luis#kind#common#import(),
\   },
\ }

function! s:Source.gather_candidates(context) abort dict
  return self.cached_candidates
endfunction

function! s:Source.on_source_enter(context) abort dict
  let candidates = []
  for path in globpath(self.directory, '*/', 1, 1)
    let path = path[-1:] == '/' ? path[:-2] : path
    call add(candidates, {
    \   'word': fnamemodify(path, ':t'),
    \   'user_data': {
    \     'project_path': path,
    \   },
    \ })
  endfor
  let self.cached_candidates = candidates
endfunction
