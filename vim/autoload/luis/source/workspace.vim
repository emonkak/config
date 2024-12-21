function! luis#source#workspace#new(path, callback) abort
  let source = copy(s:Source)
  let source._path = a:path
  let source._callback = a:callback
  let source._cached_candidates = []
  return source
endfunction

function! s:action_open(candidate, context) abort
  if !has_key(a:candidate.user_data, 'project_path')
    return 'No workspace chosen'
  endif
  let Callback = a:context.session.source._callback
  let path = a:candidate.user_data.project_path
  try
    call Callback(path)
  catch
    return v:exception
  endtry
  return 0
endfunction

let s:Source = {
\   'name': 'workspace',
\   'default_kind': {
\     'name': 'workspace',
\     'action_table': {
\       'open': function('s:action_open'),
\     },
\     'key_table': {},
\     'prototype': g:luis#kind#common#import(),
\   },
\ }

function! s:Source.gather_candidates(context) abort dict
  return self._cached_candidates
endfunction

function! s:Source.on_source_enter(context) abort dict
  let candidates = []
  for path in globpath(self._path, '*', 0, 1)
    if !isdirectory(path)
      break
    endif
    let dir_name = fnamemodify(path, ':t')
    call add(candidates, {
    \   'word': dir_name,
    \   'user_data': {
    \     'preview_function': function('s:preview_candidate'),
    \     'preview_title': dir_name,
    \     'project_path': path,
    \   },
    \ })
  endfor
  let self._cached_candidates = candidates
endfunction

function! s:preview_candidate(candidate) abort
  return map(
  \   globpath(a:candidate.user_data.project_path, '*', 0, 1),
  \   'fnamemodify(v:val, ":t")'
  \ )
endfunction
