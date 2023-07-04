function! s:eps() abort
  let i = 0
  while 1.0 + pow(2, -i) != 1.0
    let i += 1
  endwhile
  return pow(2, -i / 2)
endfunction
let s:EPS = s:eps()

function! luis#source#lsp_symbol#new() abort
  let source = copy(s:Source)
  let source.cached_candidates = []
  let source.sequence = 0
  return source
endfunction

let s:Source = {
\   'name': 'lsp_symbol',
\   'default_kind': g:luis#kind#file#import(),
\ }

function! s:Source.gather_candidates(context) abort dict
  return self.cached_candidates
endfunction

function! s:Source.on_source_enter(context) abort dict
  let bufnr = winbufnr(winnr('#'))
  let self.cached_candidates = []
  let self.sequence += 1
  for server in s:available_servers(bufnr)
    call lsp#send_request(server, {
    \   'method': 'textDocument/documentSymbol',
    \   'params': {
    \     'textDocument': lsp#get_text_document_identifier(bufnr),
    \   },
    \   'on_notification': function(s:Source._on_notification,
    \                               [server, self.sequence, a:context.session],
    \                               self),
    \ })
  endfor
endfunction

function! s:Source._on_notification(server, sequence, session, data) abort dict
  if !has_key(a:data.response, 'result') || self.sequence != a:sequence
    return
  endif

  let symbols = type(a:data.response.result) is v:t_dict
  \             ? [a:data.response.result]
  \             : a:data.response.result
  if empty(symbols)  " some servers also return null
    return
  endif

  let queue = []
  let depth = 0
  let has_changed = 0

  while 1
    for symbol in symbols
      if lsp#utils#is_file_uri(symbol.location.uri)
        let candidate = s:candidate_from_symbol(a:server, symbol, depth)
        call add(self.cached_candidates, candidate)
        let has_changed = 1
      endif
      if has_key(symbol, 'children') && !empty(symbol.children)
        call add(queue, [depth + 1, symbol.children])
      endif
    endfor
    if empty(queue)
      break
    endif
    let [depth, symbols] = remove(queue, 0)
  endwhile

  if has_changed
    call a:session.reload_candidates()
  endif
endfunction

function! s:available_servers(bufnr) abort
  return filter(lsp#get_allowed_servers(a:bufnr),
  \             'lsp#capabilities#has_document_symbol_provider(v:val)')
endfunction

function! s:candidate_from_symbol(server, symbol, depth) abort
  let location = a:symbol.location
  let path = lsp#utils#uri_to_path(location.uri)
  let pos = lsp#utils#position#lsp_to_vim(path, location.range.start)
  let indent = repeat('  ', a:depth)
  let kind = lsp#ui#vim#utils#_get_symbol_text_from_kind(
  \   a:server,
  \   a:symbol.kind
  \ )
  return {
  \   'word': a:symbol.name,
  \   'abbr': indent . a:symbol.name,
  \   'menu': kind,
  \   'user_data': {
  \     'file_path': path,
  \     'file_pos': pos,
  \   },
  \   'luis_sort_priority': s:sort_priority_from_pos(pos),
  \ }
endfunction

function! s:sort_priority_from_pos(pos) abort
  let lnum = a:pos[0]
  let col = a:pos[1] * s:EPS
  let max_col  = 1.0 - s:EPS
  if col > max_col
    let col = max_col
  endif
  return lnum + col
endfunction
