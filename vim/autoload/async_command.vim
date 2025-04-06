let s:AsyncCommand = {}

function! async_command#new(command, handler) abort
  let self = copy(s:AsyncCommand)
  if has('nvim')
    let self._current_job = jobstart(a:command, {
    \   'on_exit': function(self._on_exit, [], self),
    \   'on_stdout': function(self._on_stdout, [], self),
    \ })
  else
    let self._current_job = job_start(a:command, {
    \   'exit_cb': self._exit_cb,
    \   'out_cb': self._out_cb,
    \ })
  endif
  let self._last_line = ''
  let self._handler = a:handler
  return self
endfunction

function! s:AsyncCommand.abort() abort dict
  if self._current_job isnot 0
    if has('nvim')
      call jobstop(self._current_job)
    else
      call job_stop(self._current_job)
    endif
    let self._current_job = 0
  endif
endfunction

function! s:AsyncCommand._on_exit(job, exit_code, event) abort dict
  let self._current_job = 0
  call self._handler.on_complete(self, a:exit_code)
endfunction

function! s:AsyncCommand._on_stdout(job, data, event) abort dict
  let line = self._last_line . a:data[0]

  if line != ''
    call self._handler.on_next(self, line)
  endif

  for line in a:data[1:-2]
    call self._handler.on_next(self, line)
  endfor

  let self._last_line = a:data[-1]
endfunction

function! s:AsyncCommand._exit_cb(job, status) abort dict
  let self._current_job = 0
  call self._handler.on_complete(self, a:status)
endfunction

function! s:AsyncCommand._out_cb(job, message) abort dict
  for line in split(a:message, "\n")
    call self._handler.on_next(self, line)
  endfor
endfunction
