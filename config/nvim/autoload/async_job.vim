let s:AsyncJob = {}

function! async_job#new(command, handler) abort
  let job = copy(s:AsyncJob)
  if has('nvim')
    let job._current_job = jobstart(a:command, {
    \   'on_exit': function(job._on_exit, [], job),
    \   'on_stdout': function(job._on_stdout, [], job),
    \ })
  else
    let job._current_job = job_start(a:command, {
    \   'exit_cb': job._exit_cb,
    \   'out_cb': job._out_cb,
    \ })
  endif
  let job._last_line = ''
  let job._handler = a:handler
  return job
endfunction

function! s:AsyncJob.abort() abort dict
  if self._current_job isnot 0
    if has('nvim')
      call jobstop(self._current_job)
    else
      call job_stop(self._current_job)
    endif
    let self._current_job = 0
  endif
endfunction

function! s:AsyncJob._on_exit(job, exit_code, event) abort dict
  let self._current_job = 0
  call self._handler.on_complete(self, a:exit_code)
endfunction

function! s:AsyncJob._on_stdout(job, data, event) abort dict
  let line = self._last_line . a:data[0]

  if line != ''
    call self._handler.on_next(self, line)
  endif

  for line in a:data[1:-2]
    call self._handler.on_next(self, line)
  endfor

  let self._last_line = a:data[-1]
endfunction

function! s:AsyncJob._exit_cb(job, status) abort dict
  let self._current_job = 0
  call self._handler.on_complete(self, a:status)
endfunction

function! s:AsyncJob._out_cb(job, message) abort dict
  for line in split(a:message, "\n")
    call self._handler.on_next(self, line)
  endfor
endfunction
