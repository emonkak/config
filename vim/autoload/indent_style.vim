function! indent_style#auto_configure() abort
  let bufnr = bufnr()
  let options = indent_style#infer_options()
  let undo_options = []

  for [key, value] in items(options)
    if value != getbufvar(bufnr, '&' . key)
      call setbufvar(bufnr, '&' . key, value)
      call add(undo_options, key . '<')
    endif
  endfor

  return !empty(undo_options) ? 'setlocal ' . join(undo_options, ' ') : ''
endfunction

function! indent_style#infer_options() abort
  " https://github.com/timakro/vim-yadi/blob/main/plugin/yadi.vim
  let tabbed_lines = 0
  let spaced_lines = 0
  let indent_widths = {}
  let last_space_width = 0

  let i = 1
  let l = line('$')
  let CHUNKS = 100

  while i < l
    let lines = getline(i, i + CHUNKS - 1)
    for line in lines
      if line[0] == "\t"
        let tabbed_lines += 1
      else
        " The position of the first non-space character is the indentation
        " width.
        let space_width = match(line, '[^ ]')
        if space_width != -1
          if space_width > 0
            let spaced_lines += 1
          endif
          let indent_width = space_width - last_space_width
          if indent_width >= 2  " minimum indentation is 2 spaces
            let indent_widths[indent_width] =
            \   get(indent_widths, indent_width, 0) + 1
          endif
          let last_space_width = space_width
        endif
      endif
    endfor
    if tabbed_lines > 0 || spaced_lines > 0
      break
    endif
    let i += CHUNKS
  endwhile

  let total_identions = 0
  let max_occurrences = 0
  let inferred_indent_width = -1

  for [indent_width, occurrences] in items(indent_widths)
    let total_identions += occurrences
    if occurrences > max_occurrences
      let max_occurrences = occurrences
      let inferred_indent_width = indent_width
    endif
  endfor

  if tabbed_lines > spaced_lines * 4  " Over 80% tabs
    return {
    \  'expandtab': 0,
    \  'shiftwidth': 0,
    \  'softtabstop': 0,
    \ }
  elseif spaced_lines > tabbed_lines * 4
  \      && max_occurrences * 5 > total_identions * 3
    " Detected over 80% spaces and the most common indentation level makes
    " up over 60% of all indentations in the file.
    return {
    \  'expandtab': 1,
    \  'shiftwidth': inferred_indent_width,
    \  'softtabstop': inferred_indent_width,
    \ }
  else
    return {}
  endif
endfunction
