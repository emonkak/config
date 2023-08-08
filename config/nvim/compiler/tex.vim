if exists('g:current_compiler')
  finish
endif

CompilerSet makeprg=platex
\\ -kanji=utf8
\\ -interaction=nonstopmode
\\ -file-line-error
\\ -shell-escape
\\ -output-directory=\"%:h\"
\\ \"%\"
\\ &&
\\ dvipdfmx\ -o\ \"%:r.pdf\"\ \"%:r.dvi\"
CompilerSet errorformat=%f:%l:\ %m

let g:current_compiler = 'platex'
