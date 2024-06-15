if exists('g:current_compiler')
  finish
endif

CompilerSet makeprg=./gradlew\ --daemon\ --quiet\ assembleDebug
CompilerSet errorformat=%E%f:%l:\ %m,%-Z%p^,%-C%.%#,%-G%.%#

let g:current_compiler = 'gradle'
