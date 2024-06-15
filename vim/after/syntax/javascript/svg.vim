if exists('g:main_syntax')
  finish
endif

function! s:include_svg_syntax()
  let original_current_syntax = b:current_syntax

  unlet b:current_syntax

  syntax include @jsSVGSyntax syntax/svg.vim

  let b:current_syntax = original_current_syntax
endfunction

call s:include_svg_syntax()

syntax region jsSVGTemplate
\ matchgroup=String
\ start='\%(\K\k*\.\|\<\)svg\s*`'hs=e
\ skip='\\`'
\ end='`'
\ contains=@jsSVGSyntax,jsSVGTemplateHole

syntax region jsSVGTemplateAttribute
\ keepend
\ start='=\s*\${'
\ end='}'
\ contains=jsSVGTemplateHole
\ contained
\ containedin=xmlTag

syntax region jsSVGTemplateHole
\ matchgroup=Special
\ start='\${'
\ end='}'
\ contains=@jsExpression
\ contained
\ containedin=xmlComment,xmlString,xmlTag

syntax cluster jsExpression add=jsSVGTemplate
