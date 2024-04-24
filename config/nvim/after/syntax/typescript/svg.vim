if exists('g:main_syntax')
  finish
endif

function! s:include_svg_syntax()
  let original_current_syntax = b:current_syntax

  unlet b:current_syntax

  syntax include @typescriptSVGSyntax syntax/svg.vim

  let b:current_syntax = original_current_syntax
endfunction

call s:include_svg_syntax()

syntax region typescriptSVGTemplate
\ matchgroup=String
\ start='\%(\K\k*\.\|\<\)svg\s*`'hs=e
\ skip='\\`'
\ end='`'
\ contains=@typescriptSVGSyntax,typescriptSVGTemplateHole

syntax region typescriptSVGTemplateAttribute
\ keepend
\ start='=\s*\${'
\ end='}'
\ contains=typescriptSVGTemplateHole
\ contained
\ containedin=xmlTag

syntax region typescriptSVGTemplateHole
\ matchgroup=Special
\ start='\${'
\ end='}'
\ contains=@typescriptValue
\ contained
\ containedin=xmlComment,xmlString,xmlTag

syntax cluster typescriptTopExpression add=typescriptSVGTemplate
