if exists('g:main_syntax')
  finish
endif

function! s:include_html_syntax()
  let original_current_syntax = b:current_syntax

  unlet b:current_syntax
  let g:main_syntax = 'java'
  let g:java_css = 1

  syntax include @javascriptHtml syntax/html.vim

  unlet g:java_css
  unlet g:main_syntax
  let b:current_syntax = original_current_syntax
endfunction

call s:include_html_syntax()

syntax keyword javaScriptAsync  async
syntax keyword javaScriptRepeat  of

syntax clear javaScriptDeprecated
syntax clear javaScriptGlobal
syntax clear javaScriptMember
syntax clear javaScriptMessage

syntax clear javaScriptReserved
syntax keyword javaScriptReserved
\              await
\              break
\              case
\              catch
\              class
\              const
\              continue
\              debugger
\              default
\              delete
\              do
\              else
\              enum
\              export
\              extends
\              false
\              finally
\              for
\              function
\              if
\              implements
\              import
\              in
\              instanceof
\              interface
\              let
\              new
\              null
\              package
\              private
\              protected
\              public
\              return
\              static
\              super
\              switch
\              this
\              throw
\              true
\              try
\              typeof
\              var
\              void
\              while
\              with
\              yield

syntax keyword javaScriptImportDeclaration
\              import
\              nextgroup=javaScriptImportedBinding,javaScriptNameSpaceImport,javaScriptNamedImports,javaScriptModuleSpecifier
\              skipwhite
syntax match javaScriptImportedBinding
\            "\<\K\k*\(\s*,\s*\)\?"
\            nextgroup=javaScriptNamedImports,javaScriptFromClause
\            contained
\            skipwhite
syntax match javaScriptNameSpaceImport
\            "\*"
\            nextgroup=javaScriptNameSpaceImportAs
\            contained
\            skipwhite
syntax keyword javaScriptNameSpaceImportAs
\              as
\              nextgroup=javaScriptImportedBinding
\              contained
\              skipwhite
syntax region javaScriptNamedImports
\             start="{"
\             end="}"
\             contains=@javaScriptModuleSpecifier,javaScriptNamedImportAs
\             nextgroup=javaScriptFromClause
\             contained
\             skipwhite
syntax cluster javaScriptModuleSpecifier
\              contains=javaScriptStringD,javaScriptStringS
syntax keyword javaScriptNamedImportAs  as  contained
syntax keyword javaScriptFromClause
\              from
\              nextgroup=@javaScriptModuleSpecifier
\              contained
\              skipwhite

syntax region javaScriptStringHtml
\             matchgroup=String
\             start=+html`+
\             skip=+\\`+
\             end=+`+
\             contains=javaScriptEmbedHtml,@javascriptHtml
\             extend
\             keepend
syntax region javaScriptEmbed
\             matchgroup=Special
\             start=+${+
\             end=+}+
\             contains=@javaScriptEmbededExpr
\             keepend
syntax region javaScriptEmbedHtml
\             matchgroup=Special
\             start=+${+
\             end=+}+
\             contained
\             containedin=htmlComment,htmlLink,htmlString,htmlTag
\             contains=@javaScriptEmbededExpr
\             keepend

highlight default link javaScriptAsync  Keyword
highlight default link javaScriptFromClause  Keyword
highlight default link javaScriptImportDeclaration  Keyword
highlight default link javaScriptNameSpaceImportAs  Keyword
highlight default link javaScriptNamedImportAs  Keyword
