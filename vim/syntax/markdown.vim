if exists('b:current_syntax')
  finish
endif

syntax sync minlines=100

syntax cluster markdownInline
\ contains=markdownHtmlTag,markdownReferenceLabel,markdownFootnote,markdownInlineMath,markdownInlineCode,markdownDelete,markdownStrong,markdownEmphasis,markdownAmpersand,markdownEscape

syntax match markdownEscape
\ '\\[!#$()*+\-.\[\\\]_`{}~]'

syntax match markdownAmpersand
\ '&\%([0-9A-Za-z]\+\|#[0-9]\+\|#x[0-9A-Fa-f]\+\);'

syntax region markdownEmphasis
\ start='\*\S\@='
\ end='\S\@<=\*'
\ skip='\\\*'
\ oneline
syntax region markdownEmphasis
\ start='_\S\@='
\ end='\S\@<=_'
\ skip='\\_'
\ oneline

syntax region markdownStrong
\ start='\*\{2}\S\@='
\ end='\S\@<=\*\{2}'
\ skip='\\\*'
\ oneline
syntax region markdownStrong
\ start='_\{2}\S\@='
\ end='\S\@<=_\{2}'
\ skip='\\_'
\ oneline

syntax region markdownDelete
\ start='\~\{2}\S\@='
\ end='\S\@<=\~\{2}'
\ skip='\\\~'
\ oneline

syntax region markdownInlineCode
\ start='`\S\@='
\ end='\S\@<=`'
\ skip='\\`'
\ oneline
syntax region markdownInlineMath
\ start='\$\S\@='
\ end='\S\@<=\$'
\ skip='\\\$'
\ oneline

syntax region markdownFootnote
\ start='\[\^\S\@='
\ end='\S\@<=\]'
\ skip='\\]'
\ oneline

syntax region markdownReferenceLabel
\ start='!\?\[\S\@='
\ end='\]'
\ skip='\\]'
\ oneline
\ contains=@markdownInline
\ nextgroup=markdownReferenceIdentifier,markdownReferenceUrl
\ skipwhite
syntax region markdownReferenceIdentifier
\ start='\['
\ end='\]'
\ skip='\\]'
\ oneline
\ contains=NONE
\ contained
syntax region markdownReferenceUrl
\ start='('
\ end=')'
\ skip='\\)'
\ oneline
\ keepend
\ contains=markdownUrl
\ contained

syntax match markdownUrl
\ '\S\+'
\ contained
\ nextgroup=markdownUrlTitle
\ skipwhite
syntax region markdownUrlTitle
\ start='"'
\ end='"'
\ skip='\\"'
\ oneline
\ contained
syntax region markdownUrlTitle
\ start="'"
\ end="'"
\ skip="\\'"
\ oneline
\ contained
syntax region markdownUrlTitle
\ start='('
\ end=')'
\ skip='\\)'
\ oneline
\ contained

syntax region markdownHtmlTag
\ start='</\?'
\ end='\s*>'
\ contains=markdownHtmlTagName
\ keepend
syntax match markdownHtmlTagName
\ '<\s*[A-Za-z][\-0-9A-Za-z]*'hs=s+1
\ contained
\ nextgroup=markdownHtmlAttribute
\ skipempty
\ skipnl
syntax match markdownHtmlTagName
\ '</\s*[A-Za-z][\-0-9A-Za-z]*'hs=s+2
\ contained
\ nextgroup=markdownHtmlAttribute
\ skipempty
\ skipnl
syntax region markdownHtmlAttribute
\ start='\s\+[-0-9A-Za-z]\+\s*='
\ end='\z1'
\ contained
\ nextgroup=markdownHtmlUnquotedValue,markdownHtmlQuotedValue
\ skipempty
\ skipnl
\ skipwhite
syntax region markdownHtmlQuotedValue
\ start="'"
\ end="'"
\ contained
\ nextgroup=markdownHtmlAttribute
\ skipempty
\ skipnl
syntax region markdownHtmlQuotedValue
\ start='"'
\ end='"'
\ contained
\ nextgroup=markdownHtmlAttribute
\ skipempty
\ skipnl
syntax match markdownHtmlUnquotedValue
\ '\S\+'
\ contained
\ nextgroup=markdownHtmlAttribute
\ skipempty
\ skipnl

syntax match markdownLine
\ '^\%(\t\| \{2,4}\)\{-}'
\ nextgroup=@markdownInline,@markdownBlock

syntax cluster markdownBlock
\ contains=markdownDefinitionIdentifier,markdownIndentedCode,markdownFencedCode,markdownMath,markdownListItem,markdownBlockquote,markdownThematicBreak,markdownHeadingUnderline,markdownHeading

syntax region markdownHeading
\ matchgroup=markdownHeadingDelimiter
\ start=' \{0,3}\z(#\{1,6}\)\s\+'
\ end='\(\s\+\z1\)\?\s*$'
\ oneline
\ contains=@markdownInline
\ contained

syntax match markdownHeadingUnderline
\ ' \{0,3}\%(=\{3,}\|-\{3,}\)\%(\s*$\n\?\)\@='
\ contained

syntax match markdownThematicBreak
\ ' \{0,3}\([*\-_]\)\%( \{0,2}\1\)\{2,}\s*$\n\?'
\ contained

syntax match markdownBlockquote
\ ' \{0,3}> \?'
\ contained
\ nextgroup=@markdownInline,@markdownBlock

syntax region markdownListItem
\ matchgroup=markdownListItemMarker
\ start=' \{0,3}[*+-]\s\+'
\ end='$'
\ oneline
\ contains=@markdownInline
\ contained
\ nextgroup=markdownListIndent
\ skipempty
\ skipnl
syntax region markdownListItem
\ matchgroup=markdownListItemMarker
\ start=' \{0,3}[0-9]\+\.\s\+'
\ end='$'
\ oneline
\ contains=@markdownInline
\ contained
\ nextgroup=markdownListIndent
\ skipempty
\ skipnl

syntax region markdownListIndent
\ start='\%(\t\| \{2,4}\)\+'
\ end='$'
\ keepend
\ oneline
\ contains=markdownIndentedBlock
\ contained
\ nextgroup=markdownListIndent
\ skipempty
\ skipnl

syntax match markdownIndentedBlock
\ '\%(\t\| \{2,4}\)\+'
\ nextgroup=@markdownInline,@markdownBlock
\ contained

syntax region markdownIndentedCode
\ start='\t[^\t]\@='
\ end='$'
\ oneline
\ contains=NONE
\ contained
syntax region markdownIndentedCode
\ start=' \{4,}[^ ]\@='
\ end='$'
\ oneline
\ contains=NONE
\ contained

syntax region markdownFencedCode
\ matchgroup=markdownCodeDelimiter
\ start='\z(\s*\)\z(`\{3,}\|\~\{3,}\).*$'
\ end='\z1\z2\s*$'
\ contains=NONE
\ contained
\ extend

syntax region markdownMath
\ matchgroup=markdownMathDelimiter
\ start='\s*\$\{2}'
\ end='\$\{2}\s*$'
\ contains=NONE
\ contained
\ extend

syntax region markdownDefinitionIdentifier
\ start=' \{0,3}\[\S\@='
\ end='\]:'
\ skip='\\]'
\ oneline
\ contains=NONE
\ contained
\ nextgroup=markdownUrl
\ skipwhite

syntax region markdownFrontmatter
\ matchgroup=markdownFrontmatterDelimiter
\ start='\%^[\s\n]*-\{3}\s*$'
\ end='^-\{3}\s*$'
\ contains=NONE

highlight default link markdownAmpersand Special
highlight default link markdownBlockquote Comment
highlight default link markdownCodeDelimiter Delimiter
highlight default link markdownDefinitionIdentifier Typedef
highlight default link markdownDelete Comment
highlight default link markdownEmphasis markdownItalic
highlight default link markdownEscape Special
highlight default link markdownFencedCode String
highlight default link markdownFootnote Define
highlight default link markdownFrontmatter Comment
highlight default link markdownFrontmatterDelimiter Delimiter
highlight default link markdownHeading markdownBold
highlight default link markdownHeadingDelimiter Delimiter
highlight default link markdownHeadingUnderline Delimiter
highlight default link markdownHtmlAttribute Type
highlight default link markdownHtmlQuotedValue String
highlight default link markdownHtmlTag Identifier
highlight default link markdownHtmlTagName Statement
highlight default link markdownHtmlUnquotedValue String
highlight default link markdownIndentedCode String
highlight default link markdownInlineCode String
highlight default link markdownInlineMath String
highlight default link markdownListItemMarker Statement
highlight default link markdownMath String
highlight default link markdownMathDelimiter Delimiter
highlight default link markdownReferenceIdentifier Typedef
highlight default link markdownReferenceLabel Define
highlight default link markdownReferenceUrl String
highlight default link markdownSeparator Delimiter
highlight default link markdownStrong markdownBold
highlight default link markdownUrl String
highlight default link markdownUrlTitle String

highlight default markdownBold term=bold cterm=bold gui=bold
highlight default markdownItalic term=italic cterm=italic gui=italic

let b:current_syntax = 'markdown'
