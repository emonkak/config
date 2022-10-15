" Allow nested comment literals
syntax clear vimLineComment
syntax match vimLineComment  +^[ \t:]*".*$+
\ contains=@vimCommentGroup,vimCommentString,vimCommentTitle
