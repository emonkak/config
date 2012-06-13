" Vim syntax: registry
" Version: 0.0.0

syntax match registryHead  "Windows Registry Editor Version \d\+\.\d\+"

syntax clear registryString
syntax match registryString  "\".\{-}\""
\ contains=registryGUID,registrySpecial

syntax clear registrySubKey
syntax match registrySubKey  "^\".\{-}\"="
syntax match registrySubKey  "^@="

highlight link registryHead  PreProc
