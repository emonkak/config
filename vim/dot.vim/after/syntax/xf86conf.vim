" Vim syntax: xf86conf
" Version: 0.0.0

syntax clear xf86confSection
syntax region xf86confSection
\ matchgroup=xf86confSectionDelim
\ start="^\s*Section\s\+\"\(Files\|Server[_ ]*Flags\|Input[_ ]*\(Device\|Class\)\|Device\|Video[_ ]*Adaptor\|Server[_ ]*Layout\|DRI\|Extensions\|Vendor\|Keyboard\|Pointer\)\""
\ end="^\s*EndSection\>"
\ skip="#.*$\|\"[^\"]*\""
\ contains=xf86confComment,xf86confOption,xf86confKeyword,xf86confSectionError

syntax keyword xf86confKeyword
\ MatchProduct MatchVendor MatchDevicePath MatchIsKeyboard MatchIsPointer MatchIsJoystick MatchIsTablet MatchIsTouchpad MatchIsTouchscreen
\ nextgroup=xf86confComment,xf86confValue
