from keyhac import *

def configure(keymap):
    # Font
    keymap.setFont('Osaka-Mono', 16)

    # Theme
    keymap.setTheme('black')

    # Right Alt to Right Ctrl
    keymap.replaceKey('RAlt', 'RCtrl')

    # Global keymap which affects any windows
    keymap_global = keymap.defineWindowKeymap()

    # Swap Semicolon and Colon
    keymap_global['Shift-Semicolon'] = 'Colon'
    keymap_global['Semicolon'] = 'Shift-Semicolon'

    # VI coordinated escape
    keymap_global['Ctrl-OpenBracket'] = '({})'.format(VK_Eisu), 'Escape'
    keymap_global['Escape'] = '({})'.format(VK_Eisu), 'Escape'

    # IME control like JIS keyboard
    keymap_global["O-LCmd"] = '({})'.format(VK_Eisu), 'Escape'
    keymap_global["O-RCmd"] = '({})'.format(VK_Kana)
