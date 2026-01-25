if vim.g.loaded_treesitter_config == 1 then
  return
end

local installed_languages = {
  'css',
  'diff',
  'gitcommit',
  'html',
  'javascript',
  'lua',
  'markdown',
  'query',
  'rust',
  'typescript',
  'vimdoc',
}

local enabled_filetypes = {
  ['css'] = 'css',
  ['diff'] = 'diff',
  ['gitcommit'] = 'gitcommit',
  ['html'] = 'html',
  ['javascript'] = 'javascript',
  ['javascriptreact'] = 'javascript',
  ['lua'] = 'lua',
  ['query'] = 'query',
  ['rust'] = 'rust',
  ['typescript'] = 'typescript',
  ['typescriptreact'] = 'typescript',
  ['vimdoc'] = 'vimdoc',
}

require('nvim-treesitter').setup()

require('nvim-treesitter').install(installed_languages)

require('nvim-treesitter-textobjects').setup()

vim.api.nvim_create_autocmd('FileType', {
  pattern = '*',
  callback = function(args)
    local language = enabled_filetypes[args.match]
    if language == nil then
      return
    end
    vim.treesitter.start(args.buf, language)
    vim.api.nvim_set_option_value(
      'indentexpr',
      'v:lua.require("nvim-treesitter").indentexpr()',
      { buf = args.buf }
    )
  end,
})

vim.keymap.set({ 'x', 'o' }, 'ac', function()
  require('nvim-treesitter-textobjects.select')
    .select_textobject('@call.outer', 'textobjects')
end)
vim.keymap.set({ 'x', 'o' }, 'ic', function()
  require('nvim-treesitter-textobjects.select')
    .select_textobject('@call.inner', 'textobjects')
end)
vim.keymap.set({ 'x', 'o' }, 'af', function()
  require('nvim-treesitter-textobjects.select')
    .select_textobject('@function.outer', 'textobjects')
end)
vim.keymap.set({ 'x', 'o' }, 'if', function()
  require('nvim-treesitter-textobjects.select')
    .select_textobject('@function.inner', 'textobjects')
end)
vim.keymap.set({ 'x', 'o' }, 'a,', function()
  require('nvim-treesitter-textobjects.select')
    .select_textobject('@parameter.outer', 'textobjects')
end)
vim.keymap.set({ 'x', 'o' }, 'i,', function()
  require('nvim-treesitter-textobjects.select')
    .select_textobject('@parameter.inner', 'textobjects')
end)

vim.g.loaded_treesitter_config = 1
