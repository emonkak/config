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

require('nvim-treesitter').setup()

require('nvim-treesitter').install(installed_languages)

require('nvim-treesitter-textobjects').setup()

vim.api.nvim_create_autocmd('FileType', {
  pattern = '*',
  callback = function(args)
    if args.match == 'markdown'
      or not vim.tbl_contains(installed_languages, args.match) then
      return
    end
    vim.treesitter.start()
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
