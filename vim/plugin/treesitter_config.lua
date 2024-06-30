if vim.g.loaded_treesitter_config == 1 then
  return
end

require('nvim-treesitter.configs').setup({
  ensure_installed = {
    'css',
    'html',
    'javascript',
    'lua',
    'markdown',
    'query',
    'rust',
    'typescript',
    'vimdoc',
  },
  parser_install_dir = vim.fn.stdpath('data') .. '/site',
  highlight = {
    enable = true,
    disable = { 'markdown' },
  },
  indent = {
    enable = true,
  },
  textobjects = {
    select = {
      enable = true,
      keymaps = {
        ['ac'] = '@call.outer',
        ['ic'] = '@call.inner',
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['a,'] = '@parameter.outer',
        ['i,'] = '@parameter.inner',
      },
    },
  },
})

vim.api.nvim_create_autocmd('Filetype', {
  callback = function(args)
    if require('nvim-treesitter.parsers').has_parser(args.match) then
      vim.api.nvim_set_option_value('foldmethod', 'expr', { scope = 'local' })
      vim.api.nvim_set_option_value(
        'foldexpr',
        'v:lua.vim.treesitter.foldexpr()',
        { scope = 'local' }
      )
    end
  end,
})

vim.g.loaded_treesitter_config = 1
