if vim.g.loaded_treesitter_config == 1 then
  return
end

local TREESITTER_CONFIG_AUGROUP = vim.api.nvim_create_augroup('MyTreesitterConfig', {})

require('nvim-treesitter.configs').setup({
  ensure_installed = {
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

vim.g.loaded_treesitter_config = 1
