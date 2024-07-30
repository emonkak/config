if vim.g.loaded_treesitter_config == 1 then
  return
end

local TREESITTER_CONFIG_AUGROUP = vim.api.nvim_create_augroup('MyTreesitterConfig', {})

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

local function configure_fold_options()
  vim.api.nvim_set_option_value('foldenable', true, { scope = 'local' })
  vim.api.nvim_set_option_value('foldmethod', 'expr', { scope = 'local' })
  vim.api.nvim_set_option_value(
    'foldexpr',
    'v:lua.vim.treesitter.foldexpr()',
    { scope = 'local' }
  )
end

vim.api.nvim_create_autocmd('Filetype', {
  group = TREESITTER_CONFIG_AUGROUP,
  callback = function(args)
    if require('nvim-treesitter.parsers').has_parser(args.match) then
      configure_fold_options()
    end
  end,
})

vim.api.nvim_create_autocmd('BufWinEnter', {
  group = TREESITTER_CONFIG_AUGROUP,
  callback = function(args)
    local filetype = vim.api.nvim_get_option_value('filetype', {
      buf = args.buf,
    })
    if filetype ~= ''
      and require('nvim-treesitter.parsers').has_parser(filetype) then
      configure_fold_options()
    end
  end
})

vim.g.loaded_treesitter_config = 1
