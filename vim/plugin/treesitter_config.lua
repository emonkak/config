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

local function configure_treesitter_fold()
  vim.api.nvim_set_option_value('foldmethod', 'expr', { scope = 'local' })
  vim.api.nvim_set_option_value(
    'foldexpr',
    'v:lua.vim.treesitter.foldexpr()',
    { scope = 'local' }
  )
end

local function is_treesitter_fold_enabled()
  return vim.api.nvim_get_option_value('foldmethod', { scope = 'local' }) == 'expr'
    and vim.api.nvim_get_option_value('foldexpr', { scope = 'local' }) == 'v:lua.vim.treesitter.foldexpr()'
end

vim.api.nvim_create_autocmd('FileType', {
  group = TREESITTER_CONFIG_AUGROUP,
  callback = function(args)
    if vim.treesitter.highlighter.active[args.buf] then
      configure_treesitter_fold()
    end
  end,
})

vim.api.nvim_create_autocmd('BufWinEnter', {
  group = TREESITTER_CONFIG_AUGROUP,
  callback = function(args)
    if is_treesitter_fold_enabled() then
      vim.b.is_treesitter_fold_enabled = true
      return
    end

    if vim.b.is_treesitter_fold_enabled then
      configure_treesitter_fold()
    end
  end
})

vim.g.loaded_treesitter_config = 1
