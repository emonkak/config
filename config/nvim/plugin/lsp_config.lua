if vim.g.loaded_lsp_config == 1 then
  return
end

local null_ls = require('null-ls')

null_ls.setup({
  sources = {
    null_ls.builtins.formatting.biome.with({
      condition = function(utils)
        return utils.root_has_file({ 'biome.json' })
      end,
      filetypes = {
        'javascript',
        'javascriptreact',
        'typescript',
        'typescriptreact',
      },
      temp_dir = '/tmp',
    }),
  },
})

local function find_root_dir(filename, patterns)
  local files = vim.fs.find(patterns, {
    upward = true,
    path = vim.fn.fnamemodify(filename, ':p:h'),
  })
  return files[1] and vim.fs.dirname(files[1])
end

local function get_attached_client_count(bufnr)
  local count = 0
  local clients = vim.lsp.get_active_clients()
  for _, client in ipairs(clients) do
    if vim.lsp.buf_is_attached(bufnr, client.id) then
      count = count + 1
    end
  end
  return count
end

local SERVER_DEFINITIONS = {
  rust_analyzer = {
    cmd = { 'rust-analyzer' },
    filetypes = { 'rust' },
    root_dir = function(filename)
      return find_root_dir(filename, { '.git', 'Cargo.toml' })
    end,
    config = {
      hover = {
        memoryLayout = {
          niches = true,
        },
      },
    },
    formatting = true,
  },
  haskell_language_server = {
    cmd = { 'haskell-language-server-wrapper', '--lsp' },
    filetypes = { 'haskell', 'lhaskell' },
    root_dir = function(filename)
      return find_root_dir(filename, { '.git', 'Setup.hs', 'stack.yml' })
    end,
    formatting = true,
  },
  typescript_language_server = {
    cmd = { 'typescript-language-server', '--stdio' },
    filetypes = {
      'javascript',
      'javascriptreact',
      'typescript',
      'typescriptreact',
    },
    root_dir = function(filename)
      return find_root_dir(filename, { '.git', 'package.json' })
    end,
    config = {
      preferences = {
        disableSuggestions = true,
      },
    },
  },
}

local LSP_CONFIG_AUGROUP = vim.api.nvim_create_augroup('MyLspConfig', {})

for name, definition in pairs(SERVER_DEFINITIONS) do
  if vim.fn.executable(definition.cmd[1]) == 1 then
    vim.api.nvim_create_autocmd('FileType', {
      group = LSP_CONFIG_AUGROUP,
      pattern = definition.filetypes,
      callback = function(args)
        if vim.api.nvim_buf_get_name(args.buf) == '' or
          vim.api.nvim_buf_get_option(args.buf, 'buftype') ~= '' then
          return
        end
        local root_dir = definition.root_dir(args.file)
        if root_dir == nil then
          return
        end
        local client_id = vim.lsp.start({
          name = name,
          cmd = definition.cmd,
          root_dir = root_dir,
          config = definition.config or {},
        })
        vim.lsp.buf_attach_client(args.buf, client_id)
      end,
    })
  end
end

vim.api.nvim_create_autocmd('BufWinEnter', {
  group = LSP_CONFIG_AUGROUP,
  callback = function(args)
    if get_attached_client_count(args.bufnr) > 0 then
      vim.api.nvim_set_option_value('signcolumn', 'yes', { buf = args.buf })
    end
  end
})

vim.api.nvim_create_autocmd('LspAttach', {
  group = LSP_CONFIG_AUGROUP,
  callback = function(args)
    if get_attached_client_count(args.bufnr) == 1 then
      local map = function(lhs, rhs)
        vim.keymap.set('n', lhs, rhs, { buffer = args.buf })
      end

      map('K', vim.lsp.buf.hover)
      map('<LocalLeader>a', vim.lsp.buf.code_action)
      map('<LocalLeader>d', vim.lsp.buf.definition)
      map('<LocalLeader>D', vim.lsp.buf.declaration)
      map('<LocalLeader>f', vim.lsp.buf.format)
      map('<LocalLeader>i', vim.lsp.buf.implementation)
      map('<LocalLeader>j', vim.diagnostic.goto_next)
      map('<LocalLeader>k', vim.diagnostic.goto_prev)
      map('<LocalLeader>n', vim.lsp.buf.rename)
      map('<LocalLeader>r', vim.lsp.buf.references)
      map('<LocalLeader>t', vim.lsp.buf.type_definition)

      vim.api.nvim_set_option_value('signcolumn', 'yes', { buf = args.buf })

      vim.api.nvim_create_autocmd('CursorHold', {
        group = LSP_CONFIG_AUGROUP,
        buffer = args.buf,
        callback = function(args)
          for _, winid in pairs(vim.api.nvim_tabpage_list_wins(0)) do
            if vim.api.nvim_win_get_config(winid).zindex then
              return
            end
          end

          vim.diagnostic.open_float(args.buf, {
            scope = 'cursor',
            focusable = false,
            close_events = {
              'CursorMoved',
              'CursorMovedI',
              'BufHidden',
              'InsertCharPre',
              'WinLeave',
            },
          })
        end,
      })

      vim.api.nvim_create_autocmd('BufWritePre', {
        group = LSP_CONFIG_AUGROUP,
        buffer = args.buf,
        callback = function(args)
          vim.lsp.buf.format({
            async = false,
            filter = function(client)
              if client.name == 'null-ls' then
                return true
              end
              local definition = SERVER_DEFINITIONS[client.name]
              if definition == nil then
                return false
              end
              return definition.formatting or false
            end
          })
        end,
      })
    end

    local client = vim.lsp.get_client_by_id(args.data.client_id)

    if client.server_capabilities.documentSymbolProvider then
      require('lsp_fold').setup(args.buf)
    end

    if client.server_capabilities.completionProvider then
      vim.bo[args.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
    end

    if client.server_capabilities.definitionProvider then
      vim.bo[args.buf].tagfunc = 'v:lua.vim.lsp.tagfunc'
    end
  end,
})

vim.api.nvim_create_autocmd('LspDetach', {
  group = LSP_CONFIG_AUGROUP,
  callback = function(args)
    if get_attached_client_count(args.bufnr) == 1 then
      local unmap = function(lhs)
        vim.keymap.del('n', lhs, { buffer = args.buf })
      end

      unmap('K')
      unmap('<LocalLeader>a')
      unmap('<LocalLeader>d')
      unmap('<LocalLeader>D')
      unmap('<LocalLeader>f')
      unmap('<LocalLeader>i')
      unmap('<LocalLeader>j')
      unmap('<LocalLeader>k')
      unmap('<LocalLeader>n')
      unmap('<LocalLeader>r')
      unmap('<LocalLeader>t')

      vim.cmd('setlocal signcolumn<')

      vim.api.nvim_clear_autocmds({
        group = LSP_CONFIG_AUGROUP,
        buffer = args.buf,
      })
    end

    local client = vim.lsp.get_client_by_id(args.data.client_id)
    local definition = SERVER_DEFINITIONS[client.name]

    if definition == nil then
      return
    end

    if client.server_capabilities.documentSymbolProvider then
      require('lsp_fold').restore(args.buf)
    end
  end,
})

vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(
  vim.lsp.handlers.hover,
  {
    border = 'rounded',
  }
)

vim.diagnostic.config {
  float = {
    border = 'rounded',
  },
  severity_sort = true,
  virtual_text = {
    prefix = '*',
    spacing = 2,
  },
}

local on_ColorScheme = function()
  vim.api.nvim_set_hl(0, '@lsp.type.variable', {})
end

vim.api.nvim_create_autocmd('ColorScheme', {
  group = LSP_CONFIG_AUGROUP,
  callback = on_ColorScheme,
})

on_ColorScheme()

-- vim.lsp.set_log_level('debug')

vim.g.loaded_lsp_config = 1
