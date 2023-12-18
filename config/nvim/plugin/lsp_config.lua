if vim.g.loaded_lsp_config == 1 then
  return
end

local null_ls = require('null-ls')

null_ls.setup({
    sources = {
        null_ls.builtins.formatting.prettier,
    },
})

local function find_root_dir(filename, patterns)
  local files = vim.fs.find(patterns, {
    upward = true,
    path = vim.fn.fnamemodify(filename, ':p:h'),
  })
  return files[1] and vim.fs.dirname(files[1])
end

local servers = {
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
  },
  haskell_language_server = {
    cmd = { 'haskell-language-server-wrapper', '--lsp' },
    filetypes = { 'haskell', 'lhaskell' },
    root_dir = function(filename)
      return find_root_dir(filename, { '.git', 'Setup.hs', 'stack.yml' })
    end,
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

local lsp_config_augroup = vim.api.nvim_create_augroup('MyLspConfig', {})

for name, server in pairs(servers) do
  if vim.fn.executable(server.cmd[1]) == 1 then
    vim.api.nvim_create_autocmd('FileType', {
      group = lsp_config_augroup,
      pattern = server.filetypes,
      callback = function(args)
        if vim.api.nvim_buf_get_name(args.buf) == '' then
          return
        end
        local root_dir = server.root_dir(args.file)
        if root_dir == nil then
          return
        end
        local client = vim.lsp.start({
          name = name,
          cmd = server.cmd,
          root_dir = root_dir,
          config = server.config or {},
        })
        vim.lsp.buf_attach_client(args.buf, client)
      end,
    })
  end
end

vim.api.nvim_create_autocmd('LspAttach', {
  group = lsp_config_augroup,
  callback = function(args)
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

    if client.server_capabilities.documentFormattingProvider then
      vim.api.nvim_create_autocmd('BufWritePre', {
        buffer = args.buf,
        callback = function(args)
          vim.lsp.buf.format({
            async = false,
            filter = function(client)
              return client.name ~= 'typescript_language_server'
            end
          })
        end,
      })
    end

    vim.api.nvim_create_autocmd('CursorHold', {
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
  group = lsp_config_augroup,
  callback = on_ColorScheme,
})

on_ColorScheme()

-- vim.lsp.set_log_level('debug')

vim.g.loaded_lsp_config = 1
