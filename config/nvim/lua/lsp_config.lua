if vim.fn.executable('rust-analyzer') == 1 then
  vim.api.nvim_create_autocmd('FileType', {
    group = 'MyAutoCmd',
    pattern = 'rust',
    callback = function(args)
      if vim.api.nvim_buf_get_name(args.buf) == '' then
        return
      end
      local root_dir = vim.fs.dirname(
        vim.fs.find({ '.git', 'Cargo.toml' }, {
          upward = true,
          path = vim.fn.fnamemodify(args.file, ':p:h'),
        })[1]
      )
      local client = vim.lsp.start({
        name = 'rust-analyzer',
        cmd = { 'rust-analyzer' },
        root_dir = root_dir,
        config = {
          hover = {
            memoryLayout = {
              niches = true,
            },
          },
        },
      })
      vim.lsp.buf_attach_client(0, client)
    end,
  })
end

if vim.fn.executable('haskell-language-server-wrapper') == 1 then
  vim.api.nvim_create_autocmd('FileType', {
    group = 'MyAutoCmd',
    pattern = 'haskell',
    callback = function(args)
      if vim.api.nvim_buf_get_name(args.buf) == '' then
        return
      end
      local root_dir = vim.fs.dirname(
        vim.fs.find({ '.git', 'Setup.hs', 'stack.yml' }, {
          upward = true,
          path = vim.fn.fnamemodify(args.file, ':p:h'),
        })[1]
      )
      local client = vim.lsp.start({
        name = 'haskell-language-server',
        cmd = { 'haskell-language-server-wrapper', '--lsp', '--cwd', root_dir },
        root_dir = root_dir,
      })
      vim.lsp.buf_attach_client(0, client)
    end,
  })
end

if vim.fn.executable('typescript-language-server') == 1 then
  vim.api.nvim_create_autocmd('FileType', {
    group = 'MyAutoCmd',
    pattern = {
      'javascript',
      'javascriptreact',
      'typescript',
      'typescriptreact',
    },
    callback = function(args)
      if vim.api.nvim_buf_get_name(args.buf) == '' then
        return
      end
      local root_dir = vim.fs.dirname(
        vim.fs.find({ '.git', 'package.json' }, {
          upward = true,
          path = vim.fn.fnamemodify(args.file, ':p:h'),
        })[1]
      )
      local client = vim.lsp.start({
        name = 'typescript-language-server',
        cmd = { 'typescript-language-server', '--stdio' },
        root_dir = root_dir,
        config = {
          preferences = {
            disableSuggestions = true,
          },
        },
      })
      vim.lsp.buf_attach_client(0, client)
    end,
  })
end

vim.api.nvim_create_autocmd('LspAttach', {
  group = 'MyAutoCmd',
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

    vim.cmd('setlocal signcolumn=yes')

    local client = vim.lsp.get_client_by_id(args.data.client_id)

    if client.server_capabilities.completionProvider then
      vim.bo[args.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
    end

    if client.server_capabilities.definitionProvider then
      vim.bo[args.buf].tagfunc = 'v:lua.vim.lsp.tagfunc'
    end

    if vim.bo.filetype == 'rust'
      or vim.bo.filetype == 'javascript' then
      vim.api.nvim_create_autocmd('BufWritePre', {
        buffer = args.buf,
        callback = function(args)
          vim.lsp.buf.format({ async = false })
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

vim.api.nvim_create_autocmd('LspDetach', {
  callback = function(args)
    vim.cmd('setlocal signcolumn<')
  end,
})

local on_ColorScheme = function()
  vim.api.nvim_set_hl(0, '@lsp.type.property', {})
  vim.api.nvim_set_hl(0, '@lsp.type.variable', {})
end

vim.api.nvim_create_autocmd('ColorScheme', {
  group = 'MyAutoCmd',
  callback = on_ColorScheme,
})

on_ColorScheme()

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

-- vim.lsp.set_log_level('debug')
