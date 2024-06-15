if vim.g.loaded_lsp_config == 1 then
  return
end

local null_ls = require('null-ls')

null_ls.setup({
  sources = {
    null_ls.builtins.formatting.biome.with({
      args = {
        'check',
        '--write',
        '--formatter-enabled=true',
        'true',
        '--skip-errors',
        '--stdin-file-path=$FILENAME',
      },
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

local function root_files(filenames)
  return function(path)
    local matches = vim.fs.find(filenames, {
      upward = true,
      path = path,
    })
    return matches[1] and vim.fs.dirname(matches[1])
  end
end

local function run_command_without_errors(command)
  local ok, error = pcall(vim.api.nvim_command, command)
  if not ok then
    vim.api.nvim_err_writeln(error)
  end
end

local SERVER_DEFINITIONS = {
  {
    name = 'rust-analyzer',
    cmd = { 'rust-analyzer' },
    filetypes = { 'rust' },
    root_dir = root_files({ '.git', 'Cargo.toml' }),
    override_config = {
      settings = {
        ['rust-analyzer'] = {
          hover = {
            memoryLayout = {
              niches = true,
            },
          },
        },
      },
    },
  },
  {
    name = 'haskell-language-server',
    cmd = { 'haskell-language-server-wrapper', '--lsp' },
    filetypes = { 'haskell', 'lhaskell' },
    root_dir = root_files({ '.git', 'Setup.hs', 'stack.yml' }),
  },
  {
    name = 'vtsls',
    cmd = { 'vtsls', '--stdio' },
    filetypes = {
      'javascript',
      'javascriptreact',
      'typescript',
      'typescriptreact',
    },
    root_dir = function(path)
      if root_files({ '.flowconfig' })(path) then
        return nil
      end
      return root_files({ '.git', 'package.json' })(path)
    end,
    override_config = {
      on_attach = function(client, bufnr)
        vim.bo[bufnr].formatexpr = nil
        client.server_capabilities.documentFormattingProvider = false
      end,
    },
  },
  {
    name = 'phpactor',
    cmd = { 'phpactor', 'language-server' },
    filetypes = { 'php' },
    root_dir = root_files({ '.git', 'composer.json' }),
  },
}

local LSP_CONFIG_AUGROUP = vim.api.nvim_create_augroup('MyLspConfig', {})

for i, server_definition in ipairs(SERVER_DEFINITIONS) do
  if vim.fn.executable(server_definition.cmd[1]) == 0 then
    goto continue
  end
  vim.api.nvim_create_autocmd('FileType', {
    group = LSP_CONFIG_AUGROUP,
    pattern = server_definition.filetypes,
    callback = function(args)
      if vim.api.nvim_buf_get_name(args.buf) == ''
        or vim.api.nvim_buf_get_option(args.buf, 'buftype') ~= '' then
        return
      end
      local config = {
        name = server_definition.name,
        cmd = server_definition.cmd,
        root_dir = server_definition.root_dir(
          vim.fn.fnamemodify(args.file, ':p:h')
        ),
      }
      if server_definition.override_config then
        config = vim.tbl_extend(
          'force',
          config,
          server_definition.override_config
        )
      end
      if not config.root_dir then
        return
      end
      local client_id = vim.lsp.start(config)
      vim.lsp.buf_attach_client(args.buf, client_id)
    end,
  })
  ::continue::
end

vim.api.nvim_create_autocmd('BufWinEnter', {
  group = LSP_CONFIG_AUGROUP,
  callback = function(args)
    local clients = vim.lsp.get_active_clients({ bufnr = args.buf })
    if #clients > 0 then
      vim.api.nvim_set_option_value('signcolumn', 'yes', { buf = args.buf })
    end
  end
})

vim.api.nvim_create_autocmd('LspAttach', {
  group = LSP_CONFIG_AUGROUP,
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client.name == 'null-ls' then
      return
    end

    local map = function(lhs, rhs)
      vim.keymap.set('n', lhs, rhs, { buffer = args.buf })
    end

    map('K', vim.lsp.buf.hover)
    map(']d', vim.diagnostic.goto_next)
    map('[d', vim.diagnostic.goto_prev)
    map('<LocalLeader>D', vim.lsp.buf.declaration)
    map('<LocalLeader>a', vim.lsp.buf.code_action)
    map('<LocalLeader>d', vim.lsp.buf.definition)
    map('<LocalLeader>f', vim.lsp.buf.format)
    map('<LocalLeader>i', vim.lsp.buf.implementation)
    map('<LocalLeader>n', vim.lsp.buf.rename)
    map('<LocalLeader>r', vim.lsp.buf.references)
    map('<LocalLeader>t', vim.lsp.buf.type_definition)
    map('<LocalLeader><LocalLeader>', function()
      vim.diagnostic.setqflist({ open = false })
      run_command_without_errors('cc')
    end)

    vim.api.nvim_set_option_value('signcolumn', 'yes', { buf = args.buf })

    if client.server_capabilities.completionProvider then
      vim.bo[args.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
    end

    if client.server_capabilities.definitionProvider then
      vim.bo[args.buf].tagfunc = 'v:lua.vim.lsp.tagfunc'
    end

    if client.server_capabilities.documentSymbolProvider then
      require('lsp_fold').setup(args.buf)
    end

    vim.api.nvim_clear_autocmds({
      group = LSP_CONFIG_AUGROUP,
      buffer = args.buf,
    })

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
        })
      end,
    })
  end
})

vim.api.nvim_create_autocmd('LspDetach', {
  group = LSP_CONFIG_AUGROUP,
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client.name == 'null-ls' then
      return
    end

    local unmap = function(lhs)
      pcall(vim.keymap.del, 'n', lhs, { buffer = args.buf })
    end

    unmap('K')
    unmap('[d')
    unmap(']d')
    unmap('<LocalLeader>D')
    unmap('<LocalLeader>a')
    unmap('<LocalLeader>d')
    unmap('<LocalLeader>f')
    unmap('<LocalLeader>i')
    unmap('<LocalLeader>n')
    unmap('<LocalLeader>r')
    unmap('<LocalLeader>t')
    unmap('<LocalLeader><LocalLeader>')

    vim.api.nvim_set_option_value('signcolumn', vim.o.signcolumn, {
      buf = args.buf,
    })
    vim.api.nvim_set_option_value('omnifunc', nil, { buf = args.buf })
    vim.api.nvim_set_option_value('tagfunc', nil, { buf = args.buf })

    if client.server_capabilities.documentSymbolProvider then
      require('lsp_fold').restore(args.buf)
    end

    vim.api.nvim_clear_autocmds({
      group = LSP_CONFIG_AUGROUP,
      buffer = args.buf,
    })
  end,
})

vim.api.nvim_create_user_command('LspRestartAll', function(info)
  local detached_clients = {}
  for _, client in ipairs(vim.lsp.get_active_clients()) do
    local buffers = vim.lsp.get_buffers_by_client_id(client.id)
    if #buffers > 0 then
      table.insert(detached_clients, { client, buffers })
    end
    client.stop(true)
  end
  if #detached_clients == 0 then
    return
  end

  local MAX_ATTEMPTS = 5
  local timer = vim.loop.new_timer()
  local cursor = 1
  local attempts = 0

  timer:start(
    500,
    100,
    vim.schedule_wrap(function()
      while cursor <= #detached_clients do
        local client, attached_buffers = unpack(detached_clients[cursor])
        if not client.is_stopped() then
          break
        end

        local client_id = vim.lsp.start(client.config)

        for i, bufnr in ipairs(attached_buffers) do
          if vim.api.nvim_buf_is_valid(bufnr) then
            vim.lsp.buf_attach_client(bufnr, client_id)
          end
        end

        detached_clients[cursor] = nil
        cursor = cursor + 1
      end

      attempts = attempts + 1

      if cursor > #detached_clients or attempts >= MAX_ATTEMPTS then
        if not timer:is_closing() then
          timer:close()
        end
      end
    end)
  )
end, {
  desc = 'Restart all LSP client(s) and reattach them to buffer(s)',
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

vim.g.loaded_lsp_config = 1
