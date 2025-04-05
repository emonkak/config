if vim.g.loaded_lsp_config == 1 then
  return
end

local null_ls = require('null-ls')

local api = vim.api
local lsp = vim.lsp

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
        'css',
        'javascript',
        'javascriptreact',
        'json',
        'typescript',
        'typescriptreact',
      },
      temp_dir = '/tmp',
    }),
  },
})

if lsp.config then
  lsp.config['haskell-language-server'] = {
    cmd = { 'haskell-language-server', '--lsp' },
    filetypes = { 'haskell', 'lhaskell' },
    root_markers = { '.git', 'Setup.hs', 'stack.yml' },
    workspace_required = true,
    settings = {
      ['rust-analyzer'] = {
        hover = {
          memoryLayout = {
            niches = true,
          },
        },
      },
    },
  }

  lsp.config.phpactor = {
    cmd = { 'phpactor', 'language-server' },
    filetypes = { 'php' },
    root_markers = { '.git', 'composer.json' },
    workspace_required = true,
  }

  lsp.config['rust-analyzer'] = {
    cmd = { 'rust-analyzer' },
    filetypes = { 'rust' },
    root_markers = { '.git', 'Cargo.toml' },
    workspace_required = true,
    settings = {
      ['rust-analyzer'] = {
        hover = {
          memoryLayout = {
            niches = true,
          },
        },
      },
    },
  }

  lsp.config.vtsls = {
    cmd = { 'vtsls', '--stdio' },
    filetypes = {
        'javascript',
        'javascriptreact',
        'typescript',
        'typescriptreact',
      },
    root_dir = function(bufnr, callback)
      local path = api.nvim_buf_get_name(bufnr)
      if #vim.fs.find('.flowconfig', { upward = true, path = path }) > 0 then
        callback(nil)
      else
        callback(vim.fs.root(path, { '.git', 'package.json' }))
      end
    end,
    workspace_required = true,
    on_attach = function(client, bufnr)
      vim.bo[bufnr].formatexpr = nil
      client.server_capabilities.documentFormattingProvider = false
    end,
  }

  lsp.enable({
    'haskell-language-server',
    'phpactor',
    'rust-analyzer',
    'vtsls',
  })
end

local LSP_CONFIG_AUGROUP = api.nvim_create_augroup('MyLspConfig', {})

api.nvim_create_autocmd('BufWinEnter', {
  group = LSP_CONFIG_AUGROUP,
  callback = function(args)
    if #lsp.get_clients({ bufnr = args.buf }) > 0 then
      api.nvim_set_option_value('signcolumn', 'yes', { scope = 'local' })
    end
  end
})

api.nvim_create_autocmd('LspAttach', {
  group = LSP_CONFIG_AUGROUP,
  callback = function(args)
    local client = lsp.get_client_by_id(args.data.client_id)
    if client.name == 'null-ls' then
      return
    end

    local map = function(lhs, rhs)
      vim.keymap.set('n', lhs, rhs, { buffer = args.buf })
    end

    map('K', lsp.buf.hover)
    map(']d', vim.diagnostic.goto_next)
    map('[d', vim.diagnostic.goto_prev)
    map('<LocalLeader>D', lsp.buf.declaration)
    map('<LocalLeader>a', lsp.buf.code_action)
    map('<LocalLeader>d', lsp.buf.definition)
    map('<LocalLeader>f', lsp.buf.format)
    map('<LocalLeader>i', lsp.buf.implementation)
    map('<LocalLeader>n', lsp.buf.rename)
    map('<LocalLeader>r', lsp.buf.references)
    map('<LocalLeader>t', lsp.buf.type_definition)
    map('<LocalLeader><LocalLeader>', function()
      vim.diagnostic.setqflist({ open = false })
      local ok, error = pcall(api.nvim_command, 'cc')
      if not ok then
        api.nvim_err_writeln(error)
      end
    end)

    if api.nvim_win_get_buf(0) == args.buf then
      api.nvim_set_option_value('signcolumn', 'yes', { scope = 'local' })
    end

    if client.server_capabilities.completionProvider then
      vim.bo[args.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
    end

    if client.server_capabilities.definitionProvider then
      vim.bo[args.buf].tagfunc = 'v:lua.vim.lsp.tagfunc'
    end

    api.nvim_clear_autocmds({
      group = LSP_CONFIG_AUGROUP,
      buffer = args.buf,
    })

    api.nvim_create_autocmd('CursorHold', {
      group = LSP_CONFIG_AUGROUP,
      buffer = args.buf,
      callback = function(args)
        for _, winid in pairs(api.nvim_tabpage_list_wins(0)) do
          if api.nvim_win_get_config(winid).zindex then
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

    api.nvim_create_autocmd('BufWritePre', {
      group = LSP_CONFIG_AUGROUP,
      buffer = args.buf,
      callback = function(args)
        lsp.buf.format({
          async = false,
        })
      end,
    })
  end
})

api.nvim_create_autocmd('LspDetach', {
  group = LSP_CONFIG_AUGROUP,
  callback = function(args)
    if not api.nvim_buf_is_loaded(args.buf) then
      return
    end

    local client = lsp.get_client_by_id(args.data.client_id)
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

    if api.nvim_win_get_buf(0) == args.buf then
      api.nvim_set_option_value('signcolumn', vim.o.signcolumn, {
        scope = 'local',
      })
    end

    api.nvim_set_option_value('omnifunc', nil, { buf = args.buf })
    api.nvim_set_option_value('tagfunc', nil, { buf = args.buf })

    api.nvim_clear_autocmds({
      group = LSP_CONFIG_AUGROUP,
      buffer = args.buf,
    })
  end,
})

local function complete_client_names(arg)
  local candidates = {}
  for _, client in ipairs(lsp.get_clients()) do
    if client.name:sub(1, #arg) == arg then
      candidates[client.name] = client.name
    end
  end
  candidates = vim.tbl_values(candidates)
  table.sort(candidates)
  return candidates
end

api.nvim_create_user_command('LspClean', function(args)
    local clients = lsp.get_clients()
    local stop_count = 0

    for i, client in ipairs(clients) do
      if #client.attached_buffers == 0 then
        client.stop()
        stop_count = stop_count + 1
      end
    end

    print(stop_count .. ' client(s) are stopped')
end, {
  desc = 'Stop LSP clients that has no buffers attached',
})

api.nvim_create_user_command('LspInfo', function(info)
  local active_clients = lsp.get_clients()

  for i, client in ipairs(active_clients) do
    print('#' .. i .. ' ' .. client.name)
    if type(client.config.cmd) == 'table' then
      print('  Command: ' .. table.concat(client.config.cmd, ' '))
    end
    if type(client.root_dir) == 'string' then
      print('  Root Directory: ' .. client.root_dir)
    end
    print('  Attached Buffers: ' .. table.concat(lsp.get_buffers_by_client_id(client.id), ', '))
  end

  print(#active_clients .. ' LSP client(s) are running')
end, {
  desc = 'Display informations about running LSP clients',
})

api.nvim_create_user_command(
  'LspRestart',
  function(args)
    local name = args.fargs[1]
    for _, client in ipairs(lsp.get_clients({ name = name })) do
      local attached_bufnrs = lsp.get_buffers_by_client_id(client.id)
      client.stop()
      vim.wait(10000, function()
        return client.is_stopped()
      end)
      local client_id = lsp.start_client(client.config)
      if client_id then
        for _, bufnr in ipairs(attached_bufnrs) do
          lsp.buf_attach_client(bufnr, client_id)
        end
      end
    end
  end,
  {
    desc = 'Manually restart the given language client(s)',
    nargs = 1,
    complete = complete_client_names,
  }
)

api.nvim_create_user_command(
  'LspStop',
  function(args)
    local name = args.fargs[1]
    for _, client in ipairs(lsp.get_clients({ name = name })) do
      client.stop()
    end
  end,
  {
    desc = 'Manually stops the given language client(s)',
    nargs = 1,
    complete = complete_client_names,
  }
)

vim.diagnostic.config({
  float = {
    border = 'rounded',
  },
  severity_sort = true,
  virtual_text = {
    prefix = '*',
    spacing = 2,
  },
})

vim.g.loaded_lsp_config = 1
