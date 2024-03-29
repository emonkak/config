local SymbolKind = vim.lsp.protocol.SymbolKind

local global_fold_states = {}

local function is_foldable_symbol(symbol)
  local range = symbol.range
  local line_count = range['end'].line - range.start.line
  if line_count < vim.wo.foldminlines then
    return false
  end
  local kind = symbol.kind
  if kind == SymbolKind.Property
    or kind == SymbolKind.Field
    or kind == SymbolKind.Variable
    or kind == SymbolKind.Constant
    or kind == SymbolKind.EnumMember
  then
    return false
  end
  return true
end

local function calculate_folds(symbols, folds, level)
  if level > vim.wo.foldnestmax then
    return
  end
  for _, symbol in ipairs(symbols) do
    if is_foldable_symbol(symbol) then
      local fold = { symbol = symbol, level = level }
      local start_line = symbol.range.start.line + 1
      local end_line = symbol.range['end'].line + 1
      local next_level
      if folds[start_line] == nil then
        folds[start_line] = fold
        next_level = level + 1
      else
        next_level = level
      end
      if symbol.children then
        calculate_folds(symbol.children, folds, next_level)
      end
      folds[end_line] = fold
    end
  end
end

local function configure_fold_options(bufnr)
  vim.api.nvim_set_option_value('foldmethod', 'expr', { buf = bufnr })
  vim.api.nvim_set_option_value(
    'foldexpr',
    'v:lua.require("lsp_fold").foldexpr(v:lnum)',
    { buf = bufnr }
  )
  vim.api.nvim_set_option_value(
    'foldtext',
    'v:lua.require("lsp_fold").foldtext(v:foldstart, v:foldend, v:folddashes)',
    { buf = bufnr }
  )
end

local function restore_fold_options(bufnr, fold_state)
  vim.api.nvim_set_option_value(
    'foldmethod',
    fold_state.original_foldmethod,
    { buf = bufnr }
  )
  vim.api.nvim_set_option_value(
    'foldexpr',
    fold_state.original_foldexpr,
    { buf = bufnr }
  )
  vim.api.nvim_set_option_value(
    'foldtext',
    fold_state.original_foldtext,
    { buf = bufnr }
  )
end

local function sync_folds(bufnr)
  local original_lazyredraw = vim.go.lazyredraw
  local view = vim.fn.winsaveview()

  vim.go.lazyredraw = true

  -- Reconfigure 'foldmethod', which forces a re-evaluation of 'foldexpr'.
  vim.api.nvim_set_option_value('foldmethod', 'expr', { buf = bufnr })

  -- A fold of the cursor may be closed, so reopen it.
  if vim.fn.foldclosed(view.lnum) >= 0 then
    vim.cmd.foldopen({ bang = true })
  end

  vim.fn.winrestview(view)
  vim.go.lazyredraw = original_lazyredraw
end

local function send_request(bufnr, fold_state)
  local params = {
    textDocument = vim.lsp.util.make_text_document_params(bufnr),
  }
  local callback = function(result)
    if result then
      local folds = {}
      for _, response in pairs(result) do
        if response.result then
          calculate_folds(response.result, folds, 1)
        end
      end
      fold_state.cancel_request = nil
      fold_state.folds = folds
      sync_folds(bufnr)
    end
  end
  if fold_state.cancel_request then
    fold_state.cancel_request()
  end
  fold_state.cancel_request = vim.lsp.buf_request_all(
    bufnr,
    'textDocument/documentSymbol',
    params,
    callback
  )
end

local function new_fold_state(bufnr)
  return {
    folds = {},
    original_foldmethod = vim.api.nvim_get_option_value(
      'foldmethod',
      { buf = bufnr }
    ),
    original_foldexpr = vim.api.nvim_get_option_value(
      'foldexpr',
      { buf = bufnr }
    ),
    original_foldtext = vim.api.nvim_get_option_value(
      'foldtext',
      { buf = bufnr }
    ),
    detached = false,
  }
end

local M = {}

function M.setup(bufnr)
  local fold_state = global_fold_states[bufnr]

  if fold_state then
    fold_state.detached = false
    return
  end

  vim.api.nvim_buf_attach(bufnr, false, {
    on_lines = function(event, bufnr, changedtick)
      local fold_state = global_fold_states[bufnr]
      if fold_state then
        send_request(bufnr, fold_state)
        return fold_state.detached
      end
    end,
    on_reload = function(event, bufnr)
      local fold_state = global_fold_states[bufnr]
      if fold_state then
        send_request(bufnr, fold_state)
        return fold_state.detached
      end
    end,
    on_detach = function(event, bufnr)
      local fold_state = global_fold_states[bufnr]
      if fold_state then
        if fold_state.cancel_request then
          fold_state.cancel_request()
          fold_state.cancel_request = nil
        end
        if vim.api.nvim_buf_is_loaded(bufnr) then
          restore_fold_options(bufnr, fold_state)
        end
        global_fold_states[bufnr] = nil
        return fold_state.detached
      end
    end,
  })

  fold_state = new_fold_state(bufnr)
  global_fold_states[bufnr] = fold_state

  configure_fold_options(bufnr)
  send_request(bufnr, fold_state)
end

function M.restore(bufnr)
  local fold_state = global_fold_states[bufnr]
  if fold_state then
    fold_state.detached = true
  end
end

function M.foldexpr(lnum)
  local bufnr = vim.api.nvim_get_current_buf()
  local fold_state = global_fold_states[bufnr]
  if fold_state == nil then
    return -1
  end
  local fold = fold_state.folds[lnum]
  if fold == nil then
    return '='
  elseif fold.symbol.range.start.line + 1 == lnum then
    return '>' .. fold.level
  else
    return '<' .. fold.level
  end
end

function M.foldtext(fold_start, fold_end, fold_dashes)
  local bufnr = vim.api.nvim_get_current_buf()
  local fold_state = global_fold_states[bufnr]
  if fold_state == nil then
    return ''
  end
  local fold = fold_state.folds[fold_start]
  if fold == nil then
    return ''
  end
  return string.format(
    '+-%s%3d lines: %s [%s]',
    fold_dashes,
    fold.symbol.range['end'].line - fold.symbol.range.start.line,
    fold.symbol.name,
    SymbolKind[fold.symbol.kind] or '?'
  )
end

function M.dump_fold_state(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  print(vim.inspect(global_fold_states[bufnr]))
end

return M
