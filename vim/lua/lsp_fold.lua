local TYPE_START = 1
local TYPE_END = 2

local SymbolKind = vim.lsp.protocol.SymbolKind

local augroup = vim.api.nvim_create_augroup('LSPFold', {})

local fold_states = {}

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

local function calculate_folds(folds, symbols, level, max_level)
  for _, symbol in pairs(symbols) do
    if is_foldable_symbol(symbol) then
      local start_line = symbol.range.start.line + 1
      local end_line = symbol.range['end'].line + 1
      if folds[start_line] == nil then
        folds[start_line] = {
          type = TYPE_START,
          level = level,
          symbol = symbol,
        }
        next_level = level + 1
      else
        next_level = level
      end
      if symbol.children and next_level <= max_level then
        calculate_folds(folds, symbol.children, next_level, max_level)
      end
      folds[end_line] = { type = TYPE_END, level = level, symbol = symbol }
    end
  end
end

local function configure_fold_options(bufnr)
  vim.api.nvim_set_option_value('foldmethod', 'expr', { scope = 'local' })
  vim.api.nvim_set_option_value(
    'foldexpr',
    'v:lua.require("lsp_fold").foldexpr(v:lnum)',
    { scope = 'local' }
  )
  vim.api.nvim_set_option_value(
    'foldtext',
    'v:lua.require("lsp_fold").foldtext(v:foldstart, v:foldend, v:folddashes)',
    { scope = 'local' }
  )
end

local function update_fold(bufnr, top, bottom)
  for _, win in ipairs(vim.fn.win_findbuf(bufnr)) do
    if vim.wo[win].foldmethod == 'expr' then
      vim._foldupdate(win, top, bottom)
    end
  end
end

local function request_update_fold(bufnr)
  if vim.api.nvim_get_mode().mode:match('^i') then
    if #(vim.api.nvim_get_autocmds({ group = group, buffer = bufnr })) > 0 then
      return
    end
    vim.api.nvim_create_autocmd('InsertLeave', {
      group = augroup,
      buffer = bufnr,
      once = true,
      callback = function()
        update_fold(bufnr, 0, vim.api.nvim_buf_line_count(bufnr))
      end,
    })
  else
    vim.schedule(function()
      if vim.api.nvim_buf_is_loaded(bufnr) then
        update_fold(bufnr, 0, vim.api.nvim_buf_line_count(bufnr))
      end
    end)
  end
end

local function restore_fold_options(bufnr, state)
  vim.api.nvim_set_option_value(
    'foldmethod',
    state.original_foldmethod,
    { scope = 'local' }
  )
  vim.api.nvim_set_option_value(
    'foldexpr',
    state.original_foldexpr,
    { scope = 'local' }
  )
  vim.api.nvim_set_option_value(
    'foldtext',
    state.original_foldtext,
    { scope = 'local' }
  )
end

local function get_document_symboles(bufnr, state, changedtick)
  local params = {
    textDocument = vim.lsp.util.make_text_document_params(bufnr),
  }
  local callback = function(responses)
    state.request = nil
    if not responses or state.version ~= changedtick then
      return
    end
    local new_folds = {}
    local max_level = vim.wo.foldnestmax
    for _, response in pairs(responses) do
      if response.result then
        calculate_folds(new_folds, response.result, 1, max_level)
      end
    end
    request_update_fold(bufnr, update_range)
    state.folds = new_folds
  end
  if state.request then
    state.request()
  end
  state.request = vim.lsp.buf_request_all(
    bufnr,
    'textDocument/documentSymbol',
    params,
    callback
  )
  state.version = changedtick
end

local function new_state(bufnr)
  return {
    folds = {},
    levels = {},
    original_foldmethod = vim.api.nvim_get_option_value(
      'foldmethod',
      { scope = 'local' }
    ),
    original_foldexpr = vim.api.nvim_get_option_value(
      'foldexpr',
      { scope = 'local' }
    ),
    original_foldtext = vim.api.nvim_get_option_value(
      'foldtext',
      { scope = 'local' }
    ),
    detached = false,
    version = 0,
  }
end

local function trim(text)
  return text:gsub('^%s+', ''):gsub('%s+$', '')
end

local M = {}

function M.attach(bufnr)
  local state = fold_states[bufnr]

  if state then
    -- This buffer has already been setup. So reuse the state that already
    -- exists and abort detaching.
    state.detached = false
    return
  end

  vim.api.nvim_buf_attach(bufnr, false, {
    on_lines = function(event, bufnr, changedtick)
      local state = fold_states[bufnr]
      if state then
        get_document_symboles(bufnr, state, changedtick)
        return state.detached
      end
    end,
    on_reload = function(event, bufnr)
      local state = fold_states[bufnr]
      if state then
        local changedtick = vim.api.nvim_buf_get_changedtick(bufnr)
        get_document_symboles(bufnr, state, changedtick)
        return state.detached
      end
    end,
    on_detach = function(event, bufnr)
      local state = fold_states[bufnr]
      if state then
        if state.request then
          state.request()
          state.request = nil
        end
        if vim.api.nvim_buf_is_loaded(bufnr) then
          restore_fold_options(bufnr, state)
        end
        fold_states[bufnr] = nil
      end
    end,
  })

  state = new_state(bufnr)
  fold_states[bufnr] = state

  local changedtick = vim.api.nvim_buf_get_changedtick(bufnr)
  configure_fold_options(bufnr)
  get_document_symboles(bufnr, state, changedtick)
end

function M.detach(bufnr)
  local state = fold_states[bufnr]
  if state then
    -- vim.api.nvim_buf_detach() is only available with RPC. Instead, detach
    -- from callbacks.
    state.detached = true
  end
end

function M.foldexpr(lnum)
  local bufnr = vim.api.nvim_get_current_buf()
  local state = fold_states[bufnr]
  if state == nil then
    return -1
  end
  local fold = state.folds[lnum]
  if fold == nil then
    return state.levels[lnum] or '='
  elseif fold.type == TYPE_START then
    return '>' .. fold.level
  else
    return '<' .. fold.level
  end
end

function M.foldtext(fold_start, fold_end, fold_dashes)
  local bufnr = vim.api.nvim_get_current_buf()
  local state = fold_states[bufnr]
  if state == nil then
    return ''
  end
  local fold = state.folds[fold_start]
  if fold == nil then
    return ''
  end
  local symbol = fold.symbol
  local line = vim.api.nvim_buf_get_lines(
    bufnr,
    symbol.selectionRange.start.line,
    symbol.selectionRange.start.line + 1,
    false
  )[1] or ''
  return string.format(
    '+-%s%3d lines: %s',
    fold_dashes,
    symbol.range['end'].line - symbol.range.start.line,
    trim(line)
  )
end

function M.get_state(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  return fold_states[bufnr]
end

return M
