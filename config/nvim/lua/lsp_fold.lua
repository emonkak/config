local SymbolKind = vim.lsp.protocol.SymbolKind

local fold_states = {}

local function is_foldable_symbol(symbol)
  local range = symbol.range
  if range.start.line >= range['end'].line then
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
  for _, symbol in ipairs(symbols) do
    if is_foldable_symbol(symbol) then
      local fold = {
        symbol = {
          name = symbol.name,
          detail = symbol.detail,
          kind = symbol.kind,
          tags = symbol.tags,
          deprecated = symbol.deprecated,
          range = symbol.range,
          selectionRange = symbol.selectionRange,
        },
        level = level,
      }
      folds[symbol.range.start.line + 1] = fold
      folds[symbol.range['end'].line + 1] = fold
      if symbol.children ~= nil then
        calculate_folds(symbol.children, folds, level + 1)
      end
    end
  end
end

local function sync_folds(bufnr)
  vim.api.nvim_set_option_value('foldmethod', 'expr', { buf = bufnr })
end

local function update_folds(bufnr, changedtick, fold_state)
  local params = {
    textDocument = vim.lsp.util.make_text_document_params(bufnr)
  }
  local callback = function(responses)
    if responses ~= nil and fold_state.changedtick < changedtick then
      local folds = {}
      for _, response in ipairs(responses) do
        if response.result ~= nil then
          calculate_folds(response.result, folds, 1)
        end
      end
      fold_state.changedtick = changedtick
      fold_state.folds = folds
      sync_folds()
    end
  end
  vim.lsp.buf_request_all(
    bufnr,
    'textDocument/documentSymbol',
    params,
    callback
  )
end

local M = {}

function M.setup(bufnr)
  local fold_state = { folds = {}, changedtick = -1 }
  fold_states[bufnr] = fold_state

  vim.api.nvim_buf_attach(bufnr, false, {
    on_lines = function(event, bufnr, changedtick)
      update_folds(bufnr, changedtick, fold_state)
    end,
    on_detach = function(event, bufnr)
      fold_states[bufnr] = nil
    end,
  })

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

  local changedtick = vim.api.nvim_buf_get_changedtick(bufnr)
  update_folds(bufnr, changedtick, fold_state)
end

function M.foldexpr(lnum)
  local bufnr = vim.api.nvim_get_current_buf()
  local fold_state = fold_states[bufnr]
  if fold_state == nil then
    return '='
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
  local fold_state = fold_states[bufnr]
  if fold_state == nil then
    return ''
  end
  local fold = fold_state.folds[fold_start]
  if fold == nil then
    return ''
  end
  return string.format(
    '+%s %2d lines: %s [%s]',
    fold_dashes,
    fold.symbol.range['end'].line - fold.symbol.range.start.line,
    fold.symbol.name,
    SymbolKind[fold.symbol.kind] or 'Unknown'
  )
end

return M
