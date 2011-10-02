function icon(path)
  return '^i($HOME/.dzen/' .. path .. ')'
end

function color(text, fg, bg)
  local head = ''
  local tail = ''

  if fg ~= '' then
    head = '^fg(\\' .. fg .. ')'
    tail = '^fg()'
  end
  if bg ~= '' then
    head = head .. '^bg(\\' .. bg .. ')'
    tail = '^bg()' .. tail
  end

  return head .. text .. tail
end

function hgraph(usage, graph)
  local text = ''
  local fg_width = graph.width * (usage / 100)
  local bg_width = graph.width - fg_width

  text = text .. color(string.format('^r(%dx%d)', fg_width, graph.height),
                 graph.fg, '')
  text = text .. color(string.format('^r(%dx%d)', bg_width, graph.height),
                 graph.bg, '')

  return text
end

function hsgraph(usage, graph)
  local fg_width = graph.width * (usage / 100)
  local step = graph.sw + graph.ss
  local fg, bg, i = '', '', 0

  while i < fg_width do
    fg = fg .. string.format('^r(%dx%d+%d+0)', graph.sw, graph.height, graph.ss)
    i = i + step
  end

  while i < graph.width do
    bg = bg .. string.format('^r(%dx%d+%d+0)', graph.sw, graph.height, graph.ss)
    i = i + step
  end

  fg = color(fg, '#7f9f7f', '')
  bg = color(bg, '#666666', '')

  return fg .. bg
end

function conky_dzen_cpu_graph(width)
  local usage = tonumber(conky_parse('${cpu}')) or 0
  local temp1 = tonumber(conky_parse('${hwmon 0/device temp 2}')) or 0
  local temp2 = tonumber(conky_parse('${hwmon 0/device temp 4}')) or 0
  local icon = color(icon('xbm8x8/cpu.xbm'), '#7f9f7f', '')
  local graph = hsgraph(usage, {
    width = tonumber(width),
    height = 4,
    sw = 4,
    ss = 1,
    fg = '#7f9f7f',
    bg = '#666666',
  })
  return string.format('%s %3d%% %s %2d/%2dC',
                       icon, usage, graph, temp1, temp2)
end

function conky_dzen_mem_graph(width)
  local usage = tonumber(conky_parse('${memperc}')) or 0
  local icon = color(icon('xbm8x8/mem.xbm'), '#7f9f7f', '')
  local graph = hsgraph(usage, {
    width = tonumber(width),
    height = 4,
    sw = 4,
    ss = 1,
    fg = '#7f9f7f',
    bg = '#666666',
  })
  return string.format('%s %3d%% %s', icon, usage, graph)
end

function conky_dzen_net_monitor(interface)
  local up = tonumber(conky_parse(string.format('${upspeedf %s}', interface))) or 0
  local down = tonumber(conky_parse(string.format('${downspeedf %s}', interface))) or 0
  return string.format('%s: ^fg(\\#cc9393)%4dkb/s %s^fg() ^fg(\\#7f9f7f)%4dkb/s %s^fg()',
                       interface,
                       up,
                       icon('xbm8x8/up.xbm'),
                       down,
                       icon('xbm8x8/down.xbm'))
end

function conky_dzen_mixer()
  local usage = tonumber(conky_parse('${mixer}')) or 0
  return string.format('${if_mixer_mute}%s${else}%s${endif} %3d%%',
                       color(icon('xbm8x8/spkr_02.xbm'), '#cc9393', ''),
                       color(icon('xbm8x8/spkr_01.xbm'), '#7f9f7f', ''),
                       usage)
end
