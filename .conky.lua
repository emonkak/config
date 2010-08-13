function conky_dzen_icon(icon)
  return string.format('^i($HOME/.dzen/%s)', icon)
end

function conky_dzen_cpu_graph(width)
  local percent = tonumber(conky_parse('${cpu}')) or 0
  local max     = tonumber(width)
  local used    = max * (percent / 100)
  local title   = '^fg(\\#666666)cpu:^fg()'
  return string.format('%s %3d%% ^r(%dx4)^fg(\\#666666)^r(%dx4)^fg()',
                       title,
                       percent,
                       used,
                       max - used)
end

function conky_dzen_mem_graph(width)
  local percent = tonumber(conky_parse('${memperc}')) or 0
  local max     = tonumber(width)
  local used    = max * (percent / 100)
  local title   = '^fg(\\#666666)mem:^fg()'
  return string.format('%s %3d%% ^r(%dx4)^fg(\\#666666)^r(%dx4)^fg()',
                       title,
                       percent,
                       used,
                       max - used)
end

function conky_dzen_net_monitor(interface)
  local up    = tonumber(conky_parse(string.format('${upspeedf %s}', interface))) or 0
  local down  = tonumber(conky_parse(string.format('${downspeedf %s}', interface))) or 0
  local title = string.format('^fg(\\#666666)%s:^fg()', interface)
  return string.format('%s %s %4dkb/s %s %4dkb/s',
                       title,
                       conky_dzen_icon('arrow_up.xbm'),
                       up,
                       conky_dzen_icon('arrow_down.xbm'),
                       down)
end

function conky_dzen_mixer()
  local percent = tonumber(conky_parse('${mixer}')) or 0
  return string.format('${if_mixer_mute}%s${else}%s${endif} %3d%%',
                       conky_dzen_icon('spkr_mute.xbm'),
                       conky_dzen_icon('spkr_hi.xbm'),
                       percent)
end
