local scrolling = false
local scrollSteps = -6
local scrollButton = 3

local leftButtonPressed = false
local rightButtonPressed = false
local middleButtonEmulated = false

local buttonMappings = {
  [2] = 3,
  [3] = 4,
}

local function emitMiddleClick()
  local pos = hs.mouse.absolutePosition()
  local eventDown = hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.otherMouseDown, pos, {})
    :setProperty(hs.eventtap.event.properties.mouseEventButtonNumber, 2)
  local eventUp = hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.otherMouseUp, pos, {})
    :setProperty(hs.eventtap.event.properties.mouseEventButtonNumber, 2)
  return true, {eventDown,eventUp}
end

hooksLeftMouseDown = hs.eventtap.new({ hs.eventtap.event.types.leftMouseDown }, function(e)
  local pos = hs.mouse.absolutePosition()
  leftButtonPressed = true
end)

hooksRightMouseDown = hs.eventtap.new({ hs.eventtap.event.types.rightMouseDown }, function(e)
  rightButtonPressed = true
  return false
end)

hooksLeftMouseUp = hs.eventtap.new({ hs.eventtap.event.types.leftMouseUp }, function(e)
  leftButtonPressed = false
  if middleButtonEmulated then
    middleButtonEmulated = false
    return true
  end
  if rightButtonPressed then
    rightButtonPressed = false
    middleButtonEmulated = true
    return emitMiddleClick()
  end
  return false
end)

hooksRightMouseUp = hs.eventtap.new({hs.eventtap.event.types.rightMouseUp}, function(e)
  rightButtonPressed = false
  if middleButtonEmulated then
    middleButtonEmulated = false
    return true
  end
  if leftButtonPressed then
    leftButtonPressed = false
    middleButtonEmulated = true
    return emitMiddleClick()
  end
  return false
end)

hooksMouseDown = hs.eventtap.new({hs.eventtap.event.types.otherMouseDown}, function(e)
  local button = e:getProperty(hs.eventtap.event.properties.mouseEventButtonNumber)
  local mapping = buttonMappings[button]
  if (mapping) then
    local pos = hs.mouse.absolutePosition()
    local event = hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.otherMouseDown, pos)
      :setProperty(hs.eventtap.event.properties.mouseEventButtonNumber, mapping)
    return true, {event}
  end
  return false
end)

hooksMouseUp = hs.eventtap.new({hs.eventtap.event.types.otherMouseUp}, function(e)
  local button = e:getProperty(hs.eventtap.event.properties.mouseEventButtonNumber)
  local mapping = buttonMappings[button]
  if (scrolling and button == scrollButton) then
    scrolling = false
    return true
  end
  if (mapping) then
    local pos = hs.mouse.absolutePosition()
    local event = hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.otherMouseUp, pos)
      :setProperty(hs.eventtap.event.properties.mouseEventButtonNumber, mapping)
    return true, {event}
  end
  return false
end)

hooksMouseDragged = hs.eventtap.new({hs.eventtap.event.types.otherMouseDragged}, function(e)
  if (e:getProperty(hs.eventtap.event.properties.mouseEventButtonNumber) == scrollButton) then
    local pos = hs.mouse.absolutePosition()
    local deltaX = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaX)
    local deltaY = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaY)
    local event = hs.eventtap.event.newScrollEvent({
      deltaX * scrollSteps,
      deltaY * scrollSteps
    }, {}, 'pixel')
    scrolling = true
    hs.mouse.absolutePosition(pos)
    return true, {event}
  end
  return false
end)

hooksLeftMouseDown:start()
hooksRightMouseDown:start()
hooksLeftMouseUp:start()
hooksRightMouseUp:start()
hooksMouseDown:start()
hooksMouseUp:start()
hooksMouseDragged:start()
