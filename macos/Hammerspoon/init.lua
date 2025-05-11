local BUTTON_MAPPINGS = {
  [2] = 3,
  [3] = 4,
  [4] = 2,
}

local scrolling = false
local scrollSteps = -6
local scrollButton = 3

hooksOtherMouseDown = hs.eventtap.new({ hs.eventtap.event.types.otherMouseDown }, function(e)
  local button = e:getProperty(hs.eventtap.event.properties.mouseEventButtonNumber)
  print('hooksOtherMouseDown', button)
  local mapping = BUTTON_MAPPINGS[button]
  if mapping then
    local location = e:location()
    local event = hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.otherMouseDown, location)
      :setProperty(hs.eventtap.event.properties.mouseEventButtonNumber, mapping)
    return true, { event }
  end
  return false
end)

hooksOtherMouseUp = hs.eventtap.new({ hs.eventtap.event.types.otherMouseUp }, function(e)
  local button = e:getProperty(hs.eventtap.event.properties.mouseEventButtonNumber)
  local mapping = BUTTON_MAPPINGS[button]
  if scrolling and button == scrollButton then
    scrolling = false
    return true
  end
  if mapping then
    local location = e:location()
    local event = hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.otherMouseUp, location)
      :setProperty(hs.eventtap.event.properties.mouseEventButtonNumber, mapping)
    return true, { event }
  end
  return false
end)

hooksOtherMouseDragged = hs.eventtap.new({ hs.eventtap.event.types.otherMouseDragged }, function(e)
  if e:getProperty(hs.eventtap.event.properties.mouseEventButtonNumber) == scrollButton then
    local location = hs.mouse.absolutePosition()
    local deltaX = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaX)
    local deltaY = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaY)
    local event = hs.eventtap.event.newScrollEvent({
      deltaX * scrollSteps,
      deltaY * scrollSteps
    }, {}, 'pixel')
    scrolling = true
    hs.mouse.absolutePosition(location)
    return true, { event }
  end
  return false
end)

hooksOtherMouseDown:start()
hooksOtherMouseUp:start()
hooksOtherMouseDragged:start()
