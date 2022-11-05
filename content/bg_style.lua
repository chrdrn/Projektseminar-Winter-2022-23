function Header(el)
    if el.level == 1 then
      table.insert(el.classes, "inverse")
      el.attributes["data-background-color"] = '#971B2F'
      return el
    end
end

function Header(el)
  if el.level == 1 then
    table.insert(el.classes, "inverse")
    el.attributes["data-background-color"] = '#971B2F'
    return el
  end
end