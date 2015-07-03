classes = { 
  { name = "player", components = { "player.lua" } },
  { name = "block", components = { "block.lua" } }
};

tree = { 
  { 
    parent = null, -- attach to the head node or to another object
    object_id = 0, -- base object to derive, from above
    components = {
      { x = 10, y = 10 }, -- corresponds to position in components above
    }
  } 
}
