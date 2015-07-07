classes = { 
  { name = "player", components = { "draw.lua" } },
  { name = "block", components = { "draw.lua" } }
};

tree = {
  { parent = -1, components = { { script = "draw.lua", env = {x = 15, y = 850} } } },
  { parent = -1, components = { { script = "draw.lua", env = {y = 908, x = 45} } } }
};

cool = {
  {x = 15, y = 850, sprite = "player.png"},
  {y = 908, x = 45, sprite = "five"}
};
