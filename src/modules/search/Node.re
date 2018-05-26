type node = {
  parent: node,
  actions: list(string),
  pathCost: int,
  depth: int,
};