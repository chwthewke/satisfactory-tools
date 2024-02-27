# Organization methods

## "Tree"

**TODO**

Series of "commands" that serve to move recipes inside a tree structure (initially, all recipes are at the root).

## "Per-item"

Given an item, produced by one or more recipe and consumed by zero or more recipes:

Allow the definition of "target sets", from the list of consuming recipes, by:
- selecting one or more of the consumer recipes
- possibly further splitting such a set in _N_ (an integer) equal parts.

For each target set, allow the selection of producing recipes to fulfill its requirements,
e.g. by prioritizing producing recipes (if heterogenous). These define source sets for the item.
It should be possible to further split source sets in _N_ equal parts like target sets.

If the consuming recipes for the item are part of source set for downstream items, allow these
"downstream source sets" to be selected as targets in the place of consuming recipes.

### Implementation

Ostensibly, `Map[Item, Vector[TargetSet]]` or equivalent.

A `TargetSet` includes:
- a list of `SourceSet` references, for the downstream source sets this target set fulfills.
- a split factor _N_ > 0.
- N source sets (upstream, i.e. recipes producing the item), and for each of these:
  - a list of producing recipes, ordered by priority (what does priority mean, really?)
  - a split factor _M_ > 0
