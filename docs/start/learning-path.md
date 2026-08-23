# Learning Path

Work through the examples in the order below. Each step builds on the previous
one and introduces one or two new concepts. All tracked examples and their build
status are listed in [Building & Verification](../project/building.md#building-all-examples).

1. [SimpleNumberList](../../examples/SimpleNumberList/SimpleNumberList.lpr) —
   generic specialization, a comparer, indexed access, and sorting.
2. [SimpleDeque](../../examples/SimpleDeque/SimpleDeque.lpr) — front/back queue
   operations and `TryPop`.
3. [SimpleHashSet](../../examples/SimpleHashSet/SimpleHashSet.lpr) — uniqueness,
   membership, removal, and specialized set types.
4. [DictionaryIterator](../../examples/DictionaryIterator/DictionaryIterator.lpr) —
   snapshot iteration with `Generics.Collections.TPair`.
5. [SimpleShoppingCart](../../examples/SimpleShoppingCart/SimpleShoppingCart.lpr) —
   records and a custom comparer.
6. [ChatMessageQueue](../../examples/ChatMessageQueue/ChatMessageQueue.lpr) —
   a multi-threaded queue demonstration; stop it with Ctrl+C.

## What each collection guide adds

- [List](../guides/list.md) — indexing, `Sort`, binary search, ranges.
- [Deque](../guides/deque.md) — `Push*`/`Pop*`/`Peek*`, `Try*` variants,
  bulk ordering.
- [Dictionary](../guides/dictionary.md) — custom hash/equality, snapshots,
  `TPair` iteration.
- [Hash set](../guides/hash-set.md) — set algebra, specialized classes,
  custom types.

## Reading about the important ideas

- [Lock tokens (RAII)](../guides/lock-tokens.md) — how locking works and when to
  use it manually.
- [Thread-safety, iteration, and lock policy](../guides/thread-safety-and-iteration.md) —
  what is and is not atomic.
- [Hashing](../guides/hashing.md) — hash selection for strings, integers, reals,
  and custom keys.
- [Recipes](../guides/recipes.md) — compiled programs for practical tasks.

## When you know the basics

- [Cheat Sheet](cheat-sheet.md) — compact API reminder.
- [API Overview](../reference/api-overview.md) — the shape of the public API.
- [Configuration](../reference/config.md) — comparers, hash functions, and
  capacity choices.
- [Contracts & Limitations](../reference/contracts-and-limitations.md) — the
  exact guarantees.