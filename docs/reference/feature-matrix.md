# Feature Matrix

A quick comparison of the four collections. Declaration-level detail lives in
the [API Overview](api-overview.md) and the generated
[Cheat Sheet](../start/cheat-sheet.md); behaviour lives in the
[collection guides](../guides/list.md).

| Feature | List | Deque | Dictionary | Hash Set |
|---|---|---|---|---|
| Element type | one (`T`) | one (`T`) | key + value | one (`T`, unique) |
| Required configuration | comparer | none | none (custom hash for other keys) | equality + hash |
| Constructor | `Create(Comparer)` | `Create` / `Create(Capacity)` | `Create` / `Create(Capacity)` / `Create(Hash, Equals)` | `Create(Equality, Hash)` or specialized |
| Indexed access | `Items[I]`, default | no | `Items[Key]` | no |
| Order guarantee | insertion order retained | positional (front/back) | none (hash order) | none (hash order) |
| Random access | O(1) | O(1) to an end | O(1) average by key | membership only |
| Sorting / searching | yes, comparer-driven | no | yes for keys | membership / set ops |
| Iteration model | lock-holding | lock-holding | snapshot | lock-holding |
| Bulk add/remove | `AddRange`, `InsertRange`, `DeleteRange` | `PushRangeFront/Back` | `AddRange` (dict/array) | `AddRange`, `RemoveRange` |
| Set algebra | — | — | — | `IntersectWith`, `UnionWith`, `ExceptWith`, `Overlaps`, `SetEquals` |
| `Try*` variants | no | `TryPopFront/Back`, `TryPeekFront/Back` | `TryGetValue`, `TryAdd` | `TryGetValue` |
| Capacity control | `Capacity`, `TrimExcess` | initial capacity | `TrimExcess` | `TrimExcess` |
| Array conversion | `ToArray`, `FromArray` | `ToArray`, `CopyTo` | `ToArray` (pairs), `GetKeys`, `GetValues` | `ToArray` |
| Specialized classes | — | — | — | Integer, String, Boolean, Real |
| Interface form | `IThreadSafeList<T>` | `IThreadSafeDeque<T>` | `IThreadSafeDictionary<TKey, TValue>` | `IThreadSafeHashSet<T>` |

## Shared behaviour across all collections

- One `TCriticalSection` per instance; each public method synchronizes.
- `Count` in O(1); `IsEmpty`; `Clear`; `Lock` returning an `ILockToken`.
- No automatic freeing of object/interface elements.
- Manual lock tokens must not be combined with public method calls on the same
  instance (see [Lock Tokens](../guides/lock-tokens.md)).

## Iteration model by collection

| Collection | Enumerator holds lock | Snapshot | Notes |
|---|---|---|---|
| List | yes | no | keep loop bodies short |
| Deque | yes | no | |
| Hash set | yes | no | |
| Dictionary | no | yes | snapshot of pairs at iteration start |

## Choosing a collection

- Need indexed access, sorting, or binary search? **List**.
- Producer/consumer queue or stack on both ends? **Deque**.
- Key/value lookup with custom keys? **Dictionary**.
- Uniqueness, membership, and set operations? **Hash set**.
- Compound operations that must be atomic together at the call site? Combine
  methods while holding a **Lock token**.

## Examples mapping

| Example | Collections used |
|---|---|
| `SimpleNumberList` | List |
| `SimpleDeque` | Deque |
| `SimpleHashSet`, `HashSetClientDemo` | Hash set |
| `DictionaryIterator`, `DictionaryIteratorRecord` | Dictionary |
| `DictionaryWithCustomType` | Dictionary with custom key |
| `SimpleShoppingCart`, `SimpleToDoList` | List with custom types |
| `ChatMessageQueue` | Deque in threads |
| `ListIterate`, `HashSetIterate`, `HashSetIterateRecord` | enumeration |
| `InterfaceTest` | interface forms and lock tokens |
| `Benchmark` | performance measurement |