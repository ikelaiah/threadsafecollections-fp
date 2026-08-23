# Configuration

Each collection makes a few construction-time choices that affect behaviour and
memory. This page summarizes them; each [collection guide](../guides/list.md)
shows the concrete declarations and examples.

## List: comparer (required)

`TThreadSafeList<T>` requires a comparer at construction because sorting and
searching depend on an element ordering.

```pascal
var
  Numbers: specialize TThreadSafeList<Integer>;
begin
  Numbers := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
end;
```

Built-in comparers cover `Integer`, `string`, `Boolean`, and `Real`. For other
element types, pass a `specialize TComparer<T>` function with the same
signature.

## Deque: capacity

`TThreadSafeDeque<T>` needs no configuration. The optional constructor
`Create(AInitialCapacity)` rounds the request up to a power of two (minimum 4)
and preallocates the circular buffer. Growth doubles the buffer when it fills.

## Dictionary: hash and equality

The default constructor works for `string` and `Integer` keys. The dictionary
dispatches on the key type:

| Key kind | Hash used |
|---|---|
| `string` | `XXHash32` |
| `Integer` | `MultiplicativeHash` |
| anything else | `DefaultHash` (FNV-1a over raw key bytes) |

For custom or compound keys, provide both callbacks:

```pascal
constructor Create(const AHashFunc: specialize THashFunction<TKey>;
                   const AEqualityComparison: specialize TEqualityComparison<TKey>);
```

See the [Dictionary guide](../guides/dictionary.md#construction) for a complete
record-key example. The optional `InitialCapacity` argument preallocates the
bucket array (rounded up to a power of two).

## Hash set: equality and hash

The generic `TThreadSafeHashSet<T>` requires both an equality comparer and a
hash function:

```pascal
constructor Create(AEqualityComparer: specialize THashSetEqualityComparer<T>;
                   AHashFunction: specialize THashFunction<T>;
                   AInitialCapacity: Integer);
```

For common element types, prefer the specialized subclasses, which wire the
matching hash/equality helpers automatically:

| Class | Element | String hash |
|---|---|---|
| `TThreadSafeHashSetInteger` | `Integer` | `MultiplicativeHash` |
| `TThreadSafeHashSetString` | `string` | `XXHash32` |
| `TThreadSafeHashSetBoolean` | `Boolean` | — |
| `TThreadSafeHashSetReal` | `Real` | fixed-point `RealHash` |

`TThreadSafeHashSetString` also accepts an alternate hash function, e.g.
`@FNV1aHash`.

## Capacity and growth

- List and deque start at a small default capacity and grow on demand; the list
  uses two growth factors (`GROWTH_FACTOR_DOUBLE` for small lists,
  `GROWTH_FACTOR_LARGE_NUMERATOR`/`.../DENOMINATOR` for large ones).
- Dictionary and hash set start with `INITIAL_BUCKET_COUNT` buckets, resize at
  `LOAD_FACTOR`, round bucket counts to powers of two, and never shrink
  automatically (`TrimExcess` releases spare memory).
- `SetCapacity`/`Create(InitialCapacity)` cannot shrink below `Count`.

## Error handling

Operations that cannot complete raise exceptions whose messages come from
`ThreadSafeCollections.ErrorMessages` (for example `ERR_ITEM_NOT_FOUND`,
`ERR_DUPLICATE_KEY`, `ERR_COMPARER_REQUIRED`). Prefer `Try*` variants
(`TryPopFront`, `TryGetValue`, `TryAdd`) where you want to branch instead of
catch.

## Equality semantics

- List membership operations (`Contains`, `IndexOf`, `IndexOfItem`, `Extract`)
  use the comparer supplied at construction; `Compare = 0` is treated as equal.
  Keep the comparer's notion of equality consistent with your ordering intent.
- Dictionary `ContainsValue` uses the RTL's default equality comparison for the
  value type.
- Hash-set equality is whatever `THashSetEqualityComparer<T>` you supply;
  consistency with the hash function matters: two equal values must hash the
  same when both are present.