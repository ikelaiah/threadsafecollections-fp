# XXHash32 Explained

A plain-English guide to how XXHash32 works, why HashSet and Dictionary use it for string keys,
and what the 4-lane improvement in v0.8.2 actually does.

---

## Why HashSet needs a hash function at all

Imagine you have a **giant warehouse** with 1000 numbered shelves (0 to 999).

You want to store the string `"apple"` and later answer the question *"is apple in here?"* super fast.

**Without hashing** — you'd have to walk every single shelf looking for it. Slow.

**With hashing** — you run `"apple"` through XXHash32, get back some big number like `3,847,291,045`,
do `3847291045 mod 1000 = 45`, and put `"apple"` on shelf 45. Later when someone asks
*"is apple here?"*, you hash it again, get shelf 45, look there — done in one step.

The hash function is just the **address calculator**. It turns a string into a shelf number.

---

## Why XXHash32 specifically for strings?

Strings are variable-length blobs of bytes. You need a function that:

1. **Spreads things out evenly** — you don't want 900 strings all landing on shelf 45
   (that's called a collision)
2. **Is fast** — you call this on every `Add`, `Contains`, `Remove`

XXHash32 was designed specifically for this. `MultiplicativeHash` works great for integers
(one multiplication, done) but would produce terrible distribution on strings because it
ignores all the internal bytes.

---

## XXHash32 is one-way

You cannot go from the hash number back to the original string. That's the point.

```
"apple"  ──→  XXHash32  ──→  3847291045
"apple"  ──→  XXHash32  ──→  3847291045   ← always the same
"apples" ──→  XXHash32  ──→  0192847561   ← completely different

3847291045  ──→  ???  ──→  impossible to recover "apple"
```

The HashSet never needs to go backwards. It only ever asks *"what shelf does this string
belong on?"* — never *"what string lives on this shelf?"*.

The actual string is stored on the shelf alongside the hash, so when you look up shelf 45
you compare the stored string directly to confirm it's really `"apple"` and not a collision
from a different string that happened to land on the same shelf.

---

## Which collections use XXHash32?

Only **Dictionary** and **HashSet** — they are the only collections that need to map a key
to a bucket. List and Deque are ordered collections and never hash anything.

| Collection | Key/value type | Hash function |
|---|---|---|
| Dictionary | `string` | XXHash32 |
| Dictionary | `integer` | MultiplicativeHash |
| Dictionary | other key types | custom hash if supplied, otherwise DefaultHash (FNV-1a on raw bytes) |
| HashSet | `string` | XXHash32 |
| HashSet | `integer` | MultiplicativeHash |
| HashSet | `Boolean` | direct value hash |
| HashSet | `Real` | fixed-point conversion hash |
| HashSet | custom generic `T` | caller-supplied hash function |

---

## How XXHash32 works — the three stages

Think of it like mixing paint. You want to take any string and produce a single 32-bit number
that looks completely random, but is always the same for the same input.

The trick is **avalanche**: changing one character should scramble the entire output. You
achieve this by repeatedly multiplying by large prime numbers and rotating bits — these two
operations interact in a way that spreads a change across all 32 bits very quickly.

### Stage 1 — Consume the input

Read the string in chunks. For each chunk:

1. Multiply by a large prime
2. Rotate the bits left (shift bits off one end, wrap them to the other)
3. Multiply by another prime

After each chunk the accumulator looks completely different from what it was before.

### Stage 2 — Fold in the length

Add the original string length into the accumulator. This ensures `"ab"` and `"abXX"` can
never accidentally produce the same hash even if all other mixing happened to cancel out.

### Stage 3 — Finalisation (avalanche)

Three rounds of XOR-shift and multiply. This is the last scramble that ensures even a
single-bit difference anywhere in the input produces a wildly different output. After this
step, every output bit depends on every input bit.

---

## The two paths — short and long strings

### Short strings (< 16 bytes): `"hi"`

Starting value (the seed): `H32 = PRIME32_5` — just a big magic number, the blank slate.

`"hi"` is 2 bytes — too short for 4-byte chunks, goes straight to the byte loop:

```
Byte 'h' (104):   H32 = H32 + 104 * PRIME32_5
                  H32 = rotate_left(H32, 11)
                  H32 = H32 * PRIME32_1

Byte 'i' (105):   H32 = H32 + 105 * PRIME32_5
                  H32 = rotate_left(H32, 11)
                  H32 = H32 * PRIME32_1
```

Then finalise (avalanche):

```
H32 = H32 xor (H32 >> 15)
H32 = H32 * PRIME32_2
H32 = H32 xor (H32 >> 13)
H32 = H32 * PRIME32_3
H32 = H32 xor (H32 >> 16)
```

One person, two stirs, one final scramble.

---

### Long strings (>= 16 bytes): `"abcdefghijklmnop"` (exactly 16 bytes)

**Initialise four independent accumulators:**

```
V1 = PRIME32_1 + PRIME32_2   ← a big number
V2 = PRIME32_2               ← a different big number
V3 = 0                       ← zero
V4 = 0 - PRIME32_1           ← wraps around (intentional)
```

**One loop iteration — all four lanes consume 4 bytes each:**

```
V1 eats "abcd":   V1 = V1 + [abcd as 32-bit int] * PRIME32_2
                  V1 = rotate_left(V1, 13)
                  V1 = V1 * PRIME32_1

V2 eats "efgh":   V2 = V2 + [efgh as 32-bit int] * PRIME32_2
                  V2 = rotate_left(V2, 13)
                  V2 = V2 * PRIME32_1

V3 eats "ijkl":   V3 = V3 + [ijkl as 32-bit int] * PRIME32_2
                  V3 = rotate_left(V3, 13)
                  V3 = V3 * PRIME32_1

V4 eats "mnop":   V4 = V4 + [mnop as 32-bit int] * PRIME32_2
                  V4 = rotate_left(V4, 13)
                  V4 = V4 * PRIME32_1
```

**Merge the four lanes into one:**

```
H32 = rotate(V1, 1) + rotate(V2, 7) + rotate(V3, 12) + rotate(V4, 18)
```

Each lane gets a different rotation before adding — this prevents two lanes that happened
to produce the same value from cancelling out.

**Fold in the length, then finalise (same avalanche as the short path).**

---

## The 4-lane improvement (v0.8.2)

### The original problem

The original single-lane path is like one person stirring a pot — they have to finish each
stir before starting the next. Modern CPUs can do several multiplications at once, but only
if the multiplications are **independent of each other**. With one accumulator every step
depends on the previous result, so the CPU is forced to wait.

### The fix: four independent accumulators

Using four accumulators (V1, V2, V3, V4), each consuming its own 4-byte chunk, the chains
are independent and the CPU can pipeline all four simultaneously:

```
Iteration 1:  V1 eats bytes  0- 3   V2 eats bytes  4- 7   V3 eats bytes  8-11  V4 eats bytes 12-15
Iteration 2:  V1 eats bytes 16-19   V2 eats bytes 20-23   V3 eats bytes 24-27  V4 eats bytes 28-31
...
```

Four people stirring four pots at the same time, then combining the results at the end.

**Measured result: 19–23% faster on Dictionary key operations at 1M items.**

For strings under 16 bytes there isn't enough data to fill four lanes, so it falls back to
the single-lane path — which is fine because short strings are so cheap the difference
barely registers.

---

## The full picture

```
< 16 bytes                          >= 16 bytes
──────────────────────────────      ──────────────────────────────────────
                                    V1 ═══╗
seed = PRIME32_5                    V2 ═══╬═ 4 lanes eat 16 bytes per pass
                                    V3 ═══╣
                                    V4 ═══╝
        │                                   │
        ▼                                   ▼
  byte / 4-byte loop              merge V1+V2+V3+V4 → H32
  (single lane)                   add length
                                  leftover 4-byte chunks
                                  leftover bytes
        │                                   │
        └───────────────┬───────────────────┘
                        ▼
               finalise (avalanche)
                        │
                        ▼
                    Result
```

The only real difference is that the `>= 16` path runs four mixing chains in parallel before
converging. The finalisation step is identical for both — that's the part that ensures even
a single changed character produces a completely different output number.
