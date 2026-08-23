# Recipes

Every Pascal program below is compiled with FPC 3.2.2 and run against its
stated output by `tools/test_docs_examples.py`. The code blocks are checked
against the linked source files by `tools/check_docs.py`, so they cannot drift
from the programs that are actually built and run.

## Keep a list sorted and find an item

**Problem:** Maintain a small number list, sort it, then look an item up.

**Recommended API:** `TThreadSafeList<Integer>`, `AddRange`, `Sort`,
`IndexOf`.

```pascal
program KeepListSorted;

{$mode objfpc}{$H+}

uses
  ThreadSafeCollections.List;

var
  Numbers: specialize TThreadSafeList<Integer>;
  Number: Integer;
begin
  Numbers := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    Numbers.AddRange([30, 10, 20]);
    Numbers.Sort;

    for Number in Numbers do
      Writeln(Number);

    if Numbers.IndexOf(20) >= 0 then
      Writeln('found');
  finally
    Numbers.Free;
  end;
end.
```

**Expected output:**

```text
10
20
30
found
```

**Caveat:** Searches are binary after `Sort` (O(log n)) and linear otherwise;
`IndexOf` uses the comparer and treats `Compare = 0` as equal.
[Source program](../../examples/documentation/01_keep_a_list_sorted.pas).

## Drain tasks front-to-back with a deque

**Problem:** Hand a small FIFO work queue, taking one task at a time without
raising when it is empty.

**Recommended API:** `PushBack`, `TryPopFront`, `TryPeekFront`.

```pascal
program DequeWorkQueue;

{$mode objfpc}{$H+}

uses
  SysUtils,
  ThreadSafeCollections.Deque;

var
  Tasks: specialize TThreadSafeDeque<string>;
  Task: string;
begin
  Tasks := specialize TThreadSafeDeque<string>.Create;
  try
    Tasks.PushBack('build');
    Tasks.PushBack('test');
    Tasks.PushBack('ship');

    if Tasks.TryPopFront(Task) then
      Writeln(Task);
    if Tasks.TryPeekFront(Task) then
      Writeln('next: ' + Task);
    Writeln('remaining: ' + IntToStr(Tasks.Count));
  finally
    Tasks.Free;
  end;
end.
```

**Expected output:**

```text
build
next: test
remaining: 2
```

**Caveat:** `TryPopFront` returns `False` on an empty deque instead of raising;
`TryPeekFront` looks without removing.
[Source program](../../examples/documentation/02_deque_work_queue.pas).

## Add-or-update a dictionary value

**Problem:** Keep a small inventory by name, updating a value that may already
exist, then inspect one key.

**Recommended API:** `Add`, `AddOrSetValue`, snapshot `for..in` with
`TPair`.

```pascal
program DictionaryUpdateSnapshot;

{$mode objfpc}{$H+}

uses
  Generics.Collections,
  ThreadSafeCollections.Dictionary;

var
  Stock: specialize TThreadSafeDictionary<string, integer>;
  Pair: specialize TPair<string, integer>;
begin
  Stock := specialize TThreadSafeDictionary<string, integer>.Create;
  try
    Stock.Add('apples', 3);
    Stock.Add('pears', 5);
    Stock.AddOrSetValue('apples', 4);

    for Pair in Stock do
      if Pair.Key = 'apples' then
        Writeln(Pair.Key, '=', Pair.Value);
  finally
    Stock.Free;
  end;
end.
```

**Expected output:**

```text
apples=4
```

**Caveat:** Dictionary iteration is a snapshot: the pairs present when the loop
starts are the pairs you see. Hash order is unspecified, so query by key instead
of relying on order.
[Source program](../../examples/documentation/03_dictionary_update_snapshot.pas).

## Intersect two integer sets

**Problem:** Keep the values common to two unique-value sets.

**Recommended API:** `TThreadSafeHashSetInteger`, `AddRange`, `IntersectWith`,
`Contains`.

```pascal
program HashSetAlgebra;

{$mode objfpc}{$H+}

uses
  ThreadSafeCollections.HashSet;

var
  A, B: TThreadSafeHashSetInteger;
begin
  A := TThreadSafeHashSetInteger.Create;
  B := TThreadSafeHashSetInteger.Create;
  try
    A.AddRange([1, 2, 3, 4]);
    B.AddRange([3, 4, 5]);
    A.IntersectWith(B);

    Writeln('count ', A.Count);
    if A.Contains(3) then
      Writeln('3 yes');
    if A.Contains(4) then
      Writeln('4 yes');
    if A.Contains(1) then
      Writeln('1 yes');
  finally
    A.Free;
    B.Free;
  end;
end.
```

**Expected output:**

```text
count 2
3 yes
4 yes
```

**Caveat:** `IntersectWith` mutates `A` in place. Sets have no order, so test
membership instead of printing in iteration order.
[Source program](../../examples/documentation/04_hash_set_algebra.pas).

## Total a shopping cart of records

**Problem:** Store custom records in a list and compute a total while iterating.

**Recommended API:** `TThreadSafeList<TItem>` with a custom comparer, `Add`,
`for..in`.

```pascal
program ShoppingCartTotal;

{$mode objfpc}{$H+}

uses
  SysUtils,
  ThreadSafeCollections.List;

type
  TItem = record
    Name: string;
    Price: Double;
  end;

function MakeItem(const AName: string; APrice: Double): TItem;
begin
  Result.Name := AName;
  Result.Price := APrice;
end;

function CompareItem(const A, B: TItem): Integer;
begin
  Result := CompareText(A.Name, B.Name);
end;

var
  Cart: specialize TThreadSafeList<TItem>;
  Total: Double;
  Item: TItem;
begin
  Cart := specialize TThreadSafeList<TItem>.Create(@CompareItem);
  try
    Cart.Add(MakeItem('eraser', 0.5));
    Cart.Add(MakeItem('pen', 1.25));
    Cart.Add(MakeItem('paper', 2.00));

    Total := 0;
    for Item in Cart do
      Total := Total + Item.Price;
    Writeln(Total:0:2);
    Writeln(Cart.Count);
  finally
    Cart.Free;
  end;
end.
```

**Expected output:**

```text
3.75
3
```

**Caveat:** The comparer is required for construction and is used for ordering
and searching. Stored records are copied by value.
[Source program](../../examples/documentation/05_shopping_cart_total.pas).

## Make a multi-step update atomic with a lock token

**Problem:** Run several list operations back-to-back without letting another
thread interleave between them.

**Recommended API:** `Lock` and an `ILockToken` held across the sequence.

```pascal
program LockTokenDemo;

{$mode objfpc}{$H+}

uses
  ThreadSafeCollections.Interfaces,
  ThreadSafeCollections.List;

var
  Pending: specialize TThreadSafeList<string>;
  Token: ILockToken;
begin
  Pending := specialize TThreadSafeList<string>.Create(@StringComparer);
  try
    Token := Pending.Lock;
    try
      Pending.Add('one');
      Pending.Add('two');
      Pending.Replace(1, 'two!');
      Pending.Sort;
    finally
      Token.Release;
    end;
    Writeln(Pending.Count);
  finally
    Pending.Free;
  end;
end.
```

**Expected output:**

```text
2
```

**Caveat:** Do not call public collection methods while holding a manual token;
methods lock internally and non-reentrant implementations can deadlock. Use the
token for combinations the API does not offer directly, or for private helper
steps. See [Lock Tokens (RAII)](lock-tokens.md).
[Source program](../../examples/documentation/06_lock_token_demo.pas).

## Related examples

The repository's 16 runnable programs go further: custom dictionary keys,
interface forms, multi-threaded queues, and iteration.
See the [Learning Path](../start/learning-path.md) and
[Building & Verification](../project/building.md#building-all-examples).