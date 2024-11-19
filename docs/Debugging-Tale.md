# The Great Dictionary Resize Mystery - A Debugging Tale

This is a classic case of "Have you tried turning it off and on again?" but in software development!

## Initial Problem

The dictionary would hang when trying to add items after reaching the load factor threshold (0.75). The resize operation appeared to not be working, despite logs in the unit test showing:

```
...
Starting TestLargeDataSet
Running test #11
...
CheckLoadFactor: Current ratio: 0.81
CheckLoadFactor: Resizing needed
CheckLoadFactor: Resize complete
...
```
But the resize wasn't actually happening.

## The Journey

1. Spent hours debugging the resize functionality
2. Analyzed method linkage
3. Added debug logging
4. Removed and re-added `try..finally` blocks in `procedure TThreadSafeDictionary.CheckLoadFactor;`

Initially, the following code was used and did not work.

```pascal
procedure TThreadSafeDictionary.CheckLoadFactor;
begin
  WriteLn(Format('CheckLoadFactor: Current ratio: %f', [FCount / Length(FBuckets)]));
  if (FCount / Length(FBuckets)) > LOAD_FACTOR then
  begin
    WriteLn('CheckLoadFactor: Resizing needed');
    try
      Resize(Length(FBuckets) * 2);
    finally
      WriteLn('CheckLoadFactor: Resize complete');
    end;
  end;
end;
```

Then, rewrote it as follows and it worked.

```pascal
procedure TThreadSafeDictionary.CheckLoadFactor;
begin
  WriteLn(Format('CheckLoadFactor: Current ratio: %f', [FCount / Length(FBuckets)]));
  if (FCount / Length(FBuckets)) > LOAD_FACTOR then
  begin
    WriteLn('CheckLoadFactor: Resizing needed');
    Resize(Length(FBuckets) * 2);
    WriteLn('CheckLoadFactor: Resize complete');
  end;
end;
```

5. Questioned our understanding of Pascal/Delphi
6. Wrote detailed implementation fixes

## The "Solution"

1. Go to top menu bar in Lazarus 
2. Click **Run** 
3. Click **Clean and Build**


## What Actually Happened

- Old compiled code was probably still in use
- Incremental builds weren't picking up all changes
- Full rebuild cleared out all old compiled units and started fresh

At the end, both versions worked.

## Key Learnings

1. Always try a clean build when behavior seems inexplicable
2. Don't trust incremental builds when debugging weird issues
3. Sometimes the simplest solution is the right one
4. Hours of sophisticated debugging can be solved by basic build hygiene

## The Irony

The `try..finally` block was perfectly fine all along. We were debugging phantom issues from stale compiled code!

## Best Practices Reminder

1. Clean build when:
   - Behavior seems inconsistent
   - Changes don't seem to take effect
   - Different developers see different behavior
2. Don't trust incremental builds during deep debugging sessions
3. Add "Clean and Build" to your initial debugging checklist

My personal "Hall of Fame" of debugging story! 😄


---

## Slow Performance Issue - Troubleshooting Log

- Initially saw very slow performance
- Discovered excessive logging was the culprit
- Removed/reduced logging for performance tests
- Performance for 100,000 items:
  - Add: 31ms (0.31 microseconds per item)
  - Find: 78ms (0.78 microseconds per item)

Machine: Processor	11th Gen Intel(R) Core(TM) i7-11800H @ 2.30GHz, 2304 Mhz, 8 Core(s), 16 Logical Processor(s)
