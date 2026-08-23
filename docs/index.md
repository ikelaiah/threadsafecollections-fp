# ThreadSafeCollections-FP

ThreadSafeCollections-FP provides familiar collection APIs with per-instance
synchronization, bulk operations, and documented iteration behavior for Free
Pascal and Lazarus. It is a learning-focused project with a tested core,
runnable examples, and no required packages beyond the Free Pascal standard
distribution.

> [!IMPORTANT]
> The library is intended for learning and experimentation. Evaluate its
> behavior, performance, and test coverage against your application's
> requirements before adopting it in production.

## What you can do

Each collection instance owns one `TCriticalSection` and exposes the same
familiar operations you expect from a non-threaded collection, synchronized per
method call. Choose a collection below, then read the guide that matches your
task.

- **List** — indexed storage with add, insert, delete, range, sort, search,
  reverse, and capacity operations.
- **Deque** — a circular-buffer double-ended queue with `Push*`/`Pop*`/`Peek*`
  and `Try*` operations at both ends.
- **Dictionary** — chained hash buckets for key/value storage with custom hash
  and equality functions and snapshot-based iteration.
- **Hash set** — unique values with intersection, union, difference, overlap,
  and set-equality operations, plus specialized integer, string, Boolean, and
  real subclasses.

## Where to start

- New to the project? Follow the [Quick Start](start/quick-start.md), then the
  [Learning Path](start/learning-path.md).
- Adding the library to an application? Start with
  [Installation](start/installation.md).
- Looking up a declaration? Open the generated
  [Cheat Sheet](start/cheat-sheet.md) and the [API Overview](reference/api-overview.md).
- Building, testing, or contributing? See [Building & Verification](project/building.md).

The online documentation is versioned. The top-level site opens the current
release (`/0.8.8/`); the version selector in the header moves between released
documentation paths and stays on the matching page when it exists.