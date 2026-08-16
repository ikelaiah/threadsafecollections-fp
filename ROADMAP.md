# Roadmap: 0.8.5 to 2.0.0 and Beyond

[Documentation home](docs/README.md) · [Project README](README.md) · [Changelog](CHANGELOG.md)

**Audience:** users and contributors tracking correctness, concurrency, API
stability, compatibility, performance, and release work. Items are goals, not
current API guarantees.

ThreadSafeCollections-FP will first make its existing thread-safe generic
collections trustworthy and stable. It may then grow into a source-compatible
collection suite where there is demonstrated demand and a reviewed
compatibility manifest. This is a capability roadmap, not a calendar: evidence
from testing, portability, and benchmarks can move milestone contents.

## Product direction and sequencing

The committed native core is:

- `TThreadSafeList<T>`;
- `TThreadSafeDeque<T>`;
- `TThreadSafeDictionary<TKey, TValue>`; and
- `TThreadSafeHashSet<T>`.

The immediate path to 1.0 is deliberately depth-first: automated verification,
concurrency hardening, API consistency, documented iterator/locking semantics,
packaging, and reproducible benchmarks. New collection families are not a 1.0
requirement.

The long-term direction has two complementary surfaces:

- **Native API** — the `TThreadSafe*` types and modern operations designed for
  concurrent programs.
- **Compatibility API** — familiar names and signatures, backed by the same
  tested storage engines, to make accepted migrations predictable.

The compatibility program is a **post-1.0 decision gate**, not a release gate
for 1.0. It starts only after a concrete migration use case or demonstrated user
demand is accepted and a machine-readable manifest defines the intended surface.
This preserves the 2.0 direction without making speculative breadth compete
with the quality of the native core.

### What “drop-in” means if the compatibility gate is accepted

Accepted compatibility-profile programs should be able to compile by selecting
the façade package or adjusting the relevant `uses` entry, without renaming
collection types or rewriting algorithms. Compatibility must cover documented
signatures, results, exceptions, ownership, notifications, capacity, and
enumeration behavior. Each intentional difference must appear in a generated
report.

This is source-level compatibility on supported Free Pascal compilers; it does
not promise binary compatibility with precompiled units or private layouts.

### Compatibility profiles

The accepted manifest will classify each public item as Core, Extended, an
intentional difference, or out of scope.

- **Core:** array algorithms; enumerable/enumerator and pair foundations;
  lists, queues, stacks, dictionaries, hash sets; object-owning variants;
  thread-safe lists; and bounded threaded queues.
- **Extended:** sorted and ordered families, tree maps and sets, indexed trees,
  additional hash-map strategies, and safe collection views.

Private, deprecated, and experimental helpers are excluded unless the manifest
explicitly includes them.

## Guiding principles

- **Correctness before cleverness.** Managed types, ownership, exceptions, and
  concurrency edge cases must be correct before an optimization is retained.
- **Tests are part of the product.** Important claims run in CI, rather than
  existing only as historical local results.
- **Thread safety has precise boundaries.** Individual operations, atomic
  compound operations, and multi-operation workflows are documented separately.
- **Deadlocks are correctness bugs.** Lock ordering, iteration, callbacks,
  bulk operations, and destruction require deliberate regression coverage.
- **Performance claims are reproducible.** Measurements include the workload,
  environment, configuration, and regression budget.
- **Documentation is part of the API.** Public behavior includes complexity,
  concurrency, iterators, ownership, and exceptions.
- **Depth before breadth.** Existing collections become excellent before new
  families are committed.
- **Compatibility is tested, not inferred.** A similar method name is not a
  compatibility guarantee.

## Historical baseline — 0.8.5

The 0.8.5 roadmap recorded a solid correctness and performance foundation:

- `TThreadSafeList<T>`, `TThreadSafeDeque<T>`,
  `TThreadSafeDictionary<TKey, TValue>`, and `TThreadSafeHashSet<T>`;
- array, circular-buffer, and chained-hash-table storage;
- managed-type-safe List and Deque operations;
- slab allocation for Dictionary and HashSet entries;
- optimized hashing and average O(n+m) HashSet intersection;
- bulk operations designed to avoid cross-collection lock-order deadlocks;
- lock-holding iteration for List, Deque, and HashSet, and snapshot iteration
  for Dictionary;
- 118 FPCUnit tests with no reported HeapTrc leaks on the recorded Win64 run;
- Windows and Linux example builds; and
- generated API cheat sheets, collection guides, examples, and benchmarks.

The principal limitations identified at 0.8.5 were one exclusive
`TCriticalSection` per collection, unsafe re-entry through manual `Lock()`
tokens, inconsistent iterator behavior, non-uniform default construction and
comparer integration, incomplete compatibility coverage, incomplete CI gates,
and point-in-time rather than regression-controlled benchmarks.

## Current baseline — 0.8.6

The project already contains generic, per-instance-synchronized List, Deque,
Dictionary, and HashSet implementations; array, circular-buffer, and
chained-hash-table storage; managed-type-aware operations; optimized hashing;
bulk-operation lock-order protections; lock-holding enumeration for List,
Deque, and HashSet; and snapshot enumeration for Dictionary.

It also has interface-backed collection forms, scoped lock tokens, 118 FPCUnit
tests in the maintained local snapshot, a recorded clean Win64 HeapTrc run, 16
tracked examples, Windows and Linux example builds, benchmarks, Lazarus package
metadata, generated cheat sheets, and task-oriented documentation.

The remaining weaknesses are primarily verification and contract maturity:

- CI does not yet run the complete FPCUnit suite on each primary environment;
- concurrency, leak, portability, and benchmark results are not yet complete
  continuous release gates;
- iterator behavior differs between collection families;
- `Lock()` has important re-entry limitations;
- comparer, hashing, and construction defaults are not fully uniform; and
- package and documentation versions can drift.

## Milestones at a glance

| Version | Theme | Primary outcome |
|---|---|---|
| 0.8.7 | CI and verification | Existing correctness claims become automated checks |
| 0.8.8 | Concurrency hardening | Races, deadlocks, iteration, resize, and lifetime behavior are exercised |
| 0.8.9 | API consistency and 1.0 preparation | Pre-1.0 inconsistencies resolved and core frozen |
| 1.0.0 | Stable native core | Four well-tested collection families with documented contracts |
| 1.1.0 | Atomic concurrent workflows | Compound operations eliminate common check-then-act races |
| 1.2.0 | Read scalability | Measured parallel-read and contention improvements |
| 1.3–1.4 | Conditional compatibility breadth | Ordered/advanced families only after the compatibility gate |
| 1.5.0 | Conditional 2.0 preview | Feature-complete façade, migration tooling, and validation |
| 2.0.0 | Conditional compatibility guarantee | Release-gated replacement surface for accepted profiles |

## 0.8.7 — CI and verification

- Run the practical FPCUnit suite in CI, separating fast unit, long stress,
  leak, benchmark, and example jobs where appropriate.
- Establish Windows and Linux runtime coverage and clearly document any scope
  that cannot yet run on a platform.
- Preserve HeapTrc verification where applicable and document its limits.
- Add a Lazarus/package smoke build that verifies paths, dependencies, unit
  compilation, version, source inclusion, and, where practical, a tiny consumer.
- Check release metadata across README badges, `CHANGELOG.md`, package metadata,
  generated documentation, and release notes.
- Automate inexpensive documentation checks: local links, referenced files,
  example paths, stale generated cheat sheets, and compilable examples.

### Exit criteria

- Full practical FPCUnit coverage runs automatically on the primary Windows
  environment, with Linux scope established and documented.
- Examples compile on Windows and Linux; package smoke verification is automated
  or reproducibly documented.
- Leak-verification scope and actual CI coverage are accurately documented.
- CI reports identify whether examples, tests, packaging, or documentation failed.

## 0.8.8 — Concurrency hardening

- Add deterministic, barrier/event-coordinated tests for List, Deque,
  Dictionary, and HashSet under concurrent add, remove, lookup, resize,
  enumeration, collision, and bulk-operation workloads.
- Maintain bounded-completion deadlock regressions for opposite lock order,
  cross-collection input, lock-holding enumeration, manual lock tokens,
  callbacks, and lifetime/destruction boundaries.
- Decide whether the current lock-holding versus snapshot iterator difference
  remains intentional or converges toward a common policy; measure safety,
  compatibility, memory, and performance before changing it.
- Exercise valid and invalid boundaries for worker threads, enumerators,
  snapshots, interfaces, and lock tokens that refer to a collection.
- Add randomized stress tests for add, remove, contains, clear, resize,
  enumeration, ranges, and poor hashes. Record seed, iterations, thread count,
  collection type, and operation mix for reproduction.

### Exit criteria

- Every core collection has intentional multi-thread stress coverage.
- Lock-order, collision, resize, iterator, and lifetime regressions have direct tests.
- Randomized failures report reproducible seeds.
- No known high-severity race, deadlock, corruption, or managed-memory defect remains open.

## 0.8.9 — API consistency and 1.0 preparation

- Normalize or deliberately document default construction, custom comparers,
  equality/hash functions, built-in specializations, and invalid callback behavior.
- Review related List, Deque, Dictionary, and HashSet names and contracts without
  renaming merely for visual symmetry.
- Specify and test duplicate keys, missing keys, indexes, empty deque operations,
  capacities, callback exceptions, partial bulk failures, and allocation failures
  where practical.
- Finalize the safe use, redesign, or deprecation of `Lock()` so it cannot be
  mistaken for a safe re-entrant multi-operation API.
- Document purpose, complexity, atomicity, blocking, iterator implications,
  ownership, and exceptions for public core methods.
- Review the candidate 1.0 public surface, final pre-1.0 deprecations, direct
  `-Fu` use, Lazarus installation, clean-checkout builds, and version consistency.

### Exit criteria

- Construction, comparer, and hash behavior are consistent or intentionally documented.
- Iterator and manual-lock policies are finalized for 1.0.
- Core APIs have concurrency and ownership documentation, and known breaking
  inconsistencies are resolved.
- The candidate 1.0 surface, package metadata, and release metadata are reviewed.

## 1.0.0 — Stable native core

1.0 marks stable, tested contracts for the four native core families. It does
not claim that every possible concurrent collection exists, nor does it depend
on the compatibility program.

### Guarantees

- Stable public core APIs under Semantic Versioning.
- Documented synchronization, iterator, ownership, exception, and lifetime rules.
- Automated unit, multi-platform, stress, leak, package, and example verification.
- Reproducible benchmark tooling and useful complexity expectations.
- A published support matrix naming the Free Pascal, Windows, Linux, Lazarus,
  and other targets actually exercised.

Thread-safe individual operations do not automatically make a sequence such as
`if Contains(X) then Remove(X);` atomic. Users need a documented compound API,
an explicitly supported scoped-access mechanism, or external synchronization.

### Release gates

- Normal CI and supported-environment FPCUnit tests pass.
- Concurrency stress and package smoke verification pass.
- No known critical/high correctness, deadlock, corruption, or leak defect remains.
- All core families have tested examples, reviewed stable APIs, and documentation
  that matches actual behavior.

## 1.1.0 — Atomic concurrent workflows

- Add Dictionary get-or-add, add-or-update, try-update, and remove-with-value
  operations where their contracts can be made precise.
- Add atomic set operations that eliminate common check-then-act races.
- Provide immutable snapshots and safe scoped read/write access for supported
  multi-operation workflows.
- Complete bounded-queue cancellation, timeout, shutdown, drain, and metrics
  behavior if the queue façade is accepted.
- Define callback/factory retry, exception, ordering, lock, and re-entry behavior.

### Exit criteria

- Deterministic race fixtures and randomized history checks pass.
- Re-entrant callbacks, exceptions, cancellation, shutdown, and destruction are covered.
- The atomicity table and examples document every new operation.

## 1.2.0 — Read scalability

- Introduce an internal lock abstraction and evaluate reader/writer locking.
- Allow concurrent reads where safe; evaluate sharding/striping for Dictionary
  and HashSet and low-contention `Count`/`IsEmpty` paths.
- Reduce write-lock duration by preparing resize/bulk-operation data outside a
  lock when correctness permits.
- Evaluate lock-free paths only with a design review, safety argument, stress
  tests, and benchmarks; retain the simpler implementation when gates are unmet.
- Record throughput, p50/p95/p99 latency, allocations, and peak memory for
  single-threaded and contended workloads.

### Exit criteria

- Read-heavy workloads show a material, reproducible gain over the 1.0 baseline.
- Write-heavy and mixed workloads have no unexplained material regression.
- Race, starvation, resize, destruction, and cross-collection stress suites pass.
- The selected lock policy and fairness guarantees are public documentation.

## Compatibility decision gate

Before committing the 1.3–2.0 compatibility track, the project must have a
concrete migration use case or demonstrated demand, maintainers willing to own
the long-term contract, and an approved manifest. The discovery work then:

- defines pair, enumerable, enumerator, comparer, equality, notification,
  exception, and collection-view contracts;
- separates storage engines from native and compatibility façades;
- adds compile fixtures in `objfpc` and `delphi` modes and behavioral fixtures
  for duplicates, missing keys, ranges, capacity, managed values, enumeration,
  notification ordering, and ownership; and
- chooses façade unit names, namespace behavior, package search order,
  deprecation policy, API-diff automation, and the iterator/locking contract.

No compatibility type is promised until it appears in the reviewed manifest.

## 1.3.0–1.4.0 — Conditional compatibility breadth

If the gate is accepted:

- Add sorted List behavior, ordered Dictionary variants, sorted sets, balanced
  tree maps/sets, indexed variants, range queries, ordering policies, and
  object-owning/notification behavior required by the Extended manifest.
- Complete accepted advanced hash-map strategies and collection views. Borrowed
  or pointer views must not silently outlive mutation; safety exceptions are
  documented as intentional incompatibilities.
- Complete the Extended manifest; test Windows, Linux, and macOS on x86-64 and
  ARM64 where infrastructure permits; add 32-bit overflow/capacity tests.
- Ship versioned archives, checksums, package metadata, clean install/uninstall
  checks, and reproducible documentation generation.

### Exit criteria

- Every accepted item is implemented or has a reviewed, documented safety exception.
- Ordered/tree differential tests verify insert, remove, search, rank, range,
  and enumeration; complexity claims match scaling benchmarks.
- Primary platform/compiler combinations pass unit, stress, leak, example,
  packaging, and compatibility fixtures.

## 1.5.0 — Conditional 2.0 compatibility preview

- Mark the accepted façade feature-complete and freeze the proposed 2.0 surface.
- Publish API-diff and compatibility reports, precise native-API deprecations,
  migration guidance, release candidates, and benchmark results.
- Validate representative applications using managed records, interfaces,
  ownership, custom comparers, packages, and multi-threaded producer/consumer
  workloads.

### Exit criteria

- Core and Extended profiles have no accidental gaps.
- Representative applications compile without collection-level rewrites beyond
  selecting the façade.
- Critical/high preview findings are closed with regression tests, and release
  packages, documentation, and benchmarks are ready.

## 2.0.0 — Conditional compatibility and performance guarantee

2.0 is released only when the accepted profiles have a 100% pass rate for their
compile and behavioral fixtures, with no unlisted differences in signatures,
exceptions, ownership, notifications, views, or enumeration.

The release also requires no known critical/high correctness, deadlock, race,
use-after-free, double-free, or leak defect; passing unit, differential, stress,
cancellation, collision, allocation-failure, and cross-collection suites; and
complete atomicity, blocking, fairness, iterator, callback, and destruction
documentation.

Benchmark reports must cover scalar, string, record, interface, and object
payloads; normal/adversarial hashes; bulk operations; and contention mixes. CI
flags median regressions above 5% and blocks unexplained regressions above 10%
against the frozen 1.5 baseline on controlled runners.

## Cross-cutting test, documentation, and release discipline

All milestones build on compile contracts, behavior contracts, model/differential
tests, deterministic and randomized concurrency histories, managed-memory and
leak tests, performance tests with recorded metadata, and clean package tests.
Fast checks run on pull requests; expensive stress, platform, memory-tooling,
and benchmark jobs may run nightly or before releases.

Maintain a five-minute quick start; installation/build guide; collection guides;
generated reference and cheat sheet; concurrency/atomicity, iterator,
comparer/hashing, ownership/lifetime, benchmark, and migration guides. Examples
claimed as supported should compile in CI.

Every release updates the changelog, package/version metadata, generated docs,
and relevant migration notes; runs the checks appropriate to its milestone; and
publishes reproducible performance evidence for material changes. From 1.0,
patch releases are compatible fixes, minor releases add compatible capability,
and breaking public changes require strong justification and an appropriate
major version.

## Explicit non-goals

- Preserving private layout or undocumented implementation accidents.
- Promising undocumented iteration order or invalid-after-mutation pointer views.
- Calling a feature lock-free without demonstrated safety and performance value.
- Sacrificing managed-type safety, ownership correctness, or deadlock freedom
  for a microbenchmark result.
- Claiming support for platforms, profiles, or types that are not tested and
  accepted by the published contract.
- Making multiple independent method calls automatically atomic.

## How to read progress

Progress is measured by automated verification, prevented regressions, verified
platform coverage, documentation that matches implementation, reproducible
performance results, and fewer ambiguous API behaviors—not by the number of
type names. Release notes should link to the relevant test, API-diff, benchmark,
and compatibility reports.

The immediate success criterion is a modest, evidence-backed 1.0 claim: these
are the collections provided, these are their concurrency guarantees, and these
are the environments continuously verified. The 2.0 claim becomes available
only after the compatibility decision gate and its release criteria are met.
