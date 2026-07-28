# Roadmap: 0.8.5 to 2.0.0

[Documentation home](docs/README.md) · [Project README](README.md) ·
[Changelog](CHANGELOG.md)

**Audience:** users and contributors tracking planned compatibility,
performance, quality, and release work. Items below are goals, not current API
guarantees.

ThreadSafeCollections-FP aims to become a production-ready, high-performance
collection library for modern Object Pascal. It should feel familiar on first
use, remain safe under concurrency, and make migration from widely used generic
collection APIs predictable.

This is a capability roadmap, not a calendar. Version contents may move when
correctness, compiler support, or benchmark evidence requires it. A milestone
is complete only when its exit criteria pass.

## Product direction

The project will offer two complementary surfaces:

- **Native API** — the existing `TThreadSafe*` types, plus modern operations
  designed specifically for concurrent programs.
- **Compatibility API** — familiar collection names and signatures backed by
  the same tested implementation, intended for migration with minimal source
  changes.

The native API will not be discarded to make room for compatibility. Existing
0.8.5 users should have a documented upgrade path, and public APIs will follow
Semantic Versioning from 1.0 onward.

### What “drop-in” means

By 2.0, accepted compatibility-profile programs should:

1. compile after selecting the compatibility package or changing the relevant
   `uses` entry, without renaming collection types or rewriting algorithms;
2. observe compatible method signatures, return values, exceptions, ownership
   rules, notifications, capacity behavior, and enumeration behavior;
3. gain a documented concurrency contract without needing to add external
   locking around individual operations; and
4. have every intentional difference listed in a generated compatibility
   report.

Compatibility will be tested, not inferred from similar method names.
It means source-level compatibility on supported Free Pascal compilers, not
binary compatibility with precompiled units or private memory layouts.

### Compatibility profiles

The exact surface will be frozen in the 0.9 compatibility manifest. It will be
split into:

- **Core profile** — array algorithms, enumerable and enumerator foundations,
  pair types, lists, queues, stacks, dictionaries, hash sets, object-owning
  variants, thread-safe lists, and bounded threaded queues.
- **Extended profile** — sorted and ordered collections, tree maps and sets,
  indexed trees, additional hash-map strategies, collection views, pointer or
  borrowed views where these can be made safe, and other accepted public types.

Internal, private, deprecated, or experimental implementation helpers are out
of scope unless the compatibility manifest explicitly includes them. There
will be no unlisted compatibility gaps.

## Guiding principles

- **Correctness before cleverness.** Managed types, ownership, exceptions, and
  concurrency edge cases must be correct before an optimization is retained.
- **Thread safety has precise semantics.** Each operation will document its
  atomicity, linearization point, blocking behavior, and iterator guarantees.
- **Performance claims are reproducible.** Claims require versioned benchmarks,
  recorded environments, representative workloads, and regression budgets.
- **Easy defaults, full control.** Common scalar, string, record, interface, and
  object types should work without boilerplate; custom comparers, hashers,
  allocators, ownership, and lock policies remain available.
- **Documentation is part of the API.** A public symbol is incomplete until its
  behavior, complexity, concurrency, exceptions, and ownership are documented.
- **Compatibility without copied implementation.** Behavior will be established
  through independently written contracts and fixtures.

## Current baseline — 0.8.5

The project already has a solid correctness and performance base:

- `TThreadSafeList<T>`, `TThreadSafeDeque<T>`,
  `TThreadSafeDictionary<TKey, TValue>`, and `TThreadSafeHashSet<T>`;
- array, circular-buffer, and chained-hash-table storage;
- managed-type-safe list and deque operations;
- slab allocation for dictionary and hash-set entries;
- optimized hashing and average O(n+m) hash-set intersection;
- bulk operations designed to avoid cross-collection lock-order deadlocks;
- lock-holding iteration for List, Deque, and HashSet, and snapshot iteration
  for Dictionary;
- 118 FPCUnit tests with no reported HeapTrc leaks on the recorded Win64 run;
- Windows and Linux example builds; and
- generated API cheat sheets, collection guides, examples, and benchmarks.

The main limitations to address are:

- one exclusive `TCriticalSection` per collection prevents concurrent reads;
- manual `Lock()` tokens cannot safely call the same collection's public
  methods on platforms with non-reentrant critical sections;
- iterator behavior is inconsistent across collection families;
- default construction and comparer integration are not yet uniform;
- the collection family and API surface are not yet compatibility-complete;
- CI builds examples but does not yet run the complete test, leak, portability,
  and benchmark gates; and
- benchmarks record useful point results but do not yet enforce a repeatable
  regression policy.

## Milestones at a glance

| Version | Theme | Primary outcome |
|---|---|---|
| 0.9.0 | Contract and foundations | Compatibility and concurrency become executable specifications |
| 1.0.0 | Stable core | Production-ready Core profile with a stable public API |
| 1.1.0 | Atomic concurrent workflows | Modern compound operations without check-then-act races |
| 1.2.0 | Read scalability | Measured parallel-read and contention improvements |
| 1.3.0 | Ordered and sorted families | High-level Extended-profile collection breadth |
| 1.4.0 | Advanced collections and portability | Remaining Extended profile, broader targets, mature packaging |
| 1.5.0 | 2.0 compatibility preview | Feature-complete façade, migration tooling, and ecosystem validation |
| 2.0.0 | Compatibility and performance guarantee | Stable, documented, release-gated replacement surface |

## 0.9.0 — Contract and foundations

### Compatibility

- Create a machine-readable compatibility manifest of public types, methods,
  overloads, properties, events, exceptions, and generic constraints.
- Classify each item as Core, Extended, implementation detail, or intentional
  difference.
- Add compile-only fixtures in both `objfpc` and `delphi` modes.
- Add behavioral fixtures for duplicate handling, missing keys, empty
  collections, range checks, capacity changes, managed values, enumeration,
  notification ordering, and object ownership.
- Decide and document façade unit names, namespace behavior, and package search
  order without disrupting native API users.
- Adopt a deprecation policy and an API-diff check for every release.

### Architecture and semantics

- Define common pair, enumerable, enumerator, comparer, equality, notification,
  exception, and collection-view contracts.
- Separate storage engines from public façades so native and compatibility APIs
  share the same implementation.
- Standardize integer/index types and overflow behavior across supported
  architectures.
- Publish a concurrency contract for every existing method.
- Choose one default iterator policy for 1.0. Snapshot iteration is preferred
  unless measurement demonstrates an unacceptable cost; explicit locked or
  borrowed iteration may be offered separately.
- Design a safe replacement for multi-operation manual locking, such as scoped
  read/write views that do not re-enter public locking methods.

### Engineering system

- Run the complete test suite in CI on Windows and Linux, not only example
  compilation.
- Test the oldest supported stable compiler and a current development compiler.
- Add separate fast unit, long stress, leak, and benchmark jobs.
- Store benchmark results with compiler, target, CPU, commit, configuration,
  sample count, and workload metadata.
- Establish compiler-warning and documentation-link checks.

### Exit criteria

- The compatibility manifest is reviewed, versioned, and diffable.
- Every 0.8.5 public method has a concurrency and ownership contract.
- Core-profile compile fixtures run in both supported language modes.
- Full Windows and Linux test jobs pass with no known leaks.
- A reproducible 0.8.5 performance and memory baseline is checked in.
- No new public API is accepted without tests and documentation.

## 1.0.0 — Stable core

### Core collection surface

- Complete array search and sorting helpers.
- Provide shared enumerable, enumerator, pair, comparer, equality, and
  notification foundations.
- Complete List behavior, including default and custom comparers, remove and
  extract variants, binary search, ranges, capacity, sorting, notifications,
  and collection constructors.
- Expose Queue and Stack APIs over the proven circular-buffer engine, including
  peek, extract, array conversion, capacity, and trimming behavior.
- Complete Dictionary behavior, including comparer-aware constructors,
  capacity, key and value views, extraction, notifications, collection
  constructors, and all duplicate-key paths.
- Complete HashSet algebra and relationship operations, including symmetric
  difference and subset/superset predicates required by the manifest.
- Add object-owning List, Queue, Stack, Dictionary, and HashSet variants with
  explicit, tested transfer and destruction rules.
- Add familiar thread-safe list and bounded blocking queue APIs, including
  timeout, shutdown, and wake-up behavior.

### Safety and usability

- Make useful default construction work for supported built-in and managed
  types.
- Keep custom comparer and hash-function construction available.
- Use a consistent snapshot iterator by default, with an explicit advanced API
  for lock-held access where justified.
- Ensure callbacks and notifications cannot cause undocumented self-deadlocks.
- Preserve the current `TThreadSafe*` API or provide compile-time deprecations
  and a migration guide for each changed member.
- Document exception guarantees for failed adds, allocation failures, comparer
  exceptions, and partial bulk operations.

### Documentation and distribution

- Publish a task-oriented getting-started guide for each Core collection.
- Generate an API reference from source and verify all examples in CI.
- Add guides for concurrency, iterators, comparers and hashing, ownership,
  exceptions, performance, and migration from 0.8.5.
- Provide tested Lazarus and command-line installation packages.

### Exit criteria

- The Core compatibility manifest is 100% implemented or has an explicit,
  reviewed exception for each missing item.
- Compile and behavioral fixtures pass in all supported compiler modes.
- No open critical or high-severity correctness, deadlock, or memory issue.
- All Core public symbols have API documentation and at least one tested usage
  path.
- Benchmark regressions beyond the release budget are fixed or explained in
  the release notes.
- Public Core APIs are declared stable under Semantic Versioning.

## 1.1.0 — Atomic concurrent workflows

- Add atomic Dictionary operations such as get-or-add, add-or-update,
  try-update, and remove-with-value.
- Add mutation callbacks or factories with documented retry, exception, and
  lock behavior.
- Add atomic set insert/remove/query variants where they remove common
  check-then-act races.
- Add immutable snapshots for all collection families.
- Add safe scoped read and write access for multi-operation transactions.
- Complete bounded-queue cancellation, timeout, shutdown, drain, and metrics
  behavior.
- Define whether notifications run before or after lock release and guarantee
  ordering consistently.

### Exit criteria

- Atomic APIs pass deterministic race fixtures and randomized history checks.
- Re-entrant callbacks, exceptions, cancellation, shutdown, and destruction
  have regression coverage.
- No public workflow requires the unsafe 0.8.5 manual-lock pattern.
- The atomicity table and examples cover every new operation.

## 1.2.0 — Read scalability

- Introduce an internal lock abstraction and a reader/writer implementation.
- Allow concurrent reads for List, Dictionary, HashSet, and Deque when safe.
- Evaluate lock striping or sharding for Dictionary and HashSet.
- Make `Count` and `IsEmpty` low-contention where supported by the target.
- Reduce time spent under write locks by preparing resize and bulk-operation
  data outside the critical section when correctness permits.
- Evaluate specialized lock-free queues or fast paths only through a design
  review, correctness proof or model, stress testing, and benchmarks. Retain
  the lock-based implementation when those gates are not met.
- Record throughput, p50/p95/p99 latency, allocation count, and peak memory for
  single-threaded and contended workloads.

### Exit criteria

- Read-heavy workloads demonstrate a material, reproducible improvement over
  the 1.0 exclusive-lock baseline.
- Write-heavy and mixed workloads have no unexplained material regression.
- Race, starvation, resize, destruction, and cross-collection stress suites
  pass on every supported operating system.
- The selected lock policy and fairness guarantees are public documentation.

## 1.3.0 — Ordered and sorted families

- Add sorted List behavior and automatic-sort options required by the manifest.
- Add ordered Dictionary variants with key- and index-based access.
- Add sorted Set and sorted HashSet variants.
- Add balanced tree Map and Set types, including indexed variants where
  required.
- Complete comparer propagation, ordering, duplicate policy, range queries,
  enumeration direction, and capacity semantics.
- Add object-owning variants and notifications for the new families.
- Provide native concurrent range and ordered-snapshot operations where they
  can be implemented without weakening compatibility.

### Exit criteria

- High-level ordered, sorted, and tree types in the Extended profile pass their
  compile and behavior fixtures.
- Ordering and iterator guarantees are deterministic and documented.
- Randomized differential tests cover insert, remove, search, rank, range, and
  enumeration behavior.
- Complexity claims are verified against benchmark scaling curves.

## 1.4.0 — Advanced collections and portability

- Complete accepted advanced hash-map strategies and public collection views.
- Support pointer or borrowed views only through lifetimes that cannot silently
  outlive a mutation; document any deliberate incompatibility needed for
  memory safety.
- Complete the Extended compatibility manifest.
- Test Windows, Linux, and macOS on x86-64 and ARM64 where CI infrastructure is
  available; document other targets as experimental or community-supported.
- Add 32-bit overflow and capacity tests even when 32-bit runners are not part
  of every CI run.
- Test stable and development compiler channels and publish the supported
  matrix.
- Provide versioned release archives, checksums, package metadata, and clean
  installation/uninstallation tests.
- Make documentation generation reproducible on all primary platforms.

### Exit criteria

- Every Extended-profile item is implemented or has a documented, reviewed
  safety exception.
- Primary platform/compiler combinations pass unit, stress, leak, example, and
  packaging jobs.
- No supported feature depends on undocumented structure layout.
- Installation and a minimal compile work in clean CI environments.

## 1.5.0 — 2.0 compatibility preview

- Mark the full compatibility façade feature-complete.
- Freeze the proposed 2.0 public surface and publish generated API-diff and
  compatibility reports.
- Deprecate superseded native APIs with precise replacements and automated
  migration notes.
- Publish complete migration guides for native 0.8.x/1.x users and
  compatibility-profile users.
- Validate representative real applications, including managed records,
  interfaces, object ownership, custom comparers, plugins/packages, and
  multi-threaded producer/consumer workloads.
- Complete performance tuning using the frozen behavior contract.
- Publish release-candidate packages and require a full compatibility cycle
  before 2.0.

### Exit criteria

- Both compatibility profiles report no accidental gaps.
- Representative applications compile without collection-level source
  rewrites beyond selecting the façade.
- No unresolved API-design issue is deferred to the 2.0 patch line.
- All critical and high-severity preview findings are closed with regression
  tests.
- Documentation, packages, and benchmark reports are release-candidate ready.

## 2.0.0 — Compatibility and performance guarantee

2.0 is the point at which the project may describe itself as a stable,
production-ready, drop-in collection suite for the accepted profiles.

The release requires all of the following:

### Compatibility

- 100% pass rate for Core and Extended compile fixtures.
- 100% pass rate for accepted behavioral contracts.
- No unlisted differences in signatures, exceptions, ownership,
  notifications, collection views, or enumeration behavior.
- A final API-diff report and complete 1.x-to-2.0 migration guide.

### Correctness and concurrency

- No known critical or high-severity correctness, deadlock, race, use-after-free,
  double-free, or leak defect.
- Unit, property-based, differential, long-running stress, cancellation,
  collision, allocation-failure, and cross-collection tests pass.
- Atomicity, blocking, fairness, iterator, callback, and destruction semantics
  are documented for every public concurrent operation.
- HeapTrc and platform-appropriate external memory/race tooling are clean for
  the supported test corpus.

### Performance

- Reproducible benchmark reports cover scalar, string, record, interface, and
  object payloads; normal and adversarial hashes; bulk operations; and
  read/write contention mixes.
- CI flags a median regression above 5% and blocks unexplained regressions above
  10% against the frozen 1.5 baseline on controlled runners.
- Every public complexity claim matches measured scaling behavior.
- Memory overhead and allocation rates are published alongside throughput and
  latency.

### Documentation and release engineering

- 100% of public symbols have generated reference documentation.
- Every collection family has a runnable quick start and a tested recipe.
- Compatibility, concurrency, ownership, migration, performance, and support
  guides are complete.
- All primary packages install, compile a smoke project, and uninstall cleanly.
- The support matrix, security policy, deprecation policy, and release process
  are published.

## Cross-cutting test strategy

All milestones build on the same test layers:

1. **Compile contracts** — names, overloads, constraints, visibility, modes,
   and unit/package selection.
2. **Behavior contracts** — results, exceptions, ownership, notifications,
   capacity, ordering, and iterators.
3. **Model and differential tests** — randomized operation sequences checked
   against simple reference models.
4. **Concurrency histories** — deterministic barriers plus randomized schedules
   checked against documented atomic behavior.
5. **Memory tests** — managed types, objects, interfaces, allocation failures,
   HeapTrc, and external tooling.
6. **Performance tests** — versioned workloads with statistical summaries and
   stored metadata.
7. **Packaging tests** — clean install, build, run, and uninstall.

Fast tests should run on every pull request. Long stress, extended platform,
memory-tooling, and benchmark jobs may run nightly and before releases.

## Documentation plan

The documentation set will grow into:

- a five-minute quick start;
- one task-oriented guide per collection family;
- a generated API reference and compact cheat sheet;
- a concurrency and atomicity guide;
- an iterator and snapshot guide;
- comparer, equality, hashing, and ordering guides;
- an object ownership and notification guide;
- a performance methodology and results dashboard;
- native and compatibility migration guides;
- a generated compatibility-status report; and
- fully tested examples ranging from single-threaded use to contended services.

Examples and declarations in documentation must compile in CI. Complexity and
thread-safety notes should be generated from the same metadata used by the API
reference wherever practical.

## Release discipline

Every release from 0.9 onward must:

- update the changelog, package version, compatibility manifest, and API diff;
- run unit, behavior, stress, leak, example, and package smoke tests appropriate
  to the milestone;
- publish or link its benchmark comparison;
- document all intentional compatibility changes;
- include migration notes for deprecations or breaking pre-1.0 changes; and
- avoid performance claims that cannot be reproduced from repository tooling.

Patch releases fix defects and documentation without expanding the stable
surface. Minor 1.x releases add backward-compatible functionality. Breaking
changes after 1.0 require deprecation where feasible and are reserved for 2.0.

## Explicit non-goals

- Preserving private field layout or undocumented implementation accidents.
- Promising a particular iteration order where the public contract does not.
- Exposing pointers that can become invalid after an unlocked mutation.
- Calling an operation lock-free without a demonstrated safety and performance
  case.
- Trading managed-type safety, ownership correctness, or deadlock freedom for a
  microbenchmark win.
- Claiming compatibility for types not present in the accepted manifest.

## How to read progress

Release notes will link to the generated compatibility report and summarize:

- Core and Extended profile completion;
- correctness and concurrency findings;
- supported compiler/platform coverage;
- performance changes against the frozen baseline; and
- documentation coverage.

The roadmap is complete when 2.0 satisfies its release gates, not merely when
all planned type names exist.
