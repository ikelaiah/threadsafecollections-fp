# Test Date: 2024-12-15 21:52

```bash
$ ./TestRunner.exe -a -p --format=plain
Starting test runner...
Time: 21:46:17.810
Running tests...
.................Multi-thread test completed in 0 ms
.Large dataset operations took: 15 ms
..Integer Test:
Sorting 100000 integers took: 0 ms
String Test:
Sorting 100000 strings took: 32 ms
....Lock test took 31046 ms, 4000 successful locks across 4 threads
......................Student Sort By Name Test:
Before sort (first 3):
Name: zygkpDGoCR, ID: 100000
Name: HCYuUjJaBk, ID: 99999
Name: zZPObcWccu, ID: 99998

Sorting 100000 students by name took: 188 ms
After sort (first 3):
Name: AABwaVyzls, ID: 11564
Name: AAFCqzrGPP, ID: 38296
Name: AAFNHgsYpZ, ID: 12727

Name sort verification: TRUE

.Student Sort By ID Test:
Before sort (first 3):
ID: 111229, Name: Student0
ID: 79650, Name: Student1
ID: 140762, Name: Student2

Sorting 100000 students by ID took: 141 ms
After sort (first 3):
ID: 1, Name: Student40650
ID: 5, Name: Student38197
ID: 6, Name: Student15325

ID sort verification: TRUE

.Setting up test...
Setup complete
Starting TestCreation
Running test #1
Completed TestCreation
.Setting up test...
Setup complete
Starting TestAdd
Running test #2
Completed TestAdd
.Setting up test...
Setup complete
Starting TestAddDuplicate
Running test #3
Completed TestAddDuplicate
.Setting up test...
Setup complete
Starting TestGetItem
Running test #4
Completed TestGetItem
.Setting up test...
Setup complete
Starting TestRemove
Running test #5
Completed TestRemove
.Setting up test...
Setup complete
Starting TestAddOrSetValue
Running test #6
Completed TestAddOrSetValue
.Setting up test...
Setup complete
Starting TestClear
Running test #7
Completed TestClear
.Setting up test...
Setup complete
Starting TestCount
Running test #8
Completed TestCount
.Setting up test...
Setup complete
Starting TestFirstLast
Running test #9
Completed TestFirstLast
.Setting up test...
Setup complete
Starting Test10_EmptyDictionary
Running test #10
Completed TestEmptyDictionary
.Setting up test...
Setup complete
Running test #11
.Setting up test...
Setup complete
.Setting up test...
Setup complete
.Setting up test...
Setup complete
Starting Test14_MultiThreadAccess
Creating thread 0
Creating thread 1
Creating thread 2
Creating thread 3
Creating thread 4
Creating thread 5
Creating thread 6
Creating thread 7
Creating thread 8
Creating thread 9
Waiting for thread 0
Waiting for thread 1
Waiting for thread 2
Waiting for thread 3
Waiting for thread 4
Waiting for thread 5
Waiting for thread 6
Waiting for thread 7
Waiting for thread 8
Waiting for thread 9
TestMultiThreadAccess completed
.Setting up test...
Setup complete
Starting Test15_ConcurrentOperations
TestConcurrentOperations completed
.Setting up test...
Setup complete
.Setting up test...
Setup complete
Adding 100000 items took: 63 ms
Finding 100000 items took: 281 ms
.Setting up test...
Setup complete
Adding 100000 items took: 47 ms
Finding 100000 items took: 15 ms
.Setting up test...
Setup complete
Starting Test19_InitialCapacity
Running test #12
Completed TestInitialCapacity
.Setting up test...
Setup complete
Starting Test20_ManualResize
Running test #13
Completed TestManualResize
.Setting up test...
Setup complete
Starting Test21_ResizeWithData
Running test #14
Completed TestResizeWithData
.Setting up test...
Setup complete
Starting Test22_ResizeUnderflow
Running test #15
Completed TestResizeUnderflow
.Setting up test...
Setup complete
Starting Test23_BucketCount
Running test #16
Completed TestBucketCount
.Setting up test...
Setup complete
Starting Test24_IteratorBasic
Running test #17
.Setting up test...
Setup complete
Starting Test25_IteratorEmpty
Running test #18
.Setting up test...
Setup complete
Starting Test26_IteratorModification
Running test #19
.Setting up test...
Setup complete
Starting Test27_MultipleIterators
Running test #20
.Setting up test...
Setup complete
Starting Test28_IteratorReset
Running test #21
.Setting up test...
Setup complete
Lock test took 31427 ms, 4000 successful locks across 4 threads
.Setting up test...
Setup complete
Starting Test30_CompoundKeyBasic
Running test #22
TestCompoundKeyBasic passed
.Setting up test...
Setup complete
Starting Test31_CompoundKeyIteration
Running test #23
TestCompoundKeyIteration passed
.Setting up test...
Setup complete
Starting Test32_CustomConstructors
Running test #24
TestCustomConstructors passed
.21:47:21.660 - SetUp starting...
21:47:21.660 - SetUp completed
21:47:21.660 - Test1_BasicOperations starting...
21:47:21.660 - Test1_BasicOperations completed
21:47:21.660 - TearDown starting...
21:47:21.660 - TearDown completed
.21:47:21.660 - SetUp starting...
21:47:21.660 - SetUp completed
21:47:21.660 - Test2_SimpleAdd starting...
21:47:21.660 - Test2_SimpleAdd completed
21:47:21.660 - TearDown starting...
21:47:21.660 - TearDown completed
.21:47:21.660 - SetUp starting...
21:47:21.660 - SetUp completed
21:47:21.660 - Test3_SimpleRemove starting...
21:47:21.660 - Test3_SimpleRemove completed
21:47:21.660 - TearDown starting...
21:47:21.660 - TearDown completed
.21:47:21.660 - SetUp starting...
21:47:21.660 - SetUp completed
21:47:21.660 - Test4_Duplicates starting...
21:47:21.660 - Test4_Duplicates completed
21:47:21.660 - TearDown starting...
21:47:21.660 - TearDown completed
.21:47:21.660 - SetUp starting...
21:47:21.660 - SetUp completed
21:47:21.660 - Test5_Clear starting...
21:47:21.660 - Test5_Clear completed
21:47:21.660 - TearDown starting...
21:47:21.660 - TearDown completed
.21:47:21.660 - SetUp starting...
21:47:21.660 - SetUp completed
21:47:21.660 - Test6_StudentBasic starting...
21:47:21.660 - Test6_StudentBasic completed
21:47:21.660 - TearDown starting...
21:47:21.660 - TearDown completed
.21:47:21.660 - SetUp starting...
21:47:21.660 - SetUp completed
21:47:21.660 - Test7_LargeDataSet starting...
21:47:21.663 - Added batch 1/100 (100 items total) - Batch time: 16 ms, Total time: 16 ms
21:47:21.674 - Added batch 2/100 (200 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.690 - Added batch 3/100 (300 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.705 - Added batch 4/100 (400 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.721 - Added batch 5/100 (500 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.737 - Added batch 6/100 (600 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.752 - Added batch 7/100 (700 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.768 - Added batch 8/100 (800 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.783 - Added batch 9/100 (900 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.798 - Added batch 10/100 (1000 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.814 - Added batch 11/100 (1100 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.829 - Added batch 12/100 (1200 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.845 - Added batch 13/100 (1300 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.861 - Added batch 14/100 (1400 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.876 - Added batch 15/100 (1500 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.892 - Added batch 16/100 (1600 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.907 - Added batch 17/100 (1700 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.924 - Added batch 18/100 (1800 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.938 - Added batch 19/100 (1900 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.953 - Added batch 20/100 (2000 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.969 - Added batch 21/100 (2100 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:21.985 - Added batch 22/100 (2200 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.001 - Added batch 23/100 (2300 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.017 - Added batch 24/100 (2400 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.032 - Added batch 25/100 (2500 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.048 - Added batch 26/100 (2600 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.064 - Added batch 27/100 (2700 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.079 - Added batch 28/100 (2800 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.094 - Added batch 29/100 (2900 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.110 - Added batch 30/100 (3000 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.125 - Added batch 31/100 (3100 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.141 - Added batch 32/100 (3200 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.157 - Added batch 33/100 (3300 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.172 - Added batch 34/100 (3400 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.187 - Added batch 35/100 (3500 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.202 - Added batch 36/100 (3600 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.219 - Added batch 37/100 (3700 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.234 - Added batch 38/100 (3800 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.249 - Added batch 39/100 (3900 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.265 - Added batch 40/100 (4000 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.280 - Added batch 41/100 (4100 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.295 - Added batch 42/100 (4200 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.311 - Added batch 43/100 (4300 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.327 - Added batch 44/100 (4400 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.343 - Added batch 45/100 (4500 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.359 - Added batch 46/100 (4600 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.374 - Added batch 47/100 (4700 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.390 - Added batch 48/100 (4800 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.406 - Added batch 49/100 (4900 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.421 - Added batch 50/100 (5000 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.438 - Added batch 51/100 (5100 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.453 - Added batch 52/100 (5200 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.469 - Added batch 53/100 (5300 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.485 - Added batch 54/100 (5400 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.501 - Added batch 55/100 (5500 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.517 - Added batch 56/100 (5600 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.532 - Added batch 57/100 (5700 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.550 - Added batch 58/100 (5800 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.563 - Added batch 59/100 (5900 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.579 - Added batch 60/100 (6000 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.594 - Added batch 61/100 (6100 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.611 - Added batch 62/100 (6200 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.626 - Added batch 63/100 (6300 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.642 - Added batch 64/100 (6400 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.658 - Added batch 65/100 (6500 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.673 - Added batch 66/100 (6600 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.689 - Added batch 67/100 (6700 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.705 - Added batch 68/100 (6800 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.720 - Added batch 69/100 (6900 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.736 - Added batch 70/100 (7000 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.751 - Added batch 71/100 (7100 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.767 - Added batch 72/100 (7200 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.783 - Added batch 73/100 (7300 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.799 - Added batch 74/100 (7400 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.814 - Added batch 75/100 (7500 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.830 - Added batch 76/100 (7600 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.845 - Added batch 77/100 (7700 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.860 - Added batch 78/100 (7800 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.876 - Added batch 79/100 (7900 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.891 - Added batch 80/100 (8000 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.907 - Added batch 81/100 (8100 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.924 - Added batch 82/100 (8200 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.939 - Added batch 83/100 (8300 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.954 - Added batch 84/100 (8400 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.970 - Added batch 85/100 (8500 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:22.986 - Added batch 86/100 (8600 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.001 - Added batch 87/100 (8700 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.017 - Added batch 88/100 (8800 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.032 - Added batch 89/100 (8900 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.048 - Added batch 90/100 (9000 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.064 - Added batch 91/100 (9100 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.079 - Added batch 92/100 (9200 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.095 - Added batch 93/100 (9300 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.111 - Added batch 94/100 (9400 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.126 - Added batch 95/100 (9500 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.141 - Added batch 96/100 (9600 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.157 - Added batch 97/100 (9700 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.172 - Added batch 98/100 (9800 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.188 - Added batch 99/100 (9900 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.204 - Added batch 100/100 (10000 items total) - Batch time: 0 ms, Total time: 16 ms
21:47:23.220 - Total time for adding 10000 items: 1563 ms
21:47:23.220 - Actual processing time (excluding logging): 16 ms
21:47:23.221 - Verification of 10000 items took: 0 ms
21:47:23.221 - TearDown starting...
21:47:23.222 - TearDown completed
.21:47:23.222 - SetUp starting...
21:47:23.222 - SetUp completed
21:47:23.222 - Test8_ConcurrentAccess starting...
21:47:23.222 - Created thread 0
21:47:23.222 - Created thread 1
21:47:23.222 - Created thread 2
21:47:23.222 - Created thread 3
21:47:23.329 - Started thread 0 at 109 ms
21:47:23.329 - Started thread 1 at 109 ms
21:47:23.329 - Started thread 2 at 109 ms
21:47:23.329 - Started thread 3 at 109 ms
21:47:23.350 - Thread 0 completed at 141 ms
21:47:23.351 - Thread 1 completed at 141 ms
21:47:23.351 - Thread 2 completed at 141 ms
21:47:23.351 - Thread 3 completed at 141 ms
21:47:23.351 - Concurrent test total time: 141 ms
21:47:23.351 - Final item count: 9964
21:47:23.351 - TearDown starting...
21:47:23.353 - TearDown completed
.21:47:23.353 - SetUp starting...
21:47:23.353 - SetUp completed
21:47:23.353 - Test9_StressTest starting...
21:47:23.357 - Completed 10000/100000 iterations (10%) - Time: 0 ms
21:47:23.365 - Completed 20000/100000 iterations (20%) - Time: 15 ms
21:47:23.382 - Completed 30000/100000 iterations (30%) - Time: 31 ms
21:47:23.400 - Completed 40000/100000 iterations (40%) - Time: 46 ms
21:47:23.413 - Completed 50000/100000 iterations (50%) - Time: 62 ms
21:47:23.429 - Completed 60000/100000 iterations (60%) - Time: 78 ms
21:47:23.443 - Completed 70000/100000 iterations (70%) - Time: 93 ms
21:47:23.462 - Completed 80000/100000 iterations (80%) - Time: 109 ms
21:47:23.476 - Completed 90000/100000 iterations (90%) - Time: 125 ms
21:47:23.490 - Completed 100000/100000 iterations (100%) - Time: 140 ms
21:47:23.500 - Stress test completed in 140 ms
21:47:23.500 - Final counts - Int: 25000, Str: 25000, Bool: 0, Student: 25000
21:47:23.500 - TearDown starting...
21:47:23.510 - TearDown completed
.21:47:23.510 - SetUp starting...
21:47:23.510 - SetUp completed
=== Starting Hash Collision Test ===
Verifying hash collisions in first group:
First item (Group 0): "A_Item_0" -> Hash: DEADBEEF
Second item (Group 0): "A_Item_4" -> Hash: DEADBEEF
SUCCESS: Hash collision confirmed!
Adding items with forced collisions...
Added 1000/10000 items...
Added 2000/10000 items...
Added 3000/10000 items...
Added 4000/10000 items...
Added 5000/10000 items...
Added 6000/10000 items...
Added 7000/10000 items...
Added 8000/10000 items...
Added 9000/10000 items...
Added 10000/10000 items...
Initial set count: 10000
Starting deep verification...
Verified 1000/10000 items...
Verified 2000/10000 items...
Verified 3000/10000 items...
Verified 4000/10000 items...
Verified 5000/10000 items...
Verified 6000/10000 items...
Verified 7000/10000 items...
Verified 8000/10000 items...
Verified 9000/10000 items...
Verified 10000/10000 items...
Deep verify summary: Total lost keys: 0
Final set count: 10000
Total test time: 812 ms
=== Hash Collision Test Complete ===
21:47:24.319 - TearDown starting...
21:47:24.320 - TearDown completed
.21:47:24.320 - SetUp starting...
21:47:24.320 - SetUp completed
=== Starting Aggressive Collision Test ===
Creating 100000 items with 4 threads
Verifying hash collisions:
Group A hash: DEADBEEF, Group B hash: DEADBEEF
Adding items with forced collisions using multiple threads...
Creating thread 0 for range 0 to 24999
Creating thread 1 for range 25000 to 49999
Creating thread 2 for range 50000 to 74999
Creating thread 3 for range 75000 to 99999
Starting all threads...
Waiting for threads to complete...
Thread 40584 starting, range 25000 to 49999
Thread 27656 starting, range 0 to 24999
Thread 44468 starting, range 50000 to 74999
Thread 35908 starting, range 75000 to 99999
Thread 40584 processed 1000/25000 items, successfully added: 1000
Thread 44468 processed 1000/25000 items, successfully added: 1000
Thread 35908 processed 1000/25000 items, successfully added: 1000
Thread 44468 processed 2000/25000 items, successfully added: 2000
Thread 35908 processed 2000/25000 items, successfully added: 2000
Thread 27656 processed 1000/25000 items, successfully added: 1000
Thread 40584 processed 2000/25000 items, successfully added: 2000
Thread 35908 processed 3000/25000 items, successfully added: 3000
Thread 40584 processed 3000/25000 items, successfully added: 3000
Thread 27656 processed 2000/25000 items, successfully added: 2000
Thread 44468 processed 3000/25000 items, successfully added: 3000
Thread 40584 processed 4000/25000 items, successfully added: 4000
Thread 35908 processed 4000/25000 items, successfully added: 4000
Thread 44468 processed 4000/25000 items, successfully added: 4000
Thread 35908 processed 5000/25000 items, successfully added: 5000
Thread 40584 processed 5000/25000 items, successfully added: 5000
Thread 44468 processed 5000/25000 items, successfully added: 5000
Thread 40584 processed 6000/25000 items, successfully added: 6000
Thread 27656 processed 3000/25000 items, successfully added: 3000
Thread 35908 processed 6000/25000 items, successfully added: 6000
Thread 27656 processed 4000/25000 items, successfully added: 4000
Thread 44468 processed 6000/25000 items, successfully added: 6000
Thread 27656 processed 5000/25000 items, successfully added: 5000
Thread 35908 processed 7000/25000 items, successfully added: 7000
Thread 40584 processed 7000/25000 items, successfully added: 7000
Thread 44468 processed 7000/25000 items, successfully added: 7000
Thread 35908 processed 8000/25000 items, successfully added: 8000
Thread 27656 processed 6000/25000 items, successfully added: 6000
Thread 44468 processed 8000/25000 items, successfully added: 8000
Thread 27656 processed 7000/25000 items, successfully added: 7000
Thread 35908 processed 9000/25000 items, successfully added: 9000
Thread 40584 processed 8000/25000 items, successfully added: 8000
Thread 44468 processed 9000/25000 items, successfully added: 9000
Thread 35908 processed 10000/25000 items, successfully added: 10000
Thread 27656 processed 8000/25000 items, successfully added: 8000
Thread 35908 processed 11000/25000 items, successfully added: 11000
Thread 27656 processed 9000/25000 items, successfully added: 9000
Thread 40584 processed 9000/25000 items, successfully added: 9000
Thread 35908 processed 12000/25000 items, successfully added: 12000
Thread 44468 processed 10000/25000 items, successfully added: 10000
Thread 35908 processed 13000/25000 items, successfully added: 13000
Thread 27656 processed 10000/25000 items, successfully added: 10000
Thread 40584 processed 10000/25000 items, successfully added: 10000
Thread 44468 processed 11000/25000 items, successfully added: 11000
Thread 35908 processed 14000/25000 items, successfully added: 14000
Thread 27656 processed 11000/25000 items, successfully added: 11000
Thread 40584 processed 11000/25000 items, successfully added: 11000
Thread 35908 processed 15000/25000 items, successfully added: 15000
Thread 44468 processed 12000/25000 items, successfully added: 12000
Thread 27656 processed 12000/25000 items, successfully added: 12000
Thread 40584 processed 12000/25000 items, successfully added: 12000
Thread 44468 processed 13000/25000 items, successfully added: 13000
Thread 35908 processed 16000/25000 items, successfully added: 16000
Thread 40584 processed 13000/25000 items, successfully added: 13000
Thread 27656 processed 13000/25000 items, successfully added: 13000
Thread 44468 processed 14000/25000 items, successfully added: 14000
Thread 35908 processed 17000/25000 items, successfully added: 17000
Thread 27656 processed 14000/25000 items, successfully added: 14000
Thread 40584 processed 14000/25000 items, successfully added: 14000
Thread 44468 processed 15000/25000 items, successfully added: 15000
Thread 35908 processed 18000/25000 items, successfully added: 18000
Thread 44468 processed 16000/25000 items, successfully added: 16000
Thread 27656 processed 15000/25000 items, successfully added: 15000
Thread 40584 processed 15000/25000 items, successfully added: 15000
Thread 35908 processed 19000/25000 items, successfully added: 19000
Thread 44468 processed 17000/25000 items, successfully added: 17000
Thread 27656 processed 16000/25000 items, successfully added: 16000
Thread 40584 processed 16000/25000 items, successfully added: 16000
Thread 35908 processed 20000/25000 items, successfully added: 20000
Thread 44468 processed 18000/25000 items, successfully added: 18000
Thread 27656 processed 17000/25000 items, successfully added: 17000
Thread 40584 processed 17000/25000 items, successfully added: 17000
Thread 35908 processed 21000/25000 items, successfully added: 21000
Thread 44468 processed 19000/25000 items, successfully added: 19000
Thread 27656 processed 18000/25000 items, successfully added: 18000
Thread 40584 processed 18000/25000 items, successfully added: 18000
Thread 35908 processed 22000/25000 items, successfully added: 22000
Thread 44468 processed 20000/25000 items, successfully added: 20000
Thread 40584 processed 19000/25000 items, successfully added: 19000
Thread 35908 processed 23000/25000 items, successfully added: 23000
Thread 27656 processed 19000/25000 items, successfully added: 19000
Thread 44468 processed 21000/25000 items, successfully added: 21000
Thread 35908 processed 24000/25000 items, successfully added: 24000
Thread 44468 processed 22000/25000 items, successfully added: 22000
Thread 35908 processed 25000/25000 items, successfully added: 25000
Thread 35908 completed. Added 25000/25000 items successfully
Thread 40584 processed 20000/25000 items, successfully added: 20000
Thread 44468 processed 23000/25000 items, successfully added: 23000
Thread 27656 processed 20000/25000 items, successfully added: 20000
Thread 44468 processed 24000/25000 items, successfully added: 24000
Thread 40584 processed 21000/25000 items, successfully added: 21000
Thread 44468 processed 25000/25000 items, successfully added: 25000
Thread 44468 completed. Added 25000/25000 items successfully
Thread 27656 processed 21000/25000 items, successfully added: 21000
Thread 40584 processed 22000/25000 items, successfully added: 22000
Thread 27656 processed 22000/25000 items, successfully added: 22000
Thread 40584 processed 23000/25000 items, successfully added: 23000
Thread 40584 processed 24000/25000 items, successfully added: 24000
Thread 27656 processed 23000/25000 items, successfully added: 23000
Thread 27656 processed 24000/25000 items, successfully added: 24000
Thread 40584 processed 25000/25000 items, successfully added: 25000
Thread 40584 completed. Added 25000/25000 items successfully
Thread 27656 processed 25000/25000 items, successfully added: 25000
Thread 27656 completed. Added 25000/25000 items successfully
Parallel insertion took: 71000 ms
Cleaning up threads...
Initial set count: 100000
Starting deep verification...
Verified 1000/100000 items...
Verified 2000/100000 items...
Verified 3000/100000 items...
Verified 4000/100000 items...
Verified 5000/100000 items...
Verified 6000/100000 items...
Verified 7000/100000 items...
Verified 8000/100000 items...
Verified 9000/100000 items...
Verified 10000/100000 items...
Verified 11000/100000 items...
Verified 12000/100000 items...
Verified 13000/100000 items...
Verified 14000/100000 items...
Verified 15000/100000 items...
Verified 16000/100000 items...
Verified 17000/100000 items...
Verified 18000/100000 items...
Verified 19000/100000 items...
Verified 20000/100000 items...
Verified 21000/100000 items...
Verified 22000/100000 items...
Verified 23000/100000 items...
Verified 24000/100000 items...
Verified 25000/100000 items...
Verified 26000/100000 items...
Verified 27000/100000 items...
Verified 28000/100000 items...
Verified 29000/100000 items...
Verified 30000/100000 items...
Verified 31000/100000 items...
Verified 32000/100000 items...
Verified 33000/100000 items...
Verified 34000/100000 items...
Verified 35000/100000 items...
Verified 36000/100000 items...
Verified 37000/100000 items...
Verified 38000/100000 items...
Verified 39000/100000 items...
Verified 40000/100000 items...
Verified 41000/100000 items...
Verified 42000/100000 items...
Verified 43000/100000 items...
Verified 44000/100000 items...
Verified 45000/100000 items...
Verified 46000/100000 items...
Verified 47000/100000 items...
Verified 48000/100000 items...
Verified 49000/100000 items...
Verified 50000/100000 items...
Verified 51000/100000 items...
Verified 52000/100000 items...
Verified 53000/100000 items...
Verified 54000/100000 items...
Verified 55000/100000 items...
Verified 56000/100000 items...
Verified 57000/100000 items...
Verified 58000/100000 items...
Verified 59000/100000 items...
Verified 60000/100000 items...
Verified 61000/100000 items...
Verified 62000/100000 items...
Verified 63000/100000 items...
Verified 64000/100000 items...
Verified 65000/100000 items...
Verified 66000/100000 items...
Verified 67000/100000 items...
Verified 68000/100000 items...
Verified 69000/100000 items...
Verified 70000/100000 items...
Verified 71000/100000 items...
Verified 72000/100000 items...
Verified 73000/100000 items...
Verified 74000/100000 items...
Verified 75000/100000 items...
Verified 76000/100000 items...
Verified 77000/100000 items...
Verified 78000/100000 items...
Verified 79000/100000 items...
Verified 80000/100000 items...
Verified 81000/100000 items...
Verified 82000/100000 items...
Verified 83000/100000 items...
Verified 84000/100000 items...
Verified 85000/100000 items...
Verified 86000/100000 items...
Verified 87000/100000 items...
Verified 88000/100000 items...
Verified 89000/100000 items...
Verified 90000/100000 items...
Verified 91000/100000 items...
Verified 92000/100000 items...
Verified 93000/100000 items...
Verified 94000/100000 items...
Verified 95000/100000 items...
Verified 96000/100000 items...
Verified 97000/100000 items...
Verified 98000/100000 items...
Verified 99000/100000 items...
Verified 100000/100000 items...
Deep verify summary: Total lost keys: 0
Final set count: 100000
Total test time: 113344 ms
=== Aggressive Collision Test Complete ===
21:49:17.677 - TearDown starting...
21:49:17.685 - TearDown completed
.21:49:17.685 - SetUp starting...
21:49:17.685 - SetUp completed
21:49:17.685 - Test12_Iterator starting...
21:49:17.685 - Test12_Iterator completed
21:49:17.685 - TearDown starting...
21:49:17.685 - TearDown completed
.21:49:17.685 - SetUp starting...
21:49:17.685 - SetUp completed
21:49:17.685 - Test13_IteratorStress starting...
21:49:17.685 - Iteration 1 found 0 items after 0 ms
21:49:17.685 - Iteration 2 found 0 items after 0 ms
21:49:17.685 - Iteration 3 found 0 items after 0 ms
21:49:17.685 - Iteration 4 found 0 items after 0 ms
21:49:17.685 - Iteration 5 found 0 items after 0 ms
21:49:17.685 - Iteration 6 found 0 items after 0 ms
21:49:17.685 - Iteration 7 found 0 items after 0 ms
21:49:17.685 - Iteration 8 found 0 items after 0 ms
21:49:17.685 - Iteration 9 found 0 items after 0 ms
21:49:17.685 - Iteration 10 found 0 items after 0 ms
21:49:25.753 - Final iteration found 2000 items
21:49:25.753 - Test13_IteratorStress completed after 8062 ms
21:49:25.753 - TearDown starting...
21:49:25.753 - TearDown completed
.21:49:25.753 - SetUp starting...
21:49:25.754 - SetUp completed
21:49:25.754 - Test14_LockingMechanism starting...
21:49:56.453 - Lock test took 30699 ms, 4000 successful locks across 4 threads
21:49:56.453 - TearDown starting...
21:49:56.453 - TearDown completed
..........Test took 30828 ms with 4 threads doing 10 iterations each
.Lock test took 31354 ms, 4000 successful locks across 4 threads
. Time:12:40.827 N:105 E:0 F:0 I:0
  TThreadSafeListTest Time:31.188 N:46 E:0 F:0 I:0
    00.000  Test01_Creation
    00.000  Test02_CreationWithNilComparer
    00.000  Test03_AddInteger
    00.000  Test04_AddString
    00.000  Test05_Delete
    00.000  Test06_IndexOf
    00.000  Test07_FirstLast
    00.000  Test08_Replace
    00.000  Test09_Sort
    00.000  Test10_SortDescending
    00.000  Test11_IsSorted
    00.000  Test12_EmptyList
    00.000  Test13_SingleElement
    00.000  Test14_DuplicateElements
    00.000  Test15_Boundaries
    00.000  Test16_BooleanList
    00.000  Test17_RealList
    00.006  Test18_MultiThreadAccess
    00.016  Test19_LargeDataSet
    00.000  Test20_RandomOperations
    00.114  Test21_LargeDataSetSortingPerformance
    00.000  Test22_Iterator
    00.004  Test23_IteratorThreadSafety
    00.000  Test24_IteratorExceptionSafety
    31.046  Test25_LockingMechanism
    00.000  Test26_Capacity
    00.000  Test27_TrimExcess
    00.000  Test28_ToArray
    00.000  Test29_FromArray
    00.000  Test30_AddRange
    00.000  Test31_AddRangeFromCollection
    00.000  Test32_InsertRange
    00.000  Test33_InsertRangeFromCollection
    00.000  Test34_DeleteRange
    00.000  Test35_Contains
    00.000  Test36_IndexOfItemWithStart
    00.000  Test37_IndexOfItemWithStartAndCount
    00.000  Test38_LastIndexOf
    00.000  Test39_LastIndexOfWithStart
    00.000  Test40_LastIndexOfWithStartAndCount
    00.000  Test41_Insert
    00.000  Test42_Exchange
    00.000  Test43_Move
    00.002  Test44_Reverse
    00.000  Test45_Extract
    00.000  Test46_ExtractAt
  TThreadSafeListStudentTest Time:00.691 N:2 E:0 F:0 I:0
    00.381  TestLargeStudentSortByName
    00.310  TestLargeStudentSortById
  TThreadSafeDictionaryTest Time:31.971 N:32 E:0 F:0 I:0
    00.000  Test1_Creation
    00.000  Test2_Add
    00.000  Test3_AddDuplicate
    00.000  Test4_GetItem
    00.000  Test5_Remove
    00.000  Test6_AddOrSetValue
    00.000  Test7_Clear
    00.000  Test8_Count
    00.000  Test9_FirstLast
    00.000  Test10_EmptyDictionary
    00.000  Test11_LargeDataSet
    00.000  Test12_NilValues
    00.001  Test13_Boundaries
    00.042  Test14_MultiThreadAccess
    00.011  Test15_ConcurrentOperations
    00.000  Test16_HashCollisions
    00.372  Test17_LargeDataSetPerformance
    00.116  Test18_HashingPerformance
    00.000  Test19_InitialCapacity
    00.000  Test20_ManualResize
    00.000  Test21_ResizeWithData
    00.001  Test22_ResizeUnderflow
    00.000  Test23_BucketCount
    00.000  Test24_IteratorBasic
    00.000  Test25_IteratorEmpty
    00.000  Test26_IteratorModification
    00.000  Test27_MultipleIterators
    00.000  Test28_IteratorReset
    31.427  Test29_LockingMechanism
    00.000  Test30_CompoundKeyBasic
    00.000  Test31_CompoundKeyIteration
    00.001  Test32_CustomConstructors
  TThreadSafeHashSetTest Time:12:34.793 N:14 E:0 F:0 I:0
    00.000  Test1_BasicOperations
    00.000  Test2_SimpleAdd
    00.000  Test3_SimpleRemove
    00.000  Test4_Duplicates
    00.000  Test5_Clear
    00.000  Test6_StudentBasic
    01.562  Test7_LargeDataSet
    00.131  Test8_ConcurrentAccess
    00.157  Test9_StressTest
    00.810  Test10_HashCollisions
    53.365  Test11_AggressiveCollisions
    00.000  Test12_Iterator
    08.068  Test13_IteratorStress
    30.700  Test14_LockingMechanism
  TThreadSafeDequeTests Time:02.184 N:11 E:0 F:0 I:0
    00.000  TestPushFrontAndPopFront
    00.000  TestPushBackAndPopBack
    00.000  TestMixedOperations
    00.000  TestClear
    00.000  TestPeekOperations
    00.000  TestTryOperations
    00.002  TestIsEmpty
    00.000  TestToArray
    00.000  TestPushRange
    30.828  TestMultiThreadPushPop
    31.354  TestLockingMechanism

Number of run tests: 105
Number of errors:    0
Number of failures:  0



Tests completed.
Time: 21:50:58.640
```

