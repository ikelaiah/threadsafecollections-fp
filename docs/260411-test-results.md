PS C:\path\to\threadsafecollections-fp\tests> .\TestRunner.exe -a --format=plain
Starting test runner...
Time: 22:13:51.593
Running tests...
Multi-thread test completed in 16 ms
Large dataset operations took: 125 ms
Integer Test:
Sorting 100000 integers took: 47 ms
String Test:
Sorting 100000 strings took: 235 ms
Lock test took 30549 ms, 4000 successful locks across 4 threads
Student Sort By Name Test:
Before sort (first 3):
Name: PEdLAUTYEZ, ID: 100000
Name: kNmOXeQXdX, ID: 99999
Name: IGfPOTEEaH, ID: 99998

Sorting 100000 students by name took: 312 ms
After sort (first 3):
Name: AAAIZoGZhF, ID: 21285
Name: AABHEeWTuq, ID: 15586
Name: AACDQHyWxF, ID: 42088

Name sort verification: TRUE

Student Sort By ID Test:
Before sort (first 3):
ID: 152410, Name: Student0
ID: 78274, Name: Student1
ID: 182601, Name: Student2

Sorting 100000 students by ID took: 234 ms
After sort (first 3):
ID: 2, Name: Student41881
ID: 4, Name: Student21401
ID: 6, Name: Student87569

ID sort verification: TRUE

Setting up test...
Setup complete
Starting TestCreation
Running test #1
Completed TestCreation
Setting up test...
Setup complete
Starting TestAdd
Running test #2
Completed TestAdd
Setting up test...
Setup complete
Starting TestAddDuplicate
Running test #3
Completed TestAddDuplicate
Setting up test...
Setup complete
Starting TestGetItem
Running test #4
Completed TestGetItem
Setting up test...
Setup complete
Starting TestRemove
Running test #5
Completed TestRemove
Setting up test...
Setup complete
Starting TestAddOrSetValue
Running test #6
Completed TestAddOrSetValue
Setting up test...
Setup complete
Starting TestClear
Running test #7
Completed TestClear
Setting up test...
Setup complete
Starting TestCount
Running test #8
Completed TestCount
Setting up test...
Setup complete
Starting TestFirstLast
Running test #9
Completed TestFirstLast
Setting up test...
Setup complete
Starting Test10_EmptyDictionary
Running test #10
Completed TestEmptyDictionary
Setting up test...
Setup complete
Running test #11
Setting up test...
Setup complete
Setting up test...
Setup complete
Setting up test...
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
Setting up test...
Setup complete
Starting Test15_ConcurrentOperations
TestConcurrentOperations completed
Setting up test...
Setup complete
Setting up test...
Setup complete
Adding 100000 items took: 1265 ms
Finding 100000 items took: 8781 ms
Setting up test...
Setup complete
Adding 100000 items took: 672 ms
Finding 100000 items took: 63 ms
Setting up test...
Setup complete
Starting Test19_InitialCapacity
Running test #12
Completed TestInitialCapacity
Setting up test...
Setup complete
Starting Test20_ManualResize
Running test #13
Completed TestManualResize
Setting up test...
Setup complete
Starting Test21_ResizeWithData
Running test #14
Completed TestResizeWithData
Setting up test...
Setup complete
Starting Test22_ResizeUnderflow
Running test #15
Completed TestResizeUnderflow
Setting up test...
Setup complete
Starting Test23_BucketCount
Running test #16
Completed TestBucketCount
Setting up test...
Setup complete
Starting Test24_IteratorBasic
Running test #17
Setting up test...
Setup complete
Starting Test25_IteratorEmpty
Running test #18
Setting up test...
Setup complete
Starting Test26_IteratorModification
Running test #19
Setting up test...
Setup complete
Starting Test27_MultipleIterators
Running test #20
Setting up test...
Setup complete
Starting Test28_IteratorReset
Running test #21
Setting up test...
Setup complete
Lock test took 32432 ms, 4000 successful locks across 4 threads
Setting up test...
Setup complete
Starting Test30_CompoundKeyBasic
Running test #22
TestCompoundKeyBasic passed
Setting up test...
Setup complete
Starting Test31_CompoundKeyIteration
Running test #23
TestCompoundKeyIteration passed
Setting up test...
Setup complete
Starting Test32_CustomConstructors
Running test #24
TestCustomConstructors passed
Setting up test...
Setup complete
Starting Test33_GetKeysAndValues
Running test #25
TestGetKeysAndValues completed
Setting up test...
Setup complete
Starting Test34_TrimExcess
Running test #26
TestTrimExcess completed
Setting up test...
Setup complete
Starting Test35_TryAdd
Running test #27
TestTryAdd completed
Setting up test...
Setup complete
Starting Test36_AddRange
Running test #28
TestAddRange completed
Setting up test...
Setup complete
Starting Test37_ToArray
Running test #29
TestToArray completed
Setting up test...
Setup complete
Starting Test38_ContainsValue
Running test #30
TestContainsValue completed
22:15:25.320 - SetUp starting...
22:15:25.320 - SetUp completed
22:15:25.320 - Test1_BasicOperations starting...
22:15:25.827 - Test1_BasicOperations completed
22:15:25.827 - TearDown starting...
22:15:25.846 - TearDown completed
22:15:25.846 - SetUp starting...
22:15:25.850 - SetUp completed
22:15:25.851 - Test2_SimpleAdd starting...
22:15:26.422 - Test2_SimpleAdd completed
22:15:26.422 - TearDown starting...
22:15:26.467 - TearDown completed
22:15:26.467 - SetUp starting...
22:15:26.467 - SetUp completed
22:15:26.467 - Test3_SimpleRemove starting...
22:15:26.467 - Test3_SimpleRemove completed
22:15:26.467 - TearDown starting...
22:15:26.467 - TearDown completed
22:15:26.467 - SetUp starting...
22:15:26.467 - SetUp completed
22:15:26.467 - Test4_Duplicates starting...
22:15:26.467 - Test4_Duplicates completed
22:15:26.467 - TearDown starting...
22:15:26.467 - TearDown completed
22:15:26.467 - SetUp starting...
22:15:26.467 - SetUp completed
22:15:26.467 - Test5_Clear starting...
22:15:26.474 - Test5_Clear completed
22:15:26.474 - TearDown starting...
22:15:26.474 - TearDown completed
22:15:26.474 - SetUp starting...
22:15:26.474 - SetUp completed
22:15:26.474 - Test6_StudentBasic starting...
22:15:26.474 - Test6_StudentBasic completed
22:15:26.474 - TearDown starting...
22:15:26.474 - TearDown completed
22:15:26.474 - SetUp starting...
22:15:26.474 - SetUp completed
22:15:26.474 - Test7_LargeDataSet starting...
22:15:26.474 - Added batch 1/100 (100 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.492 - Added batch 2/100 (200 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.500 - Added batch 3/100 (300 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.509 - Added batch 4/100 (400 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.522 - Added batch 5/100 (500 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.534 - Added batch 6/100 (600 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.543 - Added batch 7/100 (700 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.556 - Added batch 8/100 (800 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.563 - Added batch 9/100 (900 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.577 - Added batch 10/100 (1000 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.589 - Added batch 11/100 (1100 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.602 - Added batch 12/100 (1200 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.612 - Added batch 13/100 (1300 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.634 - Added batch 14/100 (1400 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.651 - Added batch 15/100 (1500 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.661 - Added batch 16/100 (1600 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.676 - Added batch 17/100 (1700 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.688 - Added batch 18/100 (1800 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.701 - Added batch 19/100 (1900 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.710 - Added batch 20/100 (2000 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.718 - Added batch 21/100 (2100 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.726 - Added batch 22/100 (2200 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.734 - Added batch 23/100 (2300 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.743 - Added batch 24/100 (2400 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.756 - Added batch 25/100 (2500 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.762 - Added batch 26/100 (2600 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.771 - Added batch 27/100 (2700 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.784 - Added batch 28/100 (2800 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.801 - Added batch 29/100 (2900 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.818 - Added batch 30/100 (3000 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.826 - Added batch 31/100 (3100 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.835 - Added batch 32/100 (3200 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.842 - Added batch 33/100 (3300 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.852 - Added batch 34/100 (3400 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.868 - Added batch 35/100 (3500 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.875 - Added batch 36/100 (3600 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.884 - Added batch 37/100 (3700 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.901 - Added batch 38/100 (3800 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.908 - Added batch 39/100 (3900 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.918 - Added batch 40/100 (4000 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.928 - Added batch 41/100 (4100 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.935 - Added batch 42/100 (4200 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.945 - Added batch 43/100 (4300 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.957 - Added batch 44/100 (4400 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.968 - Added batch 45/100 (4500 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.979 - Added batch 46/100 (4600 items total) - Batch time: 0 ms, Total time: 0 ms
22:15:26.989 - Added batch 47/100 (4700 items total) - Batch time: 16 ms, Total time: 16 ms
22:15:27.005 - Added batch 48/100 (4800 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.018 - Added batch 49/100 (4900 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.029 - Added batch 50/100 (5000 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.035 - Added batch 51/100 (5100 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.045 - Added batch 52/100 (5200 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.051 - Added batch 53/100 (5300 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.061 - Added batch 54/100 (5400 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.071 - Added batch 55/100 (5500 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.078 - Added batch 56/100 (5600 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.087 - Added batch 57/100 (5700 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.095 - Added batch 58/100 (5800 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.104 - Added batch 59/100 (5900 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.112 - Added batch 60/100 (6000 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.121 - Added batch 61/100 (6100 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.138 - Added batch 62/100 (6200 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.147 - Added batch 63/100 (6300 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.163 - Added batch 64/100 (6400 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.172 - Added batch 65/100 (6500 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.181 - Added batch 66/100 (6600 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.196 - Added batch 67/100 (6700 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.206 - Added batch 68/100 (6800 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.215 - Added batch 69/100 (6900 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.230 - Added batch 70/100 (7000 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.234 - Added batch 71/100 (7100 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.247 - Added batch 72/100 (7200 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.251 - Added batch 73/100 (7300 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.259 - Added batch 74/100 (7400 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.284 - Added batch 75/100 (7500 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.294 - Added batch 76/100 (7600 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.302 - Added batch 77/100 (7700 items total) - Batch time: 0 ms, Total time: 16 ms
22:15:27.317 - Added batch 78/100 (7800 items total) - Batch time: 16 ms, Total time: 32 ms
22:15:27.324 - Added batch 79/100 (7900 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.335 - Added batch 80/100 (8000 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.346 - Added batch 81/100 (8100 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.351 - Added batch 82/100 (8200 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.361 - Added batch 83/100 (8300 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.369 - Added batch 84/100 (8400 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.377 - Added batch 85/100 (8500 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.386 - Added batch 86/100 (8600 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.402 - Added batch 87/100 (8700 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.404 - Added batch 88/100 (8800 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.422 - Added batch 89/100 (8900 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.437 - Added batch 90/100 (9000 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.447 - Added batch 91/100 (9100 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.455 - Added batch 92/100 (9200 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.463 - Added batch 93/100 (9300 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.481 - Added batch 94/100 (9400 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.490 - Added batch 95/100 (9500 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.506 - Added batch 96/100 (9600 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.521 - Added batch 97/100 (9700 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.530 - Added batch 98/100 (9800 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.534 - Added batch 99/100 (9900 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.542 - Added batch 100/100 (10000 items total) - Batch time: 0 ms, Total time: 32 ms
22:15:27.550 - Total time for adding 10000 items: 1063 ms
22:15:27.550 - Actual processing time (excluding logging): 32 ms
22:15:27.551 - Verification of 10000 items took: 15 ms
22:15:27.551 - TearDown starting...
22:15:27.571 - TearDown completed
22:15:27.571 - SetUp starting...
22:15:27.571 - SetUp completed
22:15:27.571 - Test8_ConcurrentAccess starting...
22:15:27.571 - Created thread 0
22:15:27.571 - Created thread 1
22:15:27.571 - Created thread 2
22:15:27.571 - Created thread 3
22:15:27.675 - Started thread 0 at 94 ms
22:15:27.675 - Started thread 1 at 94 ms
22:15:27.675 - Started thread 2 at 94 ms
22:15:27.675 - Started thread 3 at 94 ms
22:15:27.788 - Thread 0 completed at 219 ms
22:15:27.792 - Thread 1 completed at 219 ms
22:15:27.797 - Thread 2 completed at 219 ms
22:15:27.797 - Thread 3 completed at 219 ms
22:15:27.797 - Concurrent test total time: 219 ms
22:15:27.797 - Final item count: 9903
22:15:27.799 - TearDown starting...
22:15:27.816 - TearDown completed
22:15:27.816 - SetUp starting...
22:15:27.816 - SetUp completed
22:15:27.816 - Test9_StressTest starting...
22:15:27.939 - Completed 10000/100000 iterations (10%) - Time: 125 ms
22:15:28.069 - Completed 20000/100000 iterations (20%) - Time: 266 ms
22:15:28.204 - Completed 30000/100000 iterations (30%) - Time: 391 ms
22:15:28.363 - Completed 40000/100000 iterations (40%) - Time: 563 ms
22:15:28.509 - Completed 50000/100000 iterations (50%) - Time: 703 ms
22:15:28.621 - Completed 60000/100000 iterations (60%) - Time: 813 ms
22:15:28.753 - Completed 70000/100000 iterations (70%) - Time: 938 ms
22:15:28.887 - Completed 80000/100000 iterations (80%) - Time: 1078 ms
22:15:29.016 - Completed 90000/100000 iterations (90%) - Time: 1203 ms
22:15:29.154 - Completed 100000/100000 iterations (100%) - Time: 1344 ms
22:15:29.170 - Stress test completed in 1360 ms
22:15:29.170 - Final counts - Int: 25000, Str: 25000, Bool: 0, Student: 25000
22:15:29.170 - TearDown starting...
22:15:29.383 - TearDown completed
22:15:29.383 - SetUp starting...
22:15:29.383 - SetUp completed
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
Total test time: 9094 ms
=== Hash Collision Test Complete ===
22:15:38.480 - TearDown starting...
22:15:38.522 - TearDown completed
22:15:38.522 - SetUp starting...
22:15:38.522 - SetUp completed
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
Thread 72332 starting, range 75000 to 99999
Thread 49668 starting, range 25000 to 49999
Thread 72252 starting, range 0 to 24999
Thread 43348 starting, range 50000 to 74999
Thread 72332 processed 1000/25000 items, successfully added: 1000
Thread 49668 processed 1000/25000 items, successfully added: 1000
Thread 43348 processed 1000/25000 items, successfully added: 1000
Thread 72332 processed 2000/25000 items, successfully added: 2000
Thread 72252 processed 1000/25000 items, successfully added: 1000
Thread 49668 processed 2000/25000 items, successfully added: 2000
Thread 43348 processed 2000/25000 items, successfully added: 2000
Thread 72332 processed 3000/25000 items, successfully added: 3000
Thread 72252 processed 2000/25000 items, successfully added: 2000
Thread 43348 processed 3000/25000 items, successfully added: 3000
Thread 72332 processed 4000/25000 items, successfully added: 4000
Thread 49668 processed 3000/25000 items, successfully added: 3000
Thread 72252 processed 3000/25000 items, successfully added: 3000
Thread 72332 processed 5000/25000 items, successfully added: 5000
Thread 43348 processed 4000/25000 items, successfully added: 4000
Thread 72332 processed 6000/25000 items, successfully added: 6000
Thread 49668 processed 4000/25000 items, successfully added: 4000
Thread 72332 processed 7000/25000 items, successfully added: 7000
Thread 72252 processed 4000/25000 items, successfully added: 4000
Thread 43348 processed 5000/25000 items, successfully added: 5000
Thread 72332 processed 8000/25000 items, successfully added: 8000
Thread 49668 processed 5000/25000 items, successfully added: 5000
Thread 72332 processed 9000/25000 items, successfully added: 9000
Thread 72252 processed 5000/25000 items, successfully added: 5000
Thread 43348 processed 6000/25000 items, successfully added: 6000
Thread 49668 processed 6000/25000 items, successfully added: 6000
Thread 72332 processed 10000/25000 items, successfully added: 10000
Thread 43348 processed 7000/25000 items, successfully added: 7000
Thread 72252 processed 6000/25000 items, successfully added: 6000
Thread 72332 processed 11000/25000 items, successfully added: 11000
Thread 72252 processed 7000/25000 items, successfully added: 7000
Thread 43348 processed 8000/25000 items, successfully added: 8000
Thread 49668 processed 7000/25000 items, successfully added: 7000
Thread 72332 processed 12000/25000 items, successfully added: 12000
Thread 72332 processed 13000/25000 items, successfully added: 13000
Thread 43348 processed 9000/25000 items, successfully added: 9000
Thread 72252 processed 8000/25000 items, successfully added: 8000
Thread 49668 processed 8000/25000 items, successfully added: 8000
Thread 72332 processed 14000/25000 items, successfully added: 14000
Thread 72252 processed 9000/25000 items, successfully added: 9000
Thread 43348 processed 10000/25000 items, successfully added: 10000
Thread 72332 processed 15000/25000 items, successfully added: 15000
Thread 49668 processed 9000/25000 items, successfully added: 9000
Thread 43348 processed 11000/25000 items, successfully added: 11000
Thread 72252 processed 10000/25000 items, successfully added: 10000
Thread 72332 processed 16000/25000 items, successfully added: 16000
Thread 49668 processed 10000/25000 items, successfully added: 10000
Thread 72332 processed 17000/25000 items, successfully added: 17000
Thread 43348 processed 12000/25000 items, successfully added: 12000
Thread 72252 processed 11000/25000 items, successfully added: 11000
Thread 49668 processed 11000/25000 items, successfully added: 11000
Thread 72332 processed 18000/25000 items, successfully added: 18000
Thread 72332 processed 19000/25000 items, successfully added: 19000
Thread 43348 processed 13000/25000 items, successfully added: 13000
Thread 72252 processed 12000/25000 items, successfully added: 12000
Thread 49668 processed 12000/25000 items, successfully added: 12000
Thread 72332 processed 20000/25000 items, successfully added: 20000
Thread 43348 processed 14000/25000 items, successfully added: 14000
Thread 72332 processed 21000/25000 items, successfully added: 21000
Thread 72252 processed 13000/25000 items, successfully added: 13000
Thread 49668 processed 13000/25000 items, successfully added: 13000
Thread 72332 processed 22000/25000 items, successfully added: 22000
Thread 72252 processed 14000/25000 items, successfully added: 14000
Thread 43348 processed 15000/25000 items, successfully added: 15000
Thread 49668 processed 14000/25000 items, successfully added: 14000
Thread 72332 processed 23000/25000 items, successfully added: 23000
Thread 72252 processed 15000/25000 items, successfully added: 15000
Thread 43348 processed 16000/25000 items, successfully added: 16000
Thread 72332 processed 24000/25000 items, successfully added: 24000
Thread 72252 processed 16000/25000 items, successfully added: 16000
Thread 49668 processed 15000/25000 items, successfully added: 15000
Thread 43348 processed 17000/25000 items, successfully added: 17000
Thread 72332 processed 25000/25000 items, successfully added: 25000
Thread 72332 completed. Added 25000/25000 items successfully
Thread 72252 processed 17000/25000 items, successfully added: 17000
Thread 49668 processed 16000/25000 items, successfully added: 16000
Thread 43348 processed 18000/25000 items, successfully added: 18000
Thread 49668 processed 17000/25000 items, successfully added: 17000
Thread 72252 processed 18000/25000 items, successfully added: 18000
Thread 43348 processed 19000/25000 items, successfully added: 19000
Thread 49668 processed 18000/25000 items, successfully added: 18000
Thread 72252 processed 19000/25000 items, successfully added: 19000
Thread 43348 processed 20000/25000 items, successfully added: 20000
Thread 72252 processed 20000/25000 items, successfully added: 20000
Thread 49668 processed 19000/25000 items, successfully added: 19000
Thread 43348 processed 21000/25000 items, successfully added: 21000
Thread 49668 processed 20000/25000 items, successfully added: 20000
Thread 43348 processed 22000/25000 items, successfully added: 22000
Thread 72252 processed 21000/25000 items, successfully added: 21000
Thread 43348 processed 23000/25000 items, successfully added: 23000
Thread 49668 processed 21000/25000 items, successfully added: 21000
Thread 72252 processed 22000/25000 items, successfully added: 22000
Thread 43348 processed 24000/25000 items, successfully added: 24000
Thread 49668 processed 22000/25000 items, successfully added: 22000
Thread 72252 processed 23000/25000 items, successfully added: 23000
Thread 43348 processed 25000/25000 items, successfully added: 25000
Thread 43348 completed. Added 25000/25000 items successfully
Thread 49668 processed 23000/25000 items, successfully added: 23000
Thread 72252 processed 24000/25000 items, successfully added: 24000
Thread 49668 processed 24000/25000 items, successfully added: 24000
Thread 72252 processed 25000/25000 items, successfully added: 25000
Thread 72252 completed. Added 25000/25000 items successfully
Thread 49668 processed 25000/25000 items, successfully added: 25000
Thread 49668 completed. Added 25000/25000 items successfully
Parallel insertion took: 392797 ms
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
Total test time: 764391 ms
=== Aggressive Collision Test Complete ===
22:28:23.078 - TearDown starting...
22:28:23.400 - TearDown completed
22:28:23.401 - SetUp starting...
22:28:23.401 - SetUp completed
22:28:23.401 - Test12_Iterator starting...
22:28:23.433 - Test12_Iterator completed
22:28:23.433 - TearDown starting...
22:28:23.435 - TearDown completed
22:28:23.435 - SetUp starting...
22:28:23.435 - SetUp completed
22:28:23.435 - Test13_IteratorStress starting...
22:28:23.440 - Iteration 1 found 0 items after 0 ms
22:28:23.479 - Iteration 2 found 0 items after 47 ms
22:28:23.479 - Iteration 3 found 0 items after 47 ms
22:28:23.479 - Iteration 4 found 0 items after 47 ms
22:28:23.479 - Iteration 5 found 1 items after 47 ms
22:28:23.479 - Iteration 6 found 1 items after 47 ms
22:28:23.479 - Iteration 7 found 1 items after 47 ms
22:28:23.483 - Iteration 8 found 1 items after 47 ms
22:28:23.483 - Iteration 9 found 1 items after 47 ms
22:28:23.483 - Iteration 10 found 2 items after 47 ms
22:28:31.652 - Final iteration found 2000 items
22:28:31.652 - Test13_IteratorStress completed after 8219 ms
22:28:31.652 - TearDown starting...
22:28:31.652 - TearDown completed
22:28:31.652 - SetUp starting...
22:28:31.652 - SetUp completed
22:28:31.652 - Test14_LockingMechanism starting...
22:29:03.099 - Lock test took 31447 ms, 4000 successful locks across 4 threads
22:29:03.099 - TearDown starting...
22:29:03.099 - TearDown completed
22:29:03.099 - SetUp starting...
22:29:03.099 - SetUp completed
22:29:03.099 - Test15_AddRange_Array starting...
22:29:05.800 - Test15_AddRange_Array completed
22:29:05.800 - TearDown starting...
22:29:06.001 - TearDown completed
22:29:06.001 - SetUp starting...
22:29:06.001 - SetUp completed
22:29:06.001 - Test16_AddRange_Collection starting...
22:29:09.445 - Test16_AddRange_Collection completed
22:29:09.461 - TearDown starting...
22:29:09.625 - TearDown completed
22:29:09.625 - SetUp starting...
22:29:09.625 - SetUp completed
22:29:09.625 - Test17_RemoveRange starting...
22:29:23.338 - Test17_RemoveRange completed
22:29:23.340 - TearDown starting...
22:29:23.447 - TearDown completed
22:29:23.447 - SetUp starting...
22:29:23.447 - SetUp completed
22:29:23.447 - Test18_SetOperations starting...
22:29:23.447 - Test18_SetOperations completed
22:29:23.447 - TearDown starting...
22:29:23.447 - TearDown completed
22:29:23.453 - SetUp starting...
22:29:23.454 - SetUp completed
22:29:23.454 - Test19_TryGetValue starting...
22:29:23.454 - Test19_TryGetValue completed
22:29:23.455 - TearDown starting...
22:29:23.455 - TearDown completed
Test took 31372 ms with 4 threads doing 10 iterations each
Lock test took 30609 ms, 4000 successful locks across 4 threads
 Time:12:33.844 N:116 E:0 F:0 I:0
  TThreadSafeListTest Time:39.209 N:46 E:0 F:0 I:0
    00.000  Test01_Creation
    00.000  Test02_CreationWithNilComparer
    00.010  Test03_AddInteger
    00.000  Test04_AddString
    00.000  Test05_Delete
    00.000  Test06_IndexOf
    00.000  Test07_FirstLast
    00.000  Test08_Replace
    00.000  Test09_Sort
    00.000  Test10_SortDescending
    00.000  Test11_IsSorted
    00.003  Test12_EmptyList
    00.000  Test13_SingleElement
    00.000  Test14_DuplicateElements
    00.000  Test15_Boundaries
    00.000  Test16_BooleanList
    00.000  Test17_RealList
    00.022  Test18_MultiThreadAccess
    00.130  Test19_LargeDataSet
    00.007  Test20_RandomOperations
    08.431  Test21_LargeDataSetSortingPerformance
    00.000  Test22_Iterator
    00.039  Test23_IteratorThreadSafety
    00.002  Test24_IteratorExceptionSafety
    30.549  Test25_LockingMechanism
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
    00.008  Test42_Exchange
    00.000  Test43_Move
    00.000  Test44_Reverse
    00.000  Test45_Extract
    00.000  Test46_ExtractAt
  TThreadSafeListStudentTest Time:09.581 N:2 E:0 F:0 I:0
    07.150  TestLargeStudentSortByName
    02.431  TestLargeStudentSortById
  TThreadSafeDictionaryTest Time:44.937 N:38 E:0 F:0 I:0
    00.017  Test1_Creation
    00.001  Test2_Add
    00.000  Test3_AddDuplicate
    00.000  Test4_GetItem
    00.003  Test5_Remove
    00.000  Test6_AddOrSetValue
    00.004  Test7_Clear
    00.002  Test8_Count
    00.000  Test9_FirstLast
    00.000  Test10_EmptyDictionary
    00.006  Test11_LargeDataSet
    00.001  Test12_NilValues
    00.004  Test13_Boundaries
    00.200  Test14_MultiThreadAccess
    00.034  Test15_ConcurrentOperations
    00.000  Test16_HashCollisions
    10.442  Test17_LargeDataSetPerformance
    01.741  Test18_HashingPerformance
    00.002  Test19_InitialCapacity
    00.000  Test20_ManualResize
    00.000  Test21_ResizeWithData
    00.011  Test22_ResizeUnderflow
    00.003  Test23_BucketCount
    00.002  Test24_IteratorBasic
    00.000  Test25_IteratorEmpty
    00.003  Test26_IteratorModification
    00.002  Test27_MultipleIterators
    00.003  Test28_IteratorReset
    32.432  Test29_LockingMechanism
    00.000  Test30_CompoundKeyBasic
    00.000  Test31_CompoundKeyIteration
    00.000  Test32_CustomConstructors
    00.010  Test33_GetKeysAndValues
    00.003  Test34_TrimExcess
    00.001  Test35_TryAdd
    00.003  Test36_AddRange
    00.001  Test37_ToArray
    00.000  Test38_ContainsValue
  TThreadSafeHashSetTest Time:12:58.135 N:19 E:0 F:0 I:0
    00.526  Test1_BasicOperations
    00.621  Test2_SimpleAdd
    00.000  Test3_SimpleRemove
    00.000  Test4_Duplicates
    00.007  Test5_Clear
    00.000  Test6_StudentBasic
    01.097  Test7_LargeDataSet
    00.245  Test8_ConcurrentAccess
    01.567  Test9_StressTest
    09.139  Test10_HashCollisions
    12:44.879  Test11_AggressiveCollisions
    00.034  Test12_Iterator
    08.217  Test13_IteratorStress
    31.447  Test14_LockingMechanism
    02.902  Test15_AddRange_Array
    03.624  Test16_AddRange_Collection
    13.822  Test17_RemoveRange
    00.006  Test18_SetOperations
    00.002  Test19_TryGetValue
  TThreadSafeDequeTests Time:01.982 N:11 E:0 F:0 I:0
    00.000  TestPushFrontAndPopFront
    00.000  TestPushBackAndPopBack
    00.000  TestMixedOperations
    00.000  TestClear
    00.000  TestPeekOperations
    00.000  TestTryOperations
    00.000  TestIsEmpty
    00.000  TestToArray
    00.000  TestPushRange
    31.372  TestMultiThreadPushPop
    30.610  TestLockingMechanism

Number of run tests: 116
Number of errors:    0
Number of failures:  0



Tests completed.
Time: 22:30:25.447
Heap dump by heaptrc unit of C:\path\to\threadsafecollections-fp\tests\TestRunner.exe
4713430 memory blocks allocated : 318702934/342070304
4713430 memory blocks freed     : 318702934/342070304
0 unfreed memory blocks : 0
True heap size : 2162688 (256 used in System startup)
True free heap : 2162432
PS C:\path\to\threadsafecollections-fp\tests> 