# Test Date: 2025-01-03 17:09

```bash
> .\TestRunner.exe -a --format=plain
Starting test runner...
Time: 17:05:09.640
Running tests...
Multi-thread test completed in 0 ms
Large dataset operations took: 16 ms
Integer Test:
Sorting 100000 integers took: 15 ms
String Test:
Sorting 100000 strings took: 32 ms
Lock test took 31501 ms, 4000 successful locks across 4 threads
Student Sort By Name Test:
Before sort (first 3):
Name: GDSrvLsaoJ, ID: 100000
Name: hyhiSxyNiz, ID: 99999
Name: pEBPhHIHLk, ID: 99998

Sorting 100000 students by name took: 78 ms
After sort (first 3):
Name: AAAUTejRCP, ID: 37536
Name: AAAYuidOKU, ID: 78051
Name: AABJQOsINP, ID: 19856

Name sort verification: TRUE

Student Sort By ID Test:
Before sort (first 3):
ID: 158980, Name: Student0
ID: 151942, Name: Student1
ID: 18718, Name: Student2

Sorting 100000 students by ID took: 31 ms
After sort (first 3):
ID: 0, Name: Student89306
ID: 0, Name: Student98755
ID: 1, Name: Student30518

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
Adding 100000 items took: 16 ms
Finding 100000 items took: 94 ms
Setting up test...
Setup complete
Adding 100000 items took: 16 ms
Finding 100000 items took: 15 ms
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
Lock test took 31210 ms, 4000 successful locks across 4 threads
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
17:06:12.952 - SetUp starting...
17:06:12.952 - SetUp completed
17:06:12.952 - Test1_BasicOperations starting...
17:06:12.958 - Test1_BasicOperations completed
17:06:12.958 - TearDown starting...
17:06:12.958 - TearDown completed
17:06:12.958 - SetUp starting...
17:06:12.958 - SetUp completed
17:06:12.958 - Test2_SimpleAdd starting...
17:06:12.967 - Test2_SimpleAdd completed
17:06:12.967 - TearDown starting...
17:06:12.968 - TearDown completed
17:06:12.968 - SetUp starting...
17:06:12.968 - SetUp completed
17:06:12.968 - Test3_SimpleRemove starting...
17:06:12.969 - Test3_SimpleRemove completed
17:06:12.969 - TearDown starting...
17:06:12.969 - TearDown completed
17:06:12.969 - SetUp starting...
17:06:12.969 - SetUp completed
17:06:12.969 - Test4_Duplicates starting...
17:06:12.969 - Test4_Duplicates completed
17:06:12.969 - TearDown starting...
17:06:12.969 - TearDown completed
17:06:12.969 - SetUp starting...
17:06:12.969 - SetUp completed
17:06:12.969 - Test5_Clear starting...
17:06:12.969 - Test5_Clear completed
17:06:12.969 - TearDown starting...
17:06:12.969 - TearDown completed
17:06:12.969 - SetUp starting...
17:06:12.969 - SetUp completed
17:06:12.969 - Test6_StudentBasic starting...
17:06:12.969 - Test6_StudentBasic completed
17:06:12.969 - TearDown starting...
17:06:12.969 - TearDown completed
17:06:12.969 - SetUp starting...
17:06:12.969 - SetUp completed
17:06:12.969 - Test7_LargeDataSet starting...
17:06:12.969 - Added batch 1/100 (100 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:12.978 - Added batch 2/100 (200 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:12.994 - Added batch 3/100 (300 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.009 - Added batch 4/100 (400 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.025 - Added batch 5/100 (500 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.040 - Added batch 6/100 (600 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.055 - Added batch 7/100 (700 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.071 - Added batch 8/100 (800 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.087 - Added batch 9/100 (900 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.103 - Added batch 10/100 (1000 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.118 - Added batch 11/100 (1100 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.134 - Added batch 12/100 (1200 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.150 - Added batch 13/100 (1300 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.165 - Added batch 14/100 (1400 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.180 - Added batch 15/100 (1500 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.196 - Added batch 16/100 (1600 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.212 - Added batch 17/100 (1700 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.227 - Added batch 18/100 (1800 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.242 - Added batch 19/100 (1900 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.257 - Added batch 20/100 (2000 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.272 - Added batch 21/100 (2100 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.288 - Added batch 22/100 (2200 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.303 - Added batch 23/100 (2300 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.319 - Added batch 24/100 (2400 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.334 - Added batch 25/100 (2500 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.349 - Added batch 26/100 (2600 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.365 - Added batch 27/100 (2700 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.380 - Added batch 28/100 (2800 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.395 - Added batch 29/100 (2900 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.411 - Added batch 30/100 (3000 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.427 - Added batch 31/100 (3100 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.442 - Added batch 32/100 (3200 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.457 - Added batch 33/100 (3300 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.473 - Added batch 34/100 (3400 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.487 - Added batch 35/100 (3500 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.503 - Added batch 36/100 (3600 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.519 - Added batch 37/100 (3700 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.534 - Added batch 38/100 (3800 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.550 - Added batch 39/100 (3900 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.565 - Added batch 40/100 (4000 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.581 - Added batch 41/100 (4100 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.597 - Added batch 42/100 (4200 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.612 - Added batch 43/100 (4300 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.627 - Added batch 44/100 (4400 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.642 - Added batch 45/100 (4500 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.658 - Added batch 46/100 (4600 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.674 - Added batch 47/100 (4700 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.690 - Added batch 48/100 (4800 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.705 - Added batch 49/100 (4900 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.720 - Added batch 50/100 (5000 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.735 - Added batch 51/100 (5100 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.751 - Added batch 52/100 (5200 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.766 - Added batch 53/100 (5300 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.781 - Added batch 54/100 (5400 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.796 - Added batch 55/100 (5500 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.811 - Added batch 56/100 (5600 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.826 - Added batch 57/100 (5700 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.842 - Added batch 58/100 (5800 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.857 - Added batch 59/100 (5900 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.873 - Added batch 60/100 (6000 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.889 - Added batch 61/100 (6100 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.905 - Added batch 62/100 (6200 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.921 - Added batch 63/100 (6300 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.936 - Added batch 64/100 (6400 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.951 - Added batch 65/100 (6500 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.967 - Added batch 66/100 (6600 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.982 - Added batch 67/100 (6700 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:13.998 - Added batch 68/100 (6800 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.014 - Added batch 69/100 (6900 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.030 - Added batch 70/100 (7000 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.046 - Added batch 71/100 (7100 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.061 - Added batch 72/100 (7200 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.077 - Added batch 73/100 (7300 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.093 - Added batch 74/100 (7400 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.109 - Added batch 75/100 (7500 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.124 - Added batch 76/100 (7600 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.139 - Added batch 77/100 (7700 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.155 - Added batch 78/100 (7800 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.171 - Added batch 79/100 (7900 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.186 - Added batch 80/100 (8000 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.202 - Added batch 81/100 (8100 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.218 - Added batch 82/100 (8200 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.233 - Added batch 83/100 (8300 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.249 - Added batch 84/100 (8400 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.264 - Added batch 85/100 (8500 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.280 - Added batch 86/100 (8600 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.295 - Added batch 87/100 (8700 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.311 - Added batch 88/100 (8800 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.327 - Added batch 89/100 (8900 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.343 - Added batch 90/100 (9000 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.359 - Added batch 91/100 (9100 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.374 - Added batch 92/100 (9200 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.390 - Added batch 93/100 (9300 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.406 - Added batch 94/100 (9400 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.422 - Added batch 95/100 (9500 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.437 - Added batch 96/100 (9600 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.452 - Added batch 97/100 (9700 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.467 - Added batch 98/100 (9800 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.482 - Added batch 99/100 (9900 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.498 - Added batch 100/100 (10000 items total) - Batch time: 0 ms, Total time: 0 ms
17:06:14.514 - Total time for adding 10000 items: 1547 ms
17:06:14.514 - Actual processing time (excluding logging): 0 ms
17:06:14.514 - Verification of 10000 items took: 0 ms
17:06:14.515 - TearDown starting...
17:06:14.515 - TearDown completed
17:06:14.516 - SetUp starting...
17:06:14.516 - SetUp completed
17:06:14.516 - Test8_ConcurrentAccess starting...
17:06:14.516 - Created thread 0
17:06:14.516 - Created thread 1
17:06:14.516 - Created thread 2
17:06:14.516 - Created thread 3
17:06:14.623 - Started thread 0 at 110 ms
17:06:14.623 - Started thread 1 at 110 ms
17:06:14.624 - Started thread 2 at 110 ms
17:06:14.624 - Started thread 3 at 110 ms
17:06:14.628 - Thread 0 completed at 110 ms
17:06:14.632 - Thread 1 completed at 110 ms
17:06:14.633 - Thread 2 completed at 110 ms
17:06:14.633 - Thread 3 completed at 110 ms
17:06:14.633 - Concurrent test total time: 110 ms
17:06:14.633 - Final item count: 9773
17:06:14.633 - TearDown starting...
17:06:14.634 - TearDown completed
17:06:14.634 - SetUp starting...
17:06:14.634 - SetUp completed
17:06:14.634 - Test9_StressTest starting...
17:06:14.638 - Completed 10000/100000 iterations (10%) - Time: 0 ms
17:06:14.658 - Completed 20000/100000 iterations (20%) - Time: 16 ms
17:06:14.674 - Completed 30000/100000 iterations (30%) - Time: 31 ms
17:06:14.689 - Completed 40000/100000 iterations (40%) - Time: 47 ms
17:06:14.707 - Completed 50000/100000 iterations (50%) - Time: 63 ms
17:06:14.721 - Completed 60000/100000 iterations (60%) - Time: 78 ms
17:06:14.737 - Completed 70000/100000 iterations (70%) - Time: 94 ms
17:06:14.753 - Completed 80000/100000 iterations (80%) - Time: 110 ms
17:06:14.769 - Completed 90000/100000 iterations (90%) - Time: 125 ms
17:06:14.785 - Completed 100000/100000 iterations (100%) - Time: 141 ms
17:06:14.795 - Stress test completed in 156 ms
17:06:14.795 - Final counts - Int: 25000, Str: 25000, Bool: 0, Student: 25000
17:06:14.795 - TearDown starting...
17:06:14.805 - TearDown completed
17:06:14.805 - SetUp starting...
17:06:14.805 - SetUp completed
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
Total test time: 1016 ms
=== Hash Collision Test Complete ===
17:06:15.829 - TearDown starting...
17:06:15.831 - TearDown completed
17:06:15.831 - SetUp starting...
17:06:15.831 - SetUp completed
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
Thread 57128 starting, range 25000 to 49999
Thread 51808 starting, range 75000 to 99999
Thread 46120 starting, range 50000 to 74999
Thread 17820 starting, range 0 to 24999
Thread 57128 processed 1000/25000 items, successfully added: 1000
Thread 46120 processed 1000/25000 items, successfully added: 1000
Thread 51808 processed 1000/25000 items, successfully added: 1000
Thread 57128 processed 2000/25000 items, successfully added: 2000
Thread 57128 processed 3000/25000 items, successfully added: 3000
Thread 51808 processed 2000/25000 items, successfully added: 2000
Thread 17820 processed 1000/25000 items, successfully added: 1000
Thread 46120 processed 2000/25000 items, successfully added: 2000
Thread 17820 processed 2000/25000 items, successfully added: 2000
Thread 57128 processed 4000/25000 items, successfully added: 4000
Thread 51808 processed 3000/25000 items, successfully added: 3000
Thread 46120 processed 3000/25000 items, successfully added: 3000
Thread 51808 processed 4000/25000 items, successfully added: 4000
Thread 46120 processed 4000/25000 items, successfully added: 4000
Thread 57128 processed 5000/25000 items, successfully added: 5000
Thread 46120 processed 5000/25000 items, successfully added: 5000
Thread 17820 processed 3000/25000 items, successfully added: 3000
Thread 46120 processed 6000/25000 items, successfully added: 6000
Thread 51808 processed 5000/25000 items, successfully added: 5000
Thread 57128 processed 6000/25000 items, successfully added: 6000
Thread 57128 processed 7000/25000 items, successfully added: 7000
Thread 17820 processed 4000/25000 items, successfully added: 4000
Thread 51808 processed 6000/25000 items, successfully added: 6000
Thread 46120 processed 7000/25000 items, successfully added: 7000
Thread 57128 processed 8000/25000 items, successfully added: 8000
Thread 17820 processed 5000/25000 items, successfully added: 5000
Thread 51808 processed 7000/25000 items, successfully added: 7000
Thread 57128 processed 9000/25000 items, successfully added: 9000
Thread 17820 processed 6000/25000 items, successfully added: 6000
Thread 57128 processed 10000/25000 items, successfully added: 10000
Thread 46120 processed 8000/25000 items, successfully added: 8000
Thread 51808 processed 8000/25000 items, successfully added: 8000
Thread 17820 processed 7000/25000 items, successfully added: 7000
Thread 51808 processed 9000/25000 items, successfully added: 9000
Thread 46120 processed 9000/25000 items, successfully added: 9000
Thread 57128 processed 11000/25000 items, successfully added: 11000
Thread 46120 processed 10000/25000 items, successfully added: 10000
Thread 17820 processed 8000/25000 items, successfully added: 8000
Thread 51808 processed 10000/25000 items, successfully added: 10000
Thread 46120 processed 11000/25000 items, successfully added: 11000
Thread 57128 processed 12000/25000 items, successfully added: 12000
Thread 17820 processed 9000/25000 items, successfully added: 9000
Thread 51808 processed 11000/25000 items, successfully added: 11000
Thread 57128 processed 13000/25000 items, successfully added: 13000
Thread 46120 processed 12000/25000 items, successfully added: 12000
Thread 51808 processed 12000/25000 items, successfully added: 12000
Thread 57128 processed 14000/25000 items, successfully added: 14000
Thread 17820 processed 10000/25000 items, successfully added: 10000
Thread 57128 processed 15000/25000 items, successfully added: 15000
Thread 51808 processed 13000/25000 items, successfully added: 13000
Thread 46120 processed 13000/25000 items, successfully added: 13000
Thread 17820 processed 11000/25000 items, successfully added: 11000
Thread 51808 processed 14000/25000 items, successfully added: 14000
Thread 57128 processed 16000/25000 items, successfully added: 16000
Thread 46120 processed 14000/25000 items, successfully added: 14000
Thread 51808 processed 15000/25000 items, successfully added: 15000
Thread 57128 processed 17000/25000 items, successfully added: 17000
Thread 17820 processed 12000/25000 items, successfully added: 12000
Thread 57128 processed 18000/25000 items, successfully added: 18000
Thread 51808 processed 16000/25000 items, successfully added: 16000
Thread 57128 processed 19000/25000 items, successfully added: 19000
Thread 46120 processed 15000/25000 items, successfully added: 15000
Thread 51808 processed 17000/25000 items, successfully added: 17000
Thread 17820 processed 13000/25000 items, successfully added: 13000
Thread 46120 processed 16000/25000 items, successfully added: 16000
Thread 57128 processed 20000/25000 items, successfully added: 20000
Thread 17820 processed 14000/25000 items, successfully added: 14000
Thread 57128 processed 21000/25000 items, successfully added: 21000
Thread 46120 processed 17000/25000 items, successfully added: 17000
Thread 57128 processed 22000/25000 items, successfully added: 22000
Thread 51808 processed 18000/25000 items, successfully added: 18000
Thread 17820 processed 15000/25000 items, successfully added: 15000
Thread 17820 processed 16000/25000 items, successfully added: 16000
Thread 46120 processed 18000/25000 items, successfully added: 18000
Thread 57128 processed 23000/25000 items, successfully added: 23000
Thread 51808 processed 19000/25000 items, successfully added: 19000
Thread 51808 processed 20000/25000 items, successfully added: 20000
Thread 17820 processed 17000/25000 items, successfully added: 17000
Thread 51808 processed 21000/25000 items, successfully added: 21000
Thread 57128 processed 24000/25000 items, successfully added: 24000
Thread 51808 processed 22000/25000 items, successfully added: 22000
Thread 46120 processed 19000/25000 items, successfully added: 19000
Thread 46120 processed 20000/25000 items, successfully added: 20000
Thread 17820 processed 18000/25000 items, successfully added: 18000
Thread 57128 processed 25000/25000 items, successfully added: 25000
Thread 57128 completed. Added 25000/25000 items successfully
Thread 51808 processed 23000/25000 items, successfully added: 23000
Thread 46120 processed 21000/25000 items, successfully added: 21000
Thread 17820 processed 19000/25000 items, successfully added: 19000
Thread 46120 processed 22000/25000 items, successfully added: 22000
Thread 51808 processed 24000/25000 items, successfully added: 24000
Thread 17820 processed 20000/25000 items, successfully added: 20000
Thread 46120 processed 23000/25000 items, successfully added: 23000
Thread 46120 processed 24000/25000 items, successfully added: 24000
Thread 17820 processed 21000/25000 items, successfully added: 21000
Thread 51808 processed 25000/25000 items, successfully added: 25000
Thread 51808 completed. Added 25000/25000 items successfully
Thread 46120 processed 25000/25000 items, successfully added: 25000
Thread 46120 completed. Added 25000/25000 items successfully
Thread 17820 processed 22000/25000 items, successfully added: 22000
Thread 17820 processed 23000/25000 items, successfully added: 23000
Thread 17820 processed 24000/25000 items, successfully added: 24000
Thread 17820 processed 25000/25000 items, successfully added: 25000
Thread 17820 completed. Added 25000/25000 items successfully
Parallel insertion took: 36469 ms
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
Total test time: 73187 ms
=== Aggressive Collision Test Complete ===
17:07:29.016 - TearDown starting...
17:07:29.023 - TearDown completed
17:07:29.023 - SetUp starting...
17:07:29.023 - SetUp completed
17:07:29.024 - Test12_Iterator starting...
17:07:29.024 - Test12_Iterator completed
17:07:29.024 - TearDown starting...
17:07:29.024 - TearDown completed
17:07:29.024 - SetUp starting...
17:07:29.024 - SetUp completed
17:07:29.025 - Test13_IteratorStress starting...
17:07:29.025 - Iteration 1 found 0 items after 0 ms
17:07:29.025 - Iteration 2 found 0 items after 0 ms
17:07:29.025 - Iteration 3 found 0 items after 0 ms
17:07:29.025 - Iteration 4 found 0 items after 0 ms
17:07:29.026 - Iteration 5 found 0 items after 0 ms
17:07:29.026 - Iteration 6 found 0 items after 0 ms
17:07:29.026 - Iteration 7 found 0 items after 0 ms
17:07:29.030 - Iteration 8 found 0 items after 0 ms
17:07:29.030 - Iteration 9 found 0 items after 0 ms
17:07:29.030 - Iteration 10 found 0 items after 0 ms
17:07:36.843 - Final iteration found 2000 items
17:07:36.844 - Test13_IteratorStress completed after 7812 ms
17:07:36.844 - TearDown starting...
17:07:36.846 - TearDown completed
17:07:36.846 - SetUp starting...
17:07:36.846 - SetUp completed
17:07:36.846 - Test14_LockingMechanism starting...
17:08:08.619 - Lock test took 31772 ms, 4000 successful locks across 4 threads
17:08:08.620 - TearDown starting...
17:08:08.620 - TearDown completed
17:08:08.620 - SetUp starting...
17:08:08.621 - SetUp completed
17:08:08.621 - Test15_AddRange_Array starting...
17:08:08.649 - Test15_AddRange_Array completed
17:08:08.650 - TearDown starting...
17:08:08.658 - TearDown completed
17:08:08.658 - SetUp starting...
17:08:08.658 - SetUp completed
17:08:08.658 - Test16_AddRange_Collection starting...
17:08:08.710 - Test16_AddRange_Collection completed
17:08:08.711 - TearDown starting...
17:08:08.713 - TearDown completed
17:08:08.714 - SetUp starting...
17:08:08.714 - SetUp completed
17:08:08.714 - Test17_RemoveRange starting...
17:08:08.848 - Test17_RemoveRange completed
17:08:08.848 - TearDown starting...
17:08:08.852 - TearDown completed
17:08:08.852 - SetUp starting...
17:08:08.853 - SetUp completed
17:08:08.853 - Test18_SetOperations starting...
17:08:08.853 - Test18_SetOperations completed
17:08:08.854 - TearDown starting...
17:08:08.854 - TearDown completed
17:08:08.854 - SetUp starting...
17:08:08.854 - SetUp completed
17:08:08.854 - Test19_TryGetValue starting...
17:08:08.854 - Test19_TryGetValue completed
17:08:08.854 - TearDown starting...
17:08:08.854 - TearDown completed
Test took 30868 ms with 4 threads doing 10 iterations each
Lock test took 30909 ms, 4000 successful locks across 4 threads
 Time:12:00.991 N:116 E:0 F:0 I:0
  TThreadSafeListTest Time:31.626 N:46 E:0 F:0 I:0
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
    00.003  Test18_MultiThreadAccess
    00.018  Test19_LargeDataSet
    00.001  Test20_RandomOperations
    00.099  Test21_LargeDataSetSortingPerformance
    00.000  Test22_Iterator
    00.003  Test23_IteratorThreadSafety
    00.000  Test24_IteratorExceptionSafety
    31.501  Test25_LockingMechanism
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
    00.001  Test37_IndexOfItemWithStartAndCount
    00.000  Test38_LastIndexOf
    00.000  Test39_LastIndexOfWithStart
    00.000  Test40_LastIndexOfWithStartAndCount
    00.000  Test41_Insert
    00.000  Test42_Exchange
    00.000  Test43_Move
    00.000  Test44_Reverse
    00.000  Test45_Extract
    00.000  Test46_ExtractAt
  TThreadSafeListStudentTest Time:00.236 N:2 E:0 F:0 I:0
    00.139  TestLargeStudentSortByName
    00.097  TestLargeStudentSortById
  TThreadSafeDictionaryTest Time:31.450 N:38 E:0 F:0 I:0
    00.001  Test1_Creation
    00.001  Test2_Add
    00.000  Test3_AddDuplicate
    00.000  Test4_GetItem
    00.000  Test5_Remove
    00.001  Test6_AddOrSetValue
    00.000  Test7_Clear
    00.000  Test8_Count
    00.000  Test9_FirstLast
    00.000  Test10_EmptyDictionary
    00.001  Test11_LargeDataSet
    00.000  Test12_NilValues
    00.000  Test13_Boundaries
    00.014  Test14_MultiThreadAccess
    00.022  Test15_ConcurrentOperations
    00.000  Test16_HashCollisions
    00.133  Test17_LargeDataSetPerformance
    00.059  Test18_HashingPerformance
    00.002  Test19_InitialCapacity
    00.001  Test20_ManualResize
    00.000  Test21_ResizeWithData
    00.000  Test22_ResizeUnderflow
    00.000  Test23_BucketCount
    00.000  Test24_IteratorBasic
    00.000  Test25_IteratorEmpty
    00.001  Test26_IteratorModification
    00.000  Test27_MultipleIterators
    00.000  Test28_IteratorReset
    31.210  Test29_LockingMechanism
    00.002  Test30_CompoundKeyBasic
    00.000  Test31_CompoundKeyIteration
    00.001  Test32_CustomConstructors
    00.000  Test33_GetKeysAndValues
    00.000  Test34_TrimExcess
    00.001  Test35_TryAdd
    00.000  Test36_AddRange
    00.000  Test37_ToArray
    00.000  Test38_ContainsValue
  TThreadSafeHashSetTest Time:55.902 N:19 E:0 F:0 I:0
    00.006  Test1_BasicOperations
    00.010  Test2_SimpleAdd
    00.001  Test3_SimpleRemove
    00.000  Test4_Duplicates
    00.000  Test5_Clear
    00.000  Test6_StudentBasic
    01.547  Test7_LargeDataSet
    00.118  Test8_ConcurrentAccess
    00.171  Test9_StressTest
    01.026  Test10_HashCollisions
    13.192  Test11_AggressiveCollisions
    00.001  Test12_Iterator
    07.822  Test13_IteratorStress
    31.774  Test14_LockingMechanism
    00.038  Test15_AddRange_Array
    00.056  Test16_AddRange_Collection
    00.138  Test17_RemoveRange
    00.002  Test18_SetOperations
    00.000  Test19_TryGetValue
  TThreadSafeDequeTests Time:01.777 N:11 E:0 F:0 I:0
    00.000  TestPushFrontAndPopFront
    00.000  TestPushBackAndPopBack
    00.000  TestMixedOperations
    00.000  TestClear
    00.000  TestPeekOperations
    00.000  TestTryOperations
    00.000  TestIsEmpty
    00.000  TestToArray
    00.000  TestPushRange
    30.868  TestMultiThreadPushPop
    30.909  TestLockingMechanism

Number of run tests: 116
Number of errors:    0
Number of failures:  0



Tests completed.
Time: 17:09:10.634
```

