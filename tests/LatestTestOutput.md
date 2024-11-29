# Latest Test Output

## Output as for 2024-11-30

```bash
$ ./TestRunner.exe -a --format=plain
Starting test runner...
Time: 05:52:01.038
Running tests...
Multi-thread operations took: 15 ms
Large dataset operations took: 16 ms
Integer Test:
Before sort (first 5):
100000 99999 99998 99997 99996
Sorting 100000 integers took: 0 ms
After sort (first 5):
1 2 3 4 5
Integer sort verification: TRUE

String Test:
Before sort (first 3):
7W0ufcyo4f
QHsa5gqYOZ
SvxGjq29kl
Sorting 100000 strings (10 chars each) took: 47 ms
After sort (first 3):
002rf8Esgw
007kGtImCz
00AZZwXMw9
String sort verification: TRUE

Real Test:
Before sort (first 5):
150000.00 149998.50 149997.00 149995.50 149994.00
Sorting 100000 reals took: 16 ms
After sort (first 5):
1.50 3.00 4.50 6.00 7.50
Real sort verification: TRUE

Lock test took 33159 ms, 4000 successful locks across 4 threads
Student Sort By Name Test:
Before sort (first 3):
Name: aGcbsHXVDt, ID: 100000
Name: znLDYDEzyt, ID: 99999
Name: XZpiIxyXNp, ID: 99998

Sorting 100000 students by name took: 63 ms
After sort (first 3):
Name: AABUUIAPud, ID: 14829
Name: AABYMTxLIF, ID: 66396
Name: AADVhSrbrt, ID: 39040

Name sort verification: TRUE

Student Sort By ID Test:
Before sort (first 3):
ID: 9946, Name: Student0
ID: 89534, Name: Student1
ID: 82793, Name: Student2

Sorting 100000 students by ID took: 47 ms
After sort (first 3):
ID: 4, Name: Student49628
ID: 5, Name: Student1559
ID: 11, Name: Student68440

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
Starting TestFind
Running test #4
Completed TestFind
Setting up test...
Setup complete
Starting TestRemove
Running test #5
Completed TestRemove
Setting up test...
Setup complete
Starting TestReplace
Running test #6
Completed TestReplace
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
Starting TestEmptyDictionary
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
Starting TestMultiThreadAccess
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
Starting TestConcurrentOperations
TestConcurrentOperations completed
Setting up test...
Setup complete
Setting up test...
Setup complete
Adding 100000 items took: 32 ms
Finding 100000 items took: 109 ms
Setting up test...
Setup complete
Adding 100000 items took: 15 ms
Finding 100000 items took: 16 ms
Setting up test...
Setup complete
Starting TestInitialCapacity
Running test #12
Completed TestInitialCapacity
Setting up test...
Setup complete
Starting TestManualResize
Running test #13
Completed TestManualResize
Setting up test...
Setup complete
Starting TestResizeWithData
Running test #14
Completed TestResizeWithData
Setting up test...
Setup complete
Starting TestResizeUnderflow
Running test #15
Completed TestResizeUnderflow
Setting up test...
Setup complete
Starting TestBucketCount
Running test #16
Completed TestBucketCount
Setting up test...
Setup complete
Starting TestIteratorBasic
Running test #17
Setting up test...
Setup complete
Starting TestIteratorEmpty
Running test #18
Setting up test...
Setup complete
Starting TestIteratorModification
Running test #19
Setting up test...
Setup complete
Starting TestMultipleIterators
Running test #20
Setting up test...
Setup complete
Starting TestIteratorReset
Running test #21
Setting up test...
Setup complete
Lock test took 32749 ms, 4000 successful locks across 4 threads
05:53:07.627 - SetUp starting...
05:53:07.627 - SetUp completed
05:53:07.627 - Test1_BasicOperations starting...
05:53:07.627 - Test1_BasicOperations completed
05:53:07.627 - TearDown starting...
05:53:07.627 - TearDown completed
05:53:07.627 - SetUp starting...
05:53:07.627 - SetUp completed
05:53:07.627 - Test2_SimpleAdd starting...
05:53:07.627 - Test2_SimpleAdd completed
05:53:07.627 - TearDown starting...
05:53:07.627 - TearDown completed
05:53:07.627 - SetUp starting...
05:53:07.627 - SetUp completed
05:53:07.627 - Test3_SimpleRemove starting...
05:53:07.627 - Test3_SimpleRemove completed
05:53:07.627 - TearDown starting...
05:53:07.627 - TearDown completed
05:53:07.627 - SetUp starting...
05:53:07.627 - SetUp completed
05:53:07.627 - Test4_Duplicates starting...
05:53:07.627 - Test4_Duplicates completed
05:53:07.627 - TearDown starting...
05:53:07.627 - TearDown completed
05:53:07.627 - SetUp starting...
05:53:07.627 - SetUp completed
05:53:07.627 - Test5_Clear starting...
05:53:07.627 - Test5_Clear completed
05:53:07.627 - TearDown starting...
05:53:07.627 - TearDown completed
05:53:07.627 - SetUp starting...
05:53:07.627 - SetUp completed
05:53:07.627 - Test6_StudentBasic starting...
05:53:07.627 - Test6_StudentBasic completed
05:53:07.627 - TearDown starting...
05:53:07.627 - TearDown completed
05:53:07.627 - SetUp starting...
05:53:07.627 - SetUp completed
05:53:07.627 - Test7_LargeDataSet starting...
05:53:07.627 - Added batch 1/100 (100 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.642 - Added batch 2/100 (200 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.658 - Added batch 3/100 (300 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.675 - Added batch 4/100 (400 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.691 - Added batch 5/100 (500 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.706 - Added batch 6/100 (600 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.722 - Added batch 7/100 (700 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.738 - Added batch 8/100 (800 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.754 - Added batch 9/100 (900 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.770 - Added batch 10/100 (1000 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.786 - Added batch 11/100 (1100 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.802 - Added batch 12/100 (1200 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.818 - Added batch 13/100 (1300 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.834 - Added batch 14/100 (1400 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.850 - Added batch 15/100 (1500 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.866 - Added batch 16/100 (1600 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.881 - Added batch 17/100 (1700 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.898 - Added batch 18/100 (1800 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.913 - Added batch 19/100 (1900 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.930 - Added batch 20/100 (2000 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.946 - Added batch 21/100 (2100 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.962 - Added batch 22/100 (2200 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.978 - Added batch 23/100 (2300 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:07.993 - Added batch 24/100 (2400 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.009 - Added batch 25/100 (2500 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.025 - Added batch 26/100 (2600 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.041 - Added batch 27/100 (2700 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.057 - Added batch 28/100 (2800 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.073 - Added batch 29/100 (2900 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.089 - Added batch 30/100 (3000 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.104 - Added batch 31/100 (3100 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.120 - Added batch 32/100 (3200 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.136 - Added batch 33/100 (3300 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.152 - Added batch 34/100 (3400 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.168 - Added batch 35/100 (3500 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.184 - Added batch 36/100 (3600 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.200 - Added batch 37/100 (3700 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.216 - Added batch 38/100 (3800 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.232 - Added batch 39/100 (3900 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.248 - Added batch 40/100 (4000 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.264 - Added batch 41/100 (4100 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.280 - Added batch 42/100 (4200 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.296 - Added batch 43/100 (4300 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.312 - Added batch 44/100 (4400 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.328 - Added batch 45/100 (4500 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.345 - Added batch 46/100 (4600 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.361 - Added batch 47/100 (4700 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.376 - Added batch 48/100 (4800 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.392 - Added batch 49/100 (4900 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.408 - Added batch 50/100 (5000 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.425 - Added batch 51/100 (5100 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.441 - Added batch 52/100 (5200 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.458 - Added batch 53/100 (5300 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.473 - Added batch 54/100 (5400 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.490 - Added batch 55/100 (5500 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.506 - Added batch 56/100 (5600 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.521 - Added batch 57/100 (5700 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.537 - Added batch 58/100 (5800 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.553 - Added batch 59/100 (5900 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.571 - Added batch 60/100 (6000 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.587 - Added batch 61/100 (6100 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.603 - Added batch 62/100 (6200 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.619 - Added batch 63/100 (6300 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.635 - Added batch 64/100 (6400 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.651 - Added batch 65/100 (6500 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.667 - Added batch 66/100 (6600 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.682 - Added batch 67/100 (6700 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.698 - Added batch 68/100 (6800 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.714 - Added batch 69/100 (6900 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.731 - Added batch 70/100 (7000 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.747 - Added batch 71/100 (7100 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.763 - Added batch 72/100 (7200 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.778 - Added batch 73/100 (7300 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.794 - Added batch 74/100 (7400 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.809 - Added batch 75/100 (7500 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.825 - Added batch 76/100 (7600 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.841 - Added batch 77/100 (7700 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.857 - Added batch 78/100 (7800 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.873 - Added batch 79/100 (7900 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.888 - Added batch 80/100 (8000 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.904 - Added batch 81/100 (8100 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.922 - Added batch 82/100 (8200 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.938 - Added batch 83/100 (8300 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.954 - Added batch 84/100 (8400 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.970 - Added batch 85/100 (8500 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:08.986 - Added batch 86/100 (8600 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.002 - Added batch 87/100 (8700 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.018 - Added batch 88/100 (8800 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.034 - Added batch 89/100 (8900 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.050 - Added batch 90/100 (9000 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.066 - Added batch 91/100 (9100 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.082 - Added batch 92/100 (9200 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.097 - Added batch 93/100 (9300 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.113 - Added batch 94/100 (9400 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.132 - Added batch 95/100 (9500 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.149 - Added batch 96/100 (9600 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.164 - Added batch 97/100 (9700 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.180 - Added batch 98/100 (9800 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.196 - Added batch 99/100 (9900 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.212 - Added batch 100/100 (10000 items total) - Batch time: 0 ms, Total time: 0 ms
05:53:09.228 - Total time for adding 10000 items: 1594 ms
05:53:09.228 - Actual processing time (excluding logging): 0 ms
05:53:09.228 - Verification of 10000 items took: 0 ms
05:53:09.228 - TearDown starting...
05:53:09.228 - TearDown completed
05:53:09.228 - SetUp starting...
05:53:09.228 - SetUp completed
05:53:09.228 - Test8_ConcurrentAccess starting...
05:53:09.228 - Created thread 0
05:53:09.228 - Created thread 1
05:53:09.228 - Created thread 2
05:53:09.228 - Created thread 3
05:53:09.340 - Started thread 0 at 110 ms
05:53:09.340 - Started thread 1 at 110 ms
05:53:09.340 - Started thread 2 at 110 ms
05:53:09.340 - Started thread 3 at 110 ms
05:53:09.351 - Thread 0 completed at 125 ms
05:53:09.351 - Thread 1 completed at 125 ms
05:53:09.351 - Thread 2 completed at 125 ms
05:53:09.351 - Thread 3 completed at 125 ms
05:53:09.351 - Concurrent test total time: 125 ms
05:53:09.351 - Final item count: 9869
05:53:09.351 - TearDown starting...
05:53:09.351 - TearDown completed
05:53:09.351 - SetUp starting...
05:53:09.351 - SetUp completed
05:53:09.351 - Test9_StressTest starting...
05:53:09.359 - Completed 10000/100000 iterations (10%) - Time: 16 ms
05:53:09.377 - Completed 20000/100000 iterations (20%) - Time: 31 ms
05:53:09.388 - Completed 30000/100000 iterations (30%) - Time: 31 ms
05:53:09.406 - Completed 40000/100000 iterations (40%) - Time: 63 ms
05:53:09.424 - Completed 50000/100000 iterations (50%) - Time: 78 ms
05:53:09.439 - Completed 60000/100000 iterations (60%) - Time: 94 ms
05:53:09.456 - Completed 70000/100000 iterations (70%) - Time: 110 ms
05:53:09.467 - Completed 80000/100000 iterations (80%) - Time: 110 ms
05:53:09.489 - Completed 90000/100000 iterations (90%) - Time: 141 ms
05:53:09.508 - Completed 100000/100000 iterations (100%) - Time: 156 ms
05:53:09.514 - Stress test completed in 156 ms
05:53:09.514 - Final counts - Int: 25000, Str: 25000, Bool: 0, Student: 25000
05:53:09.514 - TearDown starting...
05:53:09.525 - TearDown completed
05:53:09.525 - SetUp starting...
05:53:09.525 - SetUp completed
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
Total test time: 1391 ms
=== Hash Collision Test Complete ===
05:53:10.919 - TearDown starting...
05:53:10.919 - TearDown completed
05:53:10.919 - SetUp starting...
05:53:10.919 - SetUp completed
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
Thread 37764 starting, range 0 to 24999
Thread 24460 starting, range 75000 to 99999
Thread 26600 starting, range 50000 to 74999
Thread 22216 starting, range 25000 to 49999
Thread 37764 processed 1000/25000 items, successfully added: 1000
Thread 24460 processed 1000/25000 items, successfully added: 1000
Thread 22216 processed 1000/25000 items, successfully added: 1000
Thread 37764 processed 2000/25000 items, successfully added: 2000
Thread 26600 processed 1000/25000 items, successfully added: 1000
Thread 22216 processed 2000/25000 items, successfully added: 2000
Thread 24460 processed 2000/25000 items, successfully added: 2000
Thread 37764 processed 3000/25000 items, successfully added: 3000
Thread 26600 processed 2000/25000 items, successfully added: 2000
Thread 22216 processed 3000/25000 items, successfully added: 3000
Thread 24460 processed 3000/25000 items, successfully added: 3000
Thread 26600 processed 3000/25000 items, successfully added: 3000
Thread 37764 processed 4000/25000 items, successfully added: 4000
Thread 22216 processed 4000/25000 items, successfully added: 4000
Thread 37764 processed 5000/25000 items, successfully added: 5000
Thread 26600 processed 4000/25000 items, successfully added: 4000
Thread 24460 processed 4000/25000 items, successfully added: 4000
Thread 22216 processed 5000/25000 items, successfully added: 5000
Thread 37764 processed 6000/25000 items, successfully added: 6000
Thread 26600 processed 5000/25000 items, successfully added: 5000
Thread 22216 processed 6000/25000 items, successfully added: 6000
Thread 24460 processed 5000/25000 items, successfully added: 5000
Thread 26600 processed 6000/25000 items, successfully added: 6000
Thread 37764 processed 7000/25000 items, successfully added: 7000
Thread 22216 processed 7000/25000 items, successfully added: 7000
Thread 26600 processed 7000/25000 items, successfully added: 7000
Thread 37764 processed 8000/25000 items, successfully added: 8000
Thread 24460 processed 6000/25000 items, successfully added: 6000
Thread 37764 processed 9000/25000 items, successfully added: 9000
Thread 22216 processed 8000/25000 items, successfully added: 8000
Thread 26600 processed 8000/25000 items, successfully added: 8000
Thread 22216 processed 9000/25000 items, successfully added: 9000
Thread 26600 processed 9000/25000 items, successfully added: 9000
Thread 24460 processed 7000/25000 items, successfully added: 7000
Thread 22216 processed 10000/25000 items, successfully added: 10000
Thread 37764 processed 10000/25000 items, successfully added: 10000
Thread 26600 processed 10000/25000 items, successfully added: 10000
Thread 24460 processed 8000/25000 items, successfully added: 8000
Thread 37764 processed 11000/25000 items, successfully added: 11000
Thread 26600 processed 11000/25000 items, successfully added: 11000
Thread 22216 processed 11000/25000 items, successfully added: 11000
Thread 22216 processed 12000/25000 items, successfully added: 12000
Thread 26600 processed 12000/25000 items, successfully added: 12000
Thread 37764 processed 12000/25000 items, successfully added: 12000
Thread 24460 processed 9000/25000 items, successfully added: 9000
Thread 22216 processed 13000/25000 items, successfully added: 13000
Thread 24460 processed 10000/25000 items, successfully added: 10000
Thread 26600 processed 13000/25000 items, successfully added: 13000
Thread 37764 processed 13000/25000 items, successfully added: 13000
Thread 22216 processed 14000/25000 items, successfully added: 14000
Thread 24460 processed 11000/25000 items, successfully added: 11000
Thread 26600 processed 14000/25000 items, successfully added: 14000
Thread 24460 processed 12000/25000 items, successfully added: 12000
Thread 37764 processed 14000/25000 items, successfully added: 14000
Thread 22216 processed 15000/25000 items, successfully added: 15000
Thread 26600 processed 15000/25000 items, successfully added: 15000
Thread 37764 processed 15000/25000 items, successfully added: 15000
Thread 24460 processed 13000/25000 items, successfully added: 13000
Thread 22216 processed 16000/25000 items, successfully added: 16000
Thread 26600 processed 16000/25000 items, successfully added: 16000
Thread 24460 processed 14000/25000 items, successfully added: 14000
Thread 37764 processed 16000/25000 items, successfully added: 16000
Thread 26600 processed 17000/25000 items, successfully added: 17000
Thread 24460 processed 15000/25000 items, successfully added: 15000
Thread 22216 processed 17000/25000 items, successfully added: 17000
Thread 37764 processed 17000/25000 items, successfully added: 17000
Thread 26600 processed 18000/25000 items, successfully added: 18000
Thread 24460 processed 16000/25000 items, successfully added: 16000
Thread 22216 processed 18000/25000 items, successfully added: 18000
Thread 37764 processed 18000/25000 items, successfully added: 18000
Thread 24460 processed 17000/25000 items, successfully added: 17000
Thread 26600 processed 19000/25000 items, successfully added: 19000
Thread 22216 processed 19000/25000 items, successfully added: 19000
Thread 24460 processed 18000/25000 items, successfully added: 18000
Thread 22216 processed 20000/25000 items, successfully added: 20000
Thread 26600 processed 20000/25000 items, successfully added: 20000
Thread 37764 processed 19000/25000 items, successfully added: 19000
Thread 24460 processed 19000/25000 items, successfully added: 19000
Thread 22216 processed 21000/25000 items, successfully added: 21000
Thread 24460 processed 20000/25000 items, successfully added: 20000
Thread 26600 processed 21000/25000 items, successfully added: 21000
Thread 22216 processed 22000/25000 items, successfully added: 22000
Thread 26600 processed 22000/25000 items, successfully added: 22000
Thread 22216 processed 23000/25000 items, successfully added: 23000
Thread 37764 processed 20000/25000 items, successfully added: 20000
Thread 24460 processed 21000/25000 items, successfully added: 21000
Thread 26600 processed 23000/25000 items, successfully added: 23000
Thread 22216 processed 24000/25000 items, successfully added: 24000
Thread 24460 processed 22000/25000 items, successfully added: 22000
Thread 37764 processed 21000/25000 items, successfully added: 21000
Thread 26600 processed 24000/25000 items, successfully added: 24000
Thread 22216 processed 25000/25000 items, successfully added: 25000
Thread 22216 completed. Added 25000/25000 items successfully
Thread 24460 processed 23000/25000 items, successfully added: 23000
Thread 26600 processed 25000/25000 items, successfully added: 25000
Thread 26600 completed. Added 25000/25000 items successfully
Thread 37764 processed 22000/25000 items, successfully added: 22000
Thread 24460 processed 24000/25000 items, successfully added: 24000
Thread 37764 processed 23000/25000 items, successfully added: 23000
Thread 24460 processed 25000/25000 items, successfully added: 25000
Thread 24460 completed. Added 25000/25000 items successfully
Thread 37764 processed 24000/25000 items, successfully added: 24000
Thread 37764 processed 25000/25000 items, successfully added: 25000
Thread 37764 completed. Added 25000/25000 items successfully
Parallel insertion took: 44125 ms
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
Total test time: 86172 ms
=== Aggressive Collision Test Complete ===
05:54:37.090 - TearDown starting...
05:54:37.106 - TearDown completed
05:54:37.106 - SetUp starting...
05:54:37.106 - SetUp completed
05:54:37.106 - Test12_Iterator starting...
05:54:37.106 - Test12_Iterator completed
05:54:37.106 - TearDown starting...
05:54:37.106 - TearDown completed
05:54:37.106 - SetUp starting...
05:54:37.106 - SetUp completed
05:54:37.106 - Test13_IteratorStress starting...
05:54:37.106 - Iteration 1 found 0 items after 0 ms
05:54:37.106 - Iteration 2 found 0 items after 0 ms
05:54:37.106 - Iteration 3 found 0 items after 0 ms
05:54:37.106 - Iteration 4 found 0 items after 0 ms
05:54:37.106 - Iteration 5 found 0 items after 0 ms
05:54:37.106 - Iteration 6 found 0 items after 0 ms
05:54:37.106 - Iteration 7 found 0 items after 0 ms
05:54:37.106 - Iteration 8 found 0 items after 0 ms
05:54:37.106 - Iteration 9 found 0 items after 0 ms
05:54:37.106 - Iteration 10 found 0 items after 0 ms
05:54:45.375 - Final iteration found 2000 items
05:54:45.375 - Test13_IteratorStress completed after 8266 ms
05:54:45.375 - TearDown starting...
05:54:45.376 - TearDown completed
05:54:45.376 - SetUp starting...
05:54:45.376 - SetUp completed
05:54:45.376 - Test14_LockingMechanism starting...
05:55:17.873 - Lock test took 32497 ms, 4000 successful locks across 4 threads
05:55:17.873 - TearDown starting...
05:55:17.873 - TearDown completed
Test took 31459 ms with 4 threads doing 10 iterations each
Lock test took 31239 ms, 4000 successful locks across 4 threads
 Time:12:19.533 N:82 E:0 F:0 I:0
  TThreadSafeListTest Time:33.320 N:25 E:0 F:0 I:0
    00.000  TestCreation
    00.000  TestCreationWithNilComparer
    00.000  TestAddInteger
    00.000  TestAddString
    00.000  TestDelete
    00.000  TestFind
    00.000  TestFirstLast
    00.000  TestReplace
    00.000  TestSort
    00.000  TestSortDescending
    00.000  TestIsSorted
    00.000  TestEmptyList
    00.000  TestSingleElement
    00.000  TestDuplicateElements
    00.000  TestBoundaries
    00.000  TestBooleanList
    00.000  TestRealList
    00.009  TestMultiThreadAccess
    00.018  TestLargeDataSet
    00.000  TestRandomOperations
    00.130  TestLargeDataSetSortingPerformance
    00.000  TestIterator
    00.004  TestIteratorThreadSafety
    00.000  TestIteratorExceptionSafety
    33.159  TestLockingMechanism
  TThreadSafeListStudentTest Time:00.268 N:2 E:0 F:0 I:0
    00.156  TestLargeStudentSortByName
    00.112  TestLargeStudentSortById
  TThreadSafeDictionaryTest Time:33.001 N:29 E:0 F:0 I:0
    00.000  TestCreation
    00.000  TestAdd
    00.000  TestAddDuplicate
    00.000  TestFind
    00.000  TestRemove
    00.000  TestReplace
    00.000  TestClear
    00.000  TestCount
    00.000  TestFirstLast
    00.000  TestEmptyDictionary
    00.000  TestLargeDataSet
    00.000  TestNilValues
    00.000  TestBoundaries
    00.029  TestMultiThreadAccess
    00.021  TestConcurrentOperations
    00.000  TestHashCollisions
    00.153  TestLargeDataSetPerformance
    00.049  TestHashingPerformance
    00.000  TestInitialCapacity
    00.000  TestManualResize
    00.000  TestResizeWithData
    00.000  TestResizeUnderflow
    00.000  TestBucketCount
    00.000  TestIteratorBasic
    00.000  TestIteratorEmpty
    00.000  TestIteratorModification
    00.000  TestMultipleIterators
    00.000  TestIteratorReset
    32.749  TestLockingMechanism
  TThreadSafeHashSetTest Time:12:10.246 N:14 E:0 F:0 I:0
    00.000  Test1_BasicOperations
    00.000  Test2_SimpleAdd
    00.000  Test3_SimpleRemove
    00.000  Test4_Duplicates
    00.000  Test5_Clear
    00.000  Test6_StudentBasic
    01.601  Test7_LargeDataSet
    00.123  Test8_ConcurrentAccess
    00.174  Test9_StressTest
    01.394  Test10_HashCollisions
    26.187  Test11_AggressiveCollisions
    00.000  Test12_Iterator
    08.270  Test13_IteratorStress
    32.497  Test14_LockingMechanism
  TThreadSafeDequeTests Time:02.698 N:12 E:0 F:0 I:0
    00.000  TestPushFrontAndPopFront
    00.000  TestPushBackAndPopBack
    00.000  TestMixedOperations
    00.000  TestClear
    00.000  TestPeekOperations
    00.000  TestTryOperations
    00.000  TestIsEmpty
    00.000  TestToArray
    00.000  TestContains
    00.000  TestPushRange
    31.459  TestMultiThreadPushPop
    31.239  TestLockingMechanism

Number of run tests: 82
Number of errors:    0
Number of failures:  0



Tests completed.
Time: 05:56:20.572

```

## Observations on RAII style locking through interface counting

Test results across all collections:

- List: TestLockingMechanism - 33.159 ms
- Dictionary: TestLockingMechanism - 32.749 ms
- HashSet: Test14_LockingMechanism - 32.497 ms
- Deque: TestLockingMechanism - 31.239 ms

Key observations:

- All collections show similar performance in lock acquisition/release (~32-33ms)
- The RAII pattern through ILockToken works consistently across all collections
- No lock failures or deadlocks occurred
- All collections successfully handled 4000 lock operations (4 threads × 1000 iterations)

The consistency in timing suggests that:

- The locking mechanism overhead is similar across collections
- The `TLockToken` implementation is efficient
- The interface-based RAII pattern works reliably






