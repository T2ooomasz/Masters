using System;
using System.Threading;
using System.Collections.Generic;
using CustomLocks; // Added to use your lock implementations
using System.Diagnostics; // Added for Stopwatch

class Program
{
    static int value = 0;
    static IReadWriteLock? currentLock; // Changed to use the interface and made nullable


    const int ReaderIterations = 10; // Increased iterations
    const int WriterIterations = 5;  // Increased iterations

    static void Reader(object? id) // Parameter made nullable
    {
        for (int i = 1; i <= ReaderIterations; i++) // Loop for specified iterations
        {
            currentLock?.ReadLock();
            // Use Stopwatch to get more precise timing relative to simulation start
            Console.WriteLine($"{Stopwatch.GetTimestamp():D10} Reader {id} reads value: {value}");
            Thread.Sleep(50); // Reduced sleep time inside lock
            currentLock?.ReadUnlock();
            Thread.Sleep(100); // Reduced sleep time between reads
        }
        Console.WriteLine($"{Stopwatch.GetTimestamp():D10} Reader {id} finished.");
    }

    static void Writer(object? id) // Parameter made nullable
    {
        // Each writer thread gets its own Random instance, seeded uniquely.
        // Using thread ID or Guid hash for seed.
        Random rand = new Random(Thread.CurrentThread.ManagedThreadId + (id?.GetHashCode() ?? 0) + Guid.NewGuid().GetHashCode());
        for (int i = 1; i <= WriterIterations; i++) // Loop for specified iterations
        {
            currentLock?.WriteLock();
            value = rand.Next(1000);
            Console.WriteLine($"{Stopwatch.GetTimestamp():D10} [Writer {id}] wrote value: {value}");
            Thread.Sleep(75); // Reduced sleep time inside lock
            currentLock?.WriteUnlock();
            Thread.Sleep(250); // Reduced sleep time between writes
        }
        Console.WriteLine($"{Stopwatch.GetTimestamp():D10} [Writer {id}] finished.");
    }

    static void RunSimulation(IReadWriteLock lockToTest, string lockDescription, int numReaders, int numWriters)
    {
        Console.WriteLine($"--- Testing: {lockDescription} ---");
        Stopwatch.StartNew(); // Start stopwatch for relative timing
        currentLock = lockToTest;
        value = 0; // Reset shared value for each simulation

        List<Thread> threads = new List<Thread>();

        for (int i = 1; i <= numReaders; i++)
        {
            Thread readerThread = new Thread(Reader);
            threads.Add(readerThread);
            readerThread.Start($"R{i}");
        }

        for (int i = 1; i <= numWriters; i++)
        {
            Thread writerThread = new Thread(Writer);
            threads.Add(writerThread);
            writerThread.Start($"W{i}");
        }

        foreach (Thread t in threads)
        {
            t.Join(); // Wait for all threads to complete
        }
        Console.WriteLine($"--- Finished testing: {lockDescription} ---");
    }

    static void Main(string[] args)
    {
        Console.WriteLine("Starting simulation with WriterPreferenceReadWriteLock...");
        RunSimulation(new WriterPreferenceReadWriteLock(), "Writer Preference Lock", numReaders: 5, numWriters: 3); // Increased thread count

        Console.WriteLine("\n---------------------------------------------------\n");
        Console.WriteLine("Starting simulation with FairReadWriteLock...");
        RunSimulation(new FairReadWriteLock(), "Fair Lock", numReaders: 5, numWriters: 3); // Increased thread count

        Console.WriteLine("\nAll simulations finished. Press any key to exit.");
        Console.ReadKey();
    }
}



/* PRZYKŁAD DZIAŁANIA

Reader 1 reads value: 0
Reader 2 reads value: 0
Reader 3 reads value: 0
[Writer 2] wrote value: 66
[Writer 1] wrote value: 430
Reader 2 reads value: 430
Reader 3 reads value: 430
Reader 1 reads value: 430
[Writer 2] wrote value: 822
Reader 2 reads value: 822
Reader 3 reads value: 822
Reader 1 reads value: 822
[Writer 1] wrote value: 827
Reader 2 reads value: 827
Reader 1 reads value: 827
Reader 3 reads value: 827
[Writer 2] wrote value: 234
Reader 2 reads value: 234
Reader 3 reads value: 234
Reader 1 reads value: 234
[Writer 1] wrote value: 497
Reader 2 reads value: 497
Reader 3 reads value: 497
Reader 1 reads value: 497
[Writer 2] wrote value: 51
Reader 2 reads value: 51
Reader 3 reads value: 51
Reader 1 reads value: 51
[Writer 1] wrote value: 556
Reader 3 reads value: 556
Reader 2 reads value: 556
Reader 1 reads value: 556
[Writer 2] wrote value: 669
Reader 1 reads value: 669
Reader 2 reads value: 669
Reader 3 reads value: 669
[Writer 1] wrote value: 833
Reader 3 reads value: 833
Reader 1 reads value: 833
Reader 2 reads value: 833
[Writer 2] wrote value: 54
Reader 2 reads value: 54
Reader 3 reads value: 54
Reader 1 reads value: 54
[Writer 1] wrote value: 670
Reader 2 reads value: 670
Reader 3 reads value: 670
Reader 1 reads value: 670
[Writer 2] wrote value: 369
Reader 1 reads value: 369
Reader 2 reads value: 369
Reader 3 reads value: 369
[Writer 1] wrote value: 991
Reader 2 reads value: 991
Reader 1 reads value: 991
Reader 3 reads value: 991
[Writer 2] wrote value: 625
Reader 3 reads value: 625
Reader 2 reads value: 625
Reader 1 reads value: 625
[Writer 1] wrote value: 704
Reader 1 reads value: 704
Reader 2 reads value: 704
Reader 3 reads value: 704
[Writer 2] wrote value: 402
Reader 1 reads value: 402
Reader 2 reads value: 402
Reader 3 reads value: 402
[Writer 1] wrote value: 70
Reader 2 reads value: 70
Reader 3 reads value: 70
Reader 1 reads value: 70
[Writer 2] wrote value: 663
*/