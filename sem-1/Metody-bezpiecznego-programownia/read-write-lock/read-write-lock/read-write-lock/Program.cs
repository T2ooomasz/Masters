using System;
using System.Reflection.PortableExecutable;
using System.Threading;

class Rwlock
{
    int i = 0; // protected by ``this'' object

    public void writer_acquire()
    {
        lock (this)
        {
            while (i != 0)
                Monitor.Wait(this);
            i = -1;
        }
    }

    public void writer_release()
    {
        lock (this)
        {
            i = 0;
            Monitor.PulseAll(this);
        }
    }

    public void reader_acquire()
    {
        lock (this)
        {
            while (i < 0)
                Monitor.Wait(this);
            i++;
        }
    }

    public void reader_release()
    {
        lock (this)
        {
            i--;
            if (i == 0)
                Monitor.Pulse(this);
        }
    }
}

class Program
{
    static int value = 0;
    static Rwlock rw = new Rwlock();

    static void Reader(object id)
    {
        while (true)
        {
            rw.reader_acquire();
            Console.WriteLine($"Reader {id} reads value: {value}");
            Thread.Sleep(100);
            rw.reader_release();
            Thread.Sleep(200);
        }
    }

    static void Writer(object id)
    {
        Random rand = new Random();
        while (true)
        {
            rw.writer_acquire();
            value = rand.Next(1000);
            Console.WriteLine($"[Writer {id}] wrote value: {value}");
            Thread.Sleep(150);
            rw.writer_release();
            Thread.Sleep(500);
        }
    }

    static void Main(string[] args)
    {
        for (int i = 1; i <= 3; i++)
        {
            new Thread(Reader).Start(i);
        }

        for (int i = 1; i <= 2; i++)
        {
            new Thread(Writer).Start(i);
        }

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