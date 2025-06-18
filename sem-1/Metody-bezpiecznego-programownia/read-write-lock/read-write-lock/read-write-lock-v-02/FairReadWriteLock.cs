using System;
using System.Threading;

namespace CustomLocks
{
    public class FairReadWriteLock : IReadWriteLock
    {
        private readonly object _gate = new object();
        private int _activeReaders = 0;
        private bool _isWriterActive = false;
        private int _waitingWriters = 0;
        private int _waitingReaders = 0; // Track waiting readers for fair wakeup

        public void ReadLock()
        {
            lock (_gate)
            {
                _waitingReaders++;
                // Readers wait if a writer is active OR if writers are waiting (to be fair to writers).
                while (_isWriterActive || _waitingWriters > 0)
                {
                    Monitor.Wait(_gate);
                }
                _waitingReaders--;
                _activeReaders++;
            }
        }

        public void ReadUnlock()
        {
            lock (_gate)
            {
                _activeReaders--;
                // If this was the last reader and writers are waiting, wake one writer.
                // The WriteUnlock logic handles waking readers if no writers are pending.
                if (_activeReaders == 0 && _waitingWriters > 0)
                {
                    Monitor.Pulse(_gate); // Wake one waiting thread (hopefully a writer)
                }
            }
        }

        public void WriteLock()
        {
            lock (_gate)
            {
                _waitingWriters++;
                // Writers wait if any readers are active OR another writer is active.
                while (_activeReaders > 0 || _isWriterActive)
                {
                    Monitor.Wait(_gate);
                }
                _waitingWriters--;
                _isWriterActive = true;
            }
        }

        public void WriteUnlock()
        {
            lock (_gate)
            {
                _isWriterActive = false;
                // Fair unlocking: Prioritize waiting writers. If none, wake all waiting readers.
                if (_waitingWriters > 0)
                {
                    Monitor.Pulse(_gate); // Wake one waiting writer
                }
                else if (_waitingReaders > 0)
                {
                    Monitor.PulseAll(_gate); // Wake all waiting readers
                }
                // If no one is waiting, pulses do nothing.
            }
        }
    }
}