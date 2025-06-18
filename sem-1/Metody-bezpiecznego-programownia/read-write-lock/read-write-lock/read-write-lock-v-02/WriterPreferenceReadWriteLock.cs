using System;
using System.Threading;

namespace CustomLocks
{
    public class WriterPreferenceReadWriteLock : IReadWriteLock
    {
        private readonly object _gate = new object();
        private int _activeReaders = 0;
        private bool _isWriterActive = false;
        private int _waitingWriters = 0;

        public void ReadLock()
        {
            lock (_gate)
            {
                // Readers must wait if a writer is active OR if any writers are waiting.
                while (_isWriterActive || _waitingWriters > 0)
                {
                    Monitor.Wait(_gate);
                }
                _activeReaders++;
            }
        }

        public void ReadUnlock()
        {
            lock (_gate)
            {
                _activeReaders--;
                // If this was the last reader and writers are waiting,
                // pulse the gate. A waiting writer should be able to proceed.
                // All waiting threads (readers and writers) re-check their conditions upon waking.
                if (_activeReaders == 0 && _waitingWriters > 0)
                {
                    Monitor.PulseAll(_gate); // PulseAll to ensure waiting writers get a chance to re-evaluate
                }
            }
        }

        public void WriteLock()
        {
            lock (_gate)
            {
                _waitingWriters++;
                // Writers must wait if any readers are active OR another writer is active.
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
                // Wake up all waiting threads. Writers will get priority due to the
                // conditions in ReadLock (_waitingWriters > 0) and WriteLock.
                Monitor.PulseAll(_gate);
            }
        }
    }
}