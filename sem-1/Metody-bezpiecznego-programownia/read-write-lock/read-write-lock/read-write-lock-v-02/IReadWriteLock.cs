namespace CustomLocks
{
    public interface IReadWriteLock
    {
        void ReadLock();
        void ReadUnlock();
        void WriteLock();
        void WriteUnlock();
    }
}