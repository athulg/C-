namespace ConsoleApp8
{
    interface IIterator
    {
        Individual CurrentItem { get; }
        Individual FirstItem { get; }
        bool IsDone { get; }
        Individual NextItem { get; }
    }

    class NullPersonCRUDIterator : IIterator
    {
        public Individual FirstItem { get { return null; } }
        public Individual NextItem { get { return null; } }
        public Individual CurrentItem { get { return null; } }
        public bool IsDone { get { return true; } }
    }
}