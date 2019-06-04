using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApp8
{
    class NullPersonCRUD : IPersonColl
    {
        public IIterator GetIterator() { return null; }
        public Individual this[int itemIndex] { get { return null; } set { } }
        public int Count { get { return 0; } }
        //public void Read();
        //public void Save();
        public void Add(Individual v) { }
        public void Remove(Individual v) { }
        public void RemoveDuplicates() { }
        public List<Individual> Search(string firstname) { return null; }
        public List<Individual> SearchNumber(long number) { return null; }
        public void ReadFromCSV(string str) { }
        public void CSVBulkReading(string str) { }
    }
}
