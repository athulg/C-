using System;
using System.Collections.Generic;
using System.Linq;


namespace ConsoleApp8
{
    interface IPersonColl
    {

        //void PrintIndividual();
        IIterator GetIterator();
        Individual this[int itemIndex] { set; get; }
        int Count { get; }
        void Add(Individual a);
        void Remove(Individual a);
        List<Individual> Search(string firstName);
        List<Individual> SearchNumber(long number);
        //void Read();
        void ReadFromCSV(string str);
        void CSVBulkReading(string str);
        //void Save();

    }
}
