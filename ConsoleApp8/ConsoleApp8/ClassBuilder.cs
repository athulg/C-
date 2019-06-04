using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;



namespace ConsoleApp8
{

    /*class DependencyClass
    {
        public static List<IIndividual> GetIndividual()
        {
            return new List<IIndividual>();
        }
        public static IIndividual Individuals()
        {
            return new Individual();
        }
        public static IPersonColl PersonCollection()
        {
            return new PersonCRUD();
        }

    }*/

    class PersonCRUD : IPersonColl, IEnumerable
    {

        private List<Individual> individualList = null;
        //private Dictionary<string, Individual> details;
        private string path = "PersonCRUD.txt";

        public PersonCRUD()
        {
            //details = new Dictionary<string, Individual>();
            individualList = new List<Individual>();
        }


       public IIterator GetIterator()
        {
            return new PersonCRUDIterator(this);
        }

        public Individual this[int itemIndex]
        {
            get
            {
                if (itemIndex < individualList.Count)
                {
                    return individualList[itemIndex];
                }
                else
                {
                    return null;
                }
            }
            set
            {
                individualList.Add(value);
            }
        }

        public int Count
        {
            get
            {
                return individualList.Count;
            }
        }

        public void Add(Individual a)
        {
            //individualList.Add(a.FirstName, a);
            individualList.Add(a);

            //foreach (var item in individualList)
            //{
            //    if (individualList != null)
            //    {
            //        Console.WriteLine(item.Value.FirstName);
            //    }

            //}
        }


        public void Remove(Individual a)
        {
            individualList.Remove(a);
        }


        public List<Individual> Search(string firstName)
        {
            List<Individual> searchList = new List<Individual>();
            foreach (Individual obj in individualList)
            {
                string firstname_ = obj.FirstName;
                if (firstname_.Contains(firstName))
                {
                    searchList.Add(obj);
                }
            }
            return searchList;
        }

        public List<Individual> SearchNumber(long number)
        {
            List<Individual> searchList = new List<Individual>();
            foreach (Individual obj in individualList)
            {
                string number_ = obj.Number.ToString();
                if (number_.StartsWith(number.ToString()))
                {
                    searchList.Add(obj);
                }
            }
            return searchList;
        }
        public List<Individual> IndividualList
        {
            get { return individualList; }
            set { individualList = value; }
        }
       /* public void PrintIndividual()
        {
            foreach (var individual in individualList)
            {
                individual.Print();
            }

            foreach (var item in individualList)
            {
                string text = item.FirstName.ToString() + "," + item.FirstName + " " + item.GetCity() + " " + item.Number();
                Console.WriteLine(text);
            };
        }
        */
        


        IEnumerator IEnumerable.GetEnumerator()
        {
            return (IEnumerator)GetIterator();
        }


        public void ReadFromCSV(string str)
        {
            string[] values = str.Split(',');
            Individual temp = new Individual();
            temp.FirstName = values[0];
            temp.City = values[1];
            temp.Number = Convert.ToInt64(values[2]);
            individualList.Add(temp);
        }

        public void CSVBulkReading(string str)
        {
            foreach (var eachLine in str.Split(new string[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries))
                ReadFromCSV(eachLine);
        }
        public void Add(object a)
        {
            throw new NotImplementedException();
        }
    }
}


