using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApp8
{
    class MainCompany : Individual
    {
        protected string name = "";
        protected string mcNumber = "Null";

        public MainCompany() : base() { }

        public MainCompany(string n) : base(n)
        {
            name = n;
        }

        public override string FirstName
        {
            get { return firstname; }
            set
            {
                name = value;
            }
        }
        public override void PrintTypeAndID()
        {
            Console.WriteLine("MainCompany with ID: " + id);
        }
        public override string McNumber
        {
            get
            {
                return mcNumber;
            }
            set
            {
                long mcNumberLong = Convert.ToInt64(value);
                if (mcNumberLong <= 6)
                {
                    mcNumber = value;
                }
            }
        }
    }
}