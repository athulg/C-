using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;

namespace ConsoleApp8
{

    class Person : Individual
    {
        private string firstName = "";

        public Person() : base() { }

        public Person(string n) : base(n)
        {
            firstName = n;
        }
        public int Number;
        public string FirstName;
        public string City;
        

        //public Person() { }

        public Person(int number, string firstName, string city, string birthday)
        {
            Number = number;
            FirstName = firstName;
            City = city;
            
        }
    }

}
