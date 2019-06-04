using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;

namespace ConsoleApp8
{
    public interface IIndividual
    {
        string FirstName { get; set; }
        string City { get; set; }
        long Number { get; set; }
        void PrintTypeAndID();
        string PersonalNumber { get; set; }
        string McNumber { get; set; }
    }
    class Individual : IIndividual
    {
        protected static long id = 0;
        protected string firstname = "Unnamed";
        protected string city = "Null";
        protected long number = 0;
        private Hashtable contactList = new Hashtable();

        /**
            Constructor.
         */
        public Individual()
        {
            id++;
        }

        /**
            Constrcutor with name input.
         */
        public Individual(string n)
        {
            firstname = n;
            id++;
        }

        public virtual string FirstName
        {
            get { return firstname; }
            set { firstname = value; }
        }
        public string City
        {
            get { return city; }
            set { city = value; }
        }

        public long Number
        {
            get { return number; }
            set { number = value; }
        }

        public virtual void PrintTypeAndID()
        {
        }

        public virtual string PersonalNumber
        {
            get; set;
        }

        public virtual string McNumber
        {
            get; set;
        }
    }
}

/*
 public string City;
        public string State;
        public string Street;
        private Hashtable contactList = new Hashtable();

        public object Value { get; internal set; }

        

        public Individual(int personID, string firstName, string lastName, string birthday, string city, string state, string street) : base(personID, firstName, lastName, birthday)
        {
            base.PersonID = personID;
            base.FirstName = firstName;
            base.LastName = lastName;
            base.Birthday = birthday;
            City = city;
            State = state;
            Street = street;
        }

        public int GetPersonID()
        { return base.PersonID; }
        public void SetPersonID(int value)
        { base.PersonID = value; }

        public string GetFirstName()
        { return base.FirstName; }
        public void SetFirstName(string value)
        { base.FirstName = value; }

        public string GetLastName()
        { return base.LastName; }
        public void SetLastName(string value)
        { base.LastName = value; }

        public string GetBirthday()
        { return base.Birthday; }
        public void SetBirthday(string value)
        { base.Birthday = value; }

        public void SetCity(string c)
        {
            this.City = c;
        }
        public string GetCity()
        {
            return this.City;
        }

        public void SetState(string s)
        {
            this.State = s;
        }
        public string GetState()
        {
            return this.State;
        }

        public void SetStreet(string st)
        {
            this.Street = st;
        }
        public string GetStreet()
        {
            return this.Street;
        }
*/
