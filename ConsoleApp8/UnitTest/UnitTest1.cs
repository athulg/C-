using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using ConsoleApp8;

namespace UnitTest
{

        [TestClass]
        public class PersonCRUDTests
        {
        PersonCRUD pc = new PersonCRUD();
        Individual ind = new Individual();

        public void CountNumberofElements()
        {
            PersonCRUD personCRUD = new PersonCRUD();
        }

        [TestMethod]
            public void Constructor()
            {
                PersonCRUD pc = new PersonCRUD();
                Assert.IsTrue(pc != null);
            }
        [TestMethod]
        public void CanAddIndividuals()
        {

            ind = new Individual(1, "August", "Karlsson", "15.06.99", "Uppsala", "Upp", "Kungsgatan");
            ind = new Company("BASE10", "Uppsala");
            pc.Add(ind);
        }

            [TestMethod]
            public void Count()
            {
                PersonCRUD pc = new PersonCRUD();
                Assert.IsTrue(pc.Count == 0);
                Person ind = new Person("Andrew");
                pc.Add(ind);
                Assert.IsTrue(pc.Count == 1);
            }

            [TestMethod]
            public void SearchEmpty()
            {
                PersonCRUD pc = new PersonCRUD();
                List<Individual> SearchList = pc.Search("Test");
                Assert.IsFalse(SearchList.Any());
            }

            [TestMethod]
            public void GetIterator()
            {
                PersonCRUD pc = new PersonCRUD();
                IIterator iter = pc.GetIterator();
            }

            [TestMethod]
            public void ReadFile()
            {
                PersonCRUD pc = new PersonCRUD();
                Person ind1 = new Person("Anna");
                pc.Add(ind1);
                pc.Save();
                pc.Read();
            }
            IIterator StandardRun()
            {
                PersonCRUD pc = new PersonCRUD();
                pc.Add(new Person("Andrew Park"));
                pc.Add(new Person("Maria Clara"));
                return pc.GetIterator();
            }

        }
}