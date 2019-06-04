using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApp8
{

    abstract class Output
    {
        public virtual void FirstName(Individual c)
        {
            Console.WriteLine(c.FirstName);
        }
        public virtual void City(Individual c)
        {
            Console.WriteLine(c.City);
        }
        public virtual void Number(Individual c)
        {
            Console.WriteLine(c.Number);
        }
        public abstract void Other(Individual c);

        void Space()
        {
            Console.WriteLine(" ,");
        }

        // The template method
        public void AllInfo(Individual c)
        {
            FirstName(c);
            Space();
            City(c);
            Space();
            Number(c);
            Space();
            Other(c);
        }
    }

    class MainCompanyOutput : Output
    {
        public override void Other(Individual c)
        {
            Console.WriteLine(c.McNumber);
        }
    }

    class PersonOutput : Output
    {
        public override void Other(Individual c)
        {
            Console.WriteLine(c.PersonalNumber);
        }
    }
}
