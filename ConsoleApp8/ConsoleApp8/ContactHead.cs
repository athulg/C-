using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApp8
{
    class ContactHead
    {
        private IIndividual _builder;
        public ContactHead(IIndividual builder)
        {
            _builder = builder;
        }
        void ContactConstruct()
        {
            _builder.City = "Stockholm";
            _builder.Number = 911;
        }
    }
}