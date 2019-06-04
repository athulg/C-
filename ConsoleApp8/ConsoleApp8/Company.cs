using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;

namespace ConsoleApp8
{
    class Company : Individual
    {
        private Hashtable companyList = new Hashtable();
        string CompanyName { get; set; }
        string CompanyCity { get; set; }


        public Company(string companyname, string companycity)
        {
            this.CompanyName = companyname;
            this.CompanyCity = companycity;
        }
    }
}
