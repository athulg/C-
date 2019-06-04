using System;
using System.Collections.Generic;
using System.Text;

namespace ConsoleApp8
{

    class PersonCRUDIterator : IIterator
    {
        IPersonColl list = null;
        int index = 0;
        //private Dictionary<string, Individual> details;


       public PersonCRUDIterator(IPersonColl pc)
       {
            //list.Add(details);
            list = pc;
       } 

       /* public PersonCRUDIterator(Dictionary<string, Individual> details)
        {
            this.details = details;
        }*/



        public Individual FirstItem
        {
            get
            {
                index = 0;
                return list[index];
            }
        }


        public Individual NextItem
        {
            get
            {
                index += 1;

                if (IsDone == false)
                {
                    return list[index];
                }
                else
                {
                    return null;
                }
            }
        }


        public Individual CurrentItem
        {
            get
            {
                return list[index];
            }
        }


        public bool IsDone
        {
            get
            {
                if (index < list.Count)
                {
                    return false;
                }
                return true;
            }
        }

    }
}
