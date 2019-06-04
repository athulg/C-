using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
//using Microsoft.VisualBasic.FileIO;

namespace ConsoleApp8
{
    class ReadFile
    {
        public ReadFile(List<Individual> cList, string path)
        {
            using (StreamReader sr = File.OpenText(path))
            {
                string s = "";
                while ((s = sr.ReadLine()) != null)
                {
                    Individual newIndividual = new Individual();
                    string[] properties = s.Split('|');
                    newIndividual.FirstName = properties[0];
                    newIndividual.City = properties[1];
                    newIndividual.Number = long.Parse(properties[2]);
                    cList.Add(newIndividual);
                }
            }
        }
    }

    class SaveFileOperation
    {
        public SaveFileOperation(List<Individual> cList, string path)
        {
            using (StreamWriter sw = File.CreateText(path))
            {
                foreach (Individual obj in cList)
                {
                    sw.Write(obj.FirstName + "|" + obj.City + "|" + obj.Number.ToString());
                    sw.Write(Environment.NewLine);
                }
            }
        }
    }
}