# -*- coding: utf-8 -*-
"""
Created on Thu Apr  9 21:40:51 2020

@author: main
"""
from prob1 import Spreadsheet
class PermanentSpreadsheet(Spreadsheet):
    
  
    def __init__(self):
        super().__init__()   #initializer from Spreadsheet class
        
    
    def export_sheet(self,filename):
        if type(filename)!= str:
            raise TypeError
        elif type(filename)==str:
            
            f=open(filename+'.txt','w')   #create new text file as 'filename
            f.write(self.x)#put self.x (changed sheet)            
            
            g=open(filename+'dict.txt','w')  #saving the FINDING DICT together
            for key, mem in self.FINDING_DICT.items():  # to retain the value type of the values
                if type(mem)==str:  
                    pass
                elif type(mem)==int:
                    mem2='@@@'+str(mem)              #gives sign of being integer
                    self.FINDING_DICT[key]=mem2
                elif type(mem)==bool:
                    if mem!=True:        #In case of False it needs to go to the second case
                        mem2='!!!'+str(mem)
                    elif mem==True:
                        mem2='???'+str(mem)
                    self.FINDING_DICT[key]=mem2
            g.write(str(self.FINDING_DICT))
            f.close()
            g.close()
            
    def import_sheet(self,filename):
        f=open(filename+'.txt','r')   # reading mode 
        self.x=f.read()    #sheet is the readable sheet saved
        g=open(filename+'dict.txt','r')
        new_dict=g.read()
        new_dict1=new_dict.replace('{','')   
        new_dict2=new_dict1.replace('}','')  #remove parenthesis
        dict_member=new_dict2.split(sep=', ')   # string form of dictionary is separated by ', '
    
        real_new_dict={}  # creating new empty dictionary
        
        for member in dict_member:   
            double_dot_idx=member.find(':')
            sp=int(member[:double_dot_idx])  #first two letter is number (starting point)
            
            val=member[4:]  #after : here comes the assigned value
            
            if '@@@' in val:            #change the string to original type according to the sign
                val1= val.replace('\'','')
                val2=val1.replace('@@@','')
                val3=int(val2)
               
            elif '!!!' in val:
                val3=False
                
            elif '???' in val:
                val3=True
                
            else:
                val1=val.replace(' \'','')   #in case of string it becomes" 'string'" so we need to change it 
                val3=val1.replace('\'','')
        
            real_new_dict[sp]=val3
        self.FINDING_DICT=real_new_dict
        f.close()
        g.close()
            
           