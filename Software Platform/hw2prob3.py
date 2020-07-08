# -*- coding: utf-8 -*-
"""
Created on Fri Apr 10 10:09:41 2020

@author: main
"""
from prob1 import Spreadsheet
class SmartSpreadsheet(Spreadsheet):
    
    def __init__(self):
        super().__init__()
        self.oppoint_and_idx={}    #dictionary that put starting point of operand index as key and index as value
        self.oppoint_and_function_dict={} #dictionary that put starting point of operand index as key and defined function as value
        
    def set_function(self, idx, function, operand_idx):
        fl=operand_idx[0] #firstletter : column 
        sl=operand_idx[1:] # second part: row
        if ord(fl)>=97:  #lowercase
            column=ord(fl)-96
        else:
            column=ord(fl)-64   #capital

        row=int(sl)
        oppoint=row*80-8*(11-column)   #starting index point of operand_idx
        
    
        self.oppoint_and_function_dict[oppoint]=function  #assign function
        self.oppoint_and_idx[oppoint]=idx   #assign idx
        k=(function)(self.FINDING_DICT[oppoint])  #execute the function with value of the operand_idx cell
        
        if type(k)!=int and type(k)!= str and type(k)!= bool or type(idx)!=str:  #from now on set_value function with idx and value 'k'
            raise TypeError
        
        elif type(k)== int or str or bool and type(idx)==str:
           
            fl=idx[0] #firstletter
            sl=idx[1:]  # second part
            if fl not in ['A','B','C','D','E','F','G','H','I','J','a','b','c','d','e','f','g','h','i','j']:
                raise IndexError
            elif sl not in ['1','2','3','4','5','6','7','8','9','10']:
                raise IndexError
                
            else: 
                if ord(fl)>=97:
                    column=ord(fl)-96
                else:
                    column=ord(fl)-64
                
                row=int(sl)
                idx_point=row*80-8*(11-column)
                
                self.FINDING_DICT[idx_point]=k
                
                
                if idx_point==0:
                    self.x=str(k).ljust(7)+self.x[7:]
             
                elif idx_point==792:
                    self.x=self.x[:792]+str(k).ljust(7)
                
                else:
                    self.x=self.x[:idx_point]+str(k).ljust(7)+self.x[idx_point+7:]
                    
        
    
    def set_value(self,idx,value):   # Defined new set_value function so that changing previously operand idx cell can change the result accordingly
        Spreadsheet.set_value(self,idx,value) #change sheet as previously defined set_value function
        fl2=idx[0] 
        sl2=idx[1:]
        if ord(fl2)>=97:
            column2=ord(fl2)-96
        else:
            column2=ord(fl2)-64

        row2=int(sl2)
        changed_point_new=row2*80-8*(11-column2)
        if changed_point_new in list(self.oppoint_and_function_dict.keys()): #which means cell with this starting point has been operand idx cell before
            self.__class__.set_function(self,self.oppoint_and_idx[changed_point_new],self.oppoint_and_function_dict[changed_point_new],idx) # idx is new operand idx and assigned function will work again at assigned index
        else:
            pass
