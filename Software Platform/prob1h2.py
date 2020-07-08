# -*- coding: utf-8 -*-
"""
Created on Wed Apr  8 22:51:37 2020

@author: main
"""

class Spreadsheet():
   
    def __init__(self):
        A='       ,'*9
        B=(A+'\n')*9+A
        self.x=B
    
    def __str__(self):
        return self.x
        
    
    def set_value(self, idx, value):
        
        
        if type(value)!=int and type(value)!= str and type(value)!= bool:
            raise TypeError
        
        elif type(value)== int or str or bool:
           
            fl=idx[0] #firstletter
            sl=idx[1:]  # second part
            if fl not in ['A','B','C','D','E','F','G','H','I','J']:
                raise IndexError
            elif sl not in ['1','2','3','4','5','6','7','8','9','10']:
                raise IndexError
                
            else: 
                
                row=ord(fl)-64
                column=int(sl)
                starting_point=(row*82-2)-8*(11-column)
                
                if starting_point==0:
                    self.x=str(value).center(7)+self.x[7:]
             
                elif starting_point==810:
                    self.x=self.x[:810]+str(value).center(7)
                
                else:
                    self.x=self.x[:starting_point]+str(value).center(7)+self.x[starting_point+7:]
                    
