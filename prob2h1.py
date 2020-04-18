# -*- coding: utf-8 -*-
"""
Created on Thu Apr  2 21:51:02 2020

@author: main
"""

def list_accumulator(a):
    k=[]    # creating an empty list
    def sub_list_accumulator(b,i=0,c=0): 
        
        for x in b:
            if type(x)==int:
                i+=x    #adding every integer element
            elif type(x)==list:
                sub_list_accumulator(x)   #recursive form of function so that regardless of the list structure, we can add all the numbers
        c+=i   #adding all the sum of integers 
        nonlocal k   #k is outside of scope
        k.append(c)
    
    sub_list_accumulator(a)  # execute sub_list_accumulator function
    
    return sum(k)