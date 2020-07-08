# -*- coding: utf-8 -*-
"""
Created on Wed Apr  1 21:18:55 2020

@author: main
"""

def prime_factorizer(a):
    list1=[]     #empty list
    i=1         #denumerator
    k=0         #quotient
    while a>i:
         
        i+=1    #starts to divide with 2 and if a=1 the result is empty list
        while True:
            if a%i==0:
                a/=i     #quotient
                k+=1    
            elif a%i!=0:
                list1.append((i,k))   # stop dividing by i and i times k tuple is included in list1
                k=0     #initialize k
                break
        if (a==1):      #it means that it is divided as much as possible
            list1=[x for x in list1 if x[1]!=0]    # if the number which is not factor is included, we need to exclude it
                
            break
    return list1
    
