# -*- coding: utf-8 -*-
"""
Created on Wed Mar 18 10:55:14 2020

@author: main
"""

# programming 0316 

def year(y):
    if y % 400 ==0:
        print ('366 days, 8784 hours, 527040 minutes, 31622400 seconds')
    elif y%100==0:
        print('365 days, 8760 hours, 525600 minutes, 31536000')
    elif y%4==0:
        print ('366 days, 8784 hours, 527040 minutes, 31622400 seconds')
    else:
        print('365 days, 8760 hours, 525600 minutes, 31536000')

