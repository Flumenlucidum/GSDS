# -*- coding: utf-8 -*-
"""
Created on Wed Mar 18 15:33:28 2020

@author: main
"""
from yeardetection import year

def is_digit(str):
  try:
      tmp = float(str)
      return True
  except ValueError:
      return False

while True:
    print("\n Enter the year you want")
    str = input("(A number 0 will terminate the program): ")
    print("<", str, "> typed in", end=".")
    if is_digit(str):
        num=eval(str)
        if (num == 0 ):
            print("The end")
            break
        elif(num>0):
            print(year(num))
        elif(num<0):
            print('year cannot be negative')
    elif str.isalpha():
        print(" It's an alphabet string: <", str, ">.")
    else:
        print(" It's neither a number nor an alphabet string: <", str, ">.")
   
    
    