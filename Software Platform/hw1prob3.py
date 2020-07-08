# -*- coding: utf-8 -*-
"""
Created on Fri Apr  3 15:48:08 2020

@author: main
"""
def comment_remover(x):
    line_list=x.splitlines()    #makes a list of lines
    for n,line in enumerate(line_list):   
            if '#' in line:   
                well_loc=line.find('#')  #index location of '#'
                
                new_line=line[:well_loc]   #delete # and comment after #
                line_list[n]=new_line      # at the same location replace the original line with new line without comment

    a='\n'.join(line_list)  # adding each lines with \n
    return a
    
