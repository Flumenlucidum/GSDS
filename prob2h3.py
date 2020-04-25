# -*- coding: utf-8 -*-
"""
Created on Fri Apr 17 16:18:24 2020

@author: main
"""

import csv   
import datetime as dt

def list_attendees(filename, timetuple):
    if type(filename)!=str or type(timetuple)!=tuple:
        raise TypeError
    elif any(type(timetuple[x])!=int for x in range(len(timetuple))):
        raise TypeError    #check whether the input is right type
    
    else:
        try:
            dt.datetime(2020,timetuple[0],timetuple[1],timetuple[2],timetuple[3],timetuple[4])   
        except ValueError:    # not a right day
            raise ValueError
        else:
            f=open(filename,'r',encoding='utf-8')  #open the csv file
            rdr =csv.reader(f)    #reading mode
            next(rdr,None)   #skip the header
            finding_dict={}   #make empty dictionary for making a pair
            classdayfinal=dt.date(2020,timetuple[0],timetuple[1])
            day_list=[]  # check which day had the class
            for line in rdr:
                student_id=int(line[0])   #student id 
        
                start_dt=dt.datetime.strptime(line[1],"%Y/%m/%d %H:%M:%S") #time which access began
                start_date=start_dt.date()       
               
                end_dt=dt.datetime.strptime(line[2],"%Y/%m/%d %H:%M:%S")  #ending time of the access
                a=dt.datetime.date(end_dt)  
                
                day_list.append(a)  #since the class does not overlap over a day
            
            
                
        
                if student_id not in finding_dict.keys():
                    finding_dict[student_id]={start_date:[[start_dt.time(),end_dt.time()]]}
                
                elif start_date not in finding_dict[student_id].keys():
                    finding_dict[student_id][start_date]=[[start_dt.time(),end_dt.time()]]
                
                elif start_date in finding_dict[student_id].keys():
                    finding_dict[student_id][start_date].append([start_dt.time(),end_dt.time()])
                
                
                else:
                    pass
            
    #finding_dict's key is student ID and value is another dictionary with key of the date and list of 
                #access record
            
            if classdayfinal not in day_list:
                raise ValueError  #if there was no class on the day input
            else:
                pass
            
            for student_id in finding_dict.keys():
                
                for k in finding_dict[student_id].keys():
                    final_range=[]
                    for a in finding_dict[student_id][k]:
                        
                        begin=a[0]
                        end=a[1]
                
                        
                        
                        if begin<=dt.time(11,0,0) and end>=dt.time(12,15,0):
                            final_range.append([dt.time(11,0,0),dt.time(12,15,0)])
        #only one access of this time period assures full attendance
                        elif end<=dt.time(11,0,0) or begin>=dt.time(12,15,0):
                            pass     # this type of attendance is not admitted
                        elif begin>=dt.time(11,0,0) or end<=dt.time(12,15,0):
                            if begin<=dt.time(11,0,0):
                                a[0]=dt.time(11,0,0)
                                #beginning before 11am is futile
                            if end>=dt.time(12,15,0):
                                a[1]=dt.time(12,15,0)
                                #class time is until 12:15pm
                            final_range.append(a)
        
        
        
                                
         
                        b = []
                        for begin,end in sorted(final_range):  #finding union of the access record
                            if b and b[-1][1] >= begin :
                                b[-1][1] = max(b[-1][1], end)  #fixing the last element of b if overlapped
                            else:
                                b.append([begin, end])
                        final_range=b
                    finding_dict[student_id][k]=final_range
            access=dt.time(timetuple[2],timetuple[3],timetuple[4])
            student_list=[]
            for stud in finding_dict.keys():
                
                if classdayfinal in finding_dict[stud].keys():
                    for list_mem in finding_dict[stud][classdayfinal]:
                        if access<list_mem[0]:   #time before the first access is non accessed time
                            pass
                        elif access>=list_mem[0] and access<list_mem[1]:
                            student_list.append(stud)  #add the student id to the list
                            
                        elif access>=list_mem[1]:
                            pass       
                else:
                    pass
           
            def bubblesort(l):  #bubblesort algorith , sorting the id numbers
                    def swap(i,j):
                        t=l[i]
                        l[i]=l[j]
                        l[j]=t
                        return
                    n=len(l)-1

                    for i in range(n,0,-1):
                        for j in range(i):
                            if (l[j]>=l[j+1]):
                                swap(j,j+1)
                    return l
            if student_list==[]:
                raise ValueError  # there is no accessed one in the time
            else:
                result=bubblesort(student_list)
                return result
            f.close()
            