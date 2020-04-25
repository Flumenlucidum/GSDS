import csv   
import datetime as dt

def check_attendance(filename, classday, idnum):
    if type(filename)!=str or type(classday)!=tuple or type(idnum)!=int:
        raise TypeError
    elif type(classday[0])!=int or type(classday[1])!=int:
        raise TypeError
    
    else:
        try:
            dt.date(2020,classday[0],classday[1])   
        except ValueError:    # not a right day
            raise ValueError
        else:
            f=open(filename,'r',encoding='utf-8')  #open the csv file
            rdr =csv.reader(f)    #reading mode
            next(rdr,None)   #skip the header
            finding_dict={}   #make empty dictionary for making a pair
            classdayfinal=dt.date(2020,classday[0],classday[1])
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
            c=dt.timedelta(minutes=0)  #comparing unit
            if idnum not in finding_dict.keys() :  #student is not in the record so it is absent
                return False
            elif classdayfinal not in finding_dict[idnum].keys():   #there is student but he or she was absent on that day
            
                return False
            else:
                for a in finding_dict[idnum][classdayfinal]:
                    after=dt.datetime.combine(classdayfinal,a[1])
                    before=dt.datetime.combine(classdayfinal,a[0])
                    dif=after-before
                    c+=dif
                if c>=dt.timedelta(minutes=65):
                    return True
                else: 
                    return False
            f.close()