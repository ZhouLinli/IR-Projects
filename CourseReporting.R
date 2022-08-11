#Read local data files for course 2021-2022

course21_22<-read.csv("/Users/linlizhou/Documents/LASELL/working on/Student Info by Course and Fiscal Year.csv")
#course enrollment data for fall/spring/summer main, session1, and 2
#run from report manager: https://reports.lasell.edu/Reports/report/IR/Student%20Info%20by%20Course%20and%20Fiscal%20Year
#filter out unique rows
nrow(course21_22)
nrow(unique(course21_22))
course21_22_unqiue<-unique(course21_22)


#external reporting for the Princeton Review 2022-8-19 due


#1.goal -question 10 and 11: confirm the following are offered as undergraduate course
dplyr::glimpse(course21_22_unqiue)
c("Entrepreneurial Leadership","Entrepreneurial Management","Introduction to Entrepreneurship","Introduction to New Business Ventures","New Product Development","New Venture Management","Venture Capital & Private Equity") %in% course21_22_unqiue$Course_Name
#cannot find exact match except "New Product Development"


#filter business and management school and see the courses offered there
library(dplyr)
#investigate department names
course21_22_unqiue %>% dplyr::group_by(Course_Department) %>% count()
#list business school courses
course21_22_unqiue%>%filter(Course_Department=="BUSS")%>%group_by(Course_Name)%>%count()
#find two courses that contains Entrepreneur: "Amer Entrepreneurs: Trends & Innovation" and "Special Topics in Entrepreneurship"
#then, need to confirm with Bruce for question 10 and 11


#let's investigate which courses does Bruce teach, since Bruce is the only faculty for the entreprenurship program
glimpse(course21_22_unqiue)
course21_22_unqiue %>% filter(Course_Department=="BUSS")%>%
  group_by(Instructors)%>%count() #find out McKinnon, Bruce

course21_22_unqiue%>% filter(Instructors=="McKinnon, Bruce" & Course_Department=="BUSS") %>%group_by(Course_Name) %>% count() #there are 7 courses
#asked Eric, "Amer Entrepreneurs: Trends & Innovation","Entrepreneurship & Venture Creation", "Managing the Growing Company" and "Special Topics in Entrepreneurship" are the four courese entrepreneurship-related.





#2.goal-question 15: What was the total enrollment (full-time and part-time) in your undergraduate entrepreneurship offerings for the 2021-2022 academic year?

#based on the 4 courses are related with entrepreneurship, I can count the enrollment based on the n in the following formula
t3<-course21_22_unqiue%>%filter(
  Course_Name %in% c("Amer Entrepreneurs: Trends & Innovation", "Entrepreneurship & Venture Creation", "Managing the Growing Company", "Special Topics in Entrepreneurship") 
) %>% group_by(Course_Name) %>% summarise(course_appearance=n())

sum(t3$course_appearance)#61 students



#goal-question 15a: within those students who enrolled in the entrepreneurship-related course, count their unique majors
glimpse(course21_22_unqiue)
course21_22_unqiue%>%filter(
  Course_Name %in% c("Amer Entrepreneurs: Trends & Innovation", "Entrepreneurship & Venture Creation", "Managing the Growing Company", "Special Topics in Entrepreneurship") 
) %>% group_by(Major) %>% summarise(major_appearance=n())#14 majors

















