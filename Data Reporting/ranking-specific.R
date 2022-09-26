#Read local data files for course 2021-2022

course21_22<-read.csv("/Users/linlizhou/Documents/LASELL/working on/Student Info by Course and Fiscal Year.csv")
#course enrollment data for fall/spring/summer main, session1, and 2
#run from report manager: https://reports.lasell.edu/Reports/report/IR/Student%20Info%20by%20Course%20and%20Fiscal%20Year
#filter out unique rows
nrow(course21_22)
nrow(unique(course21_22))
course21_22_unqiue<-unique(course21_22)

######################################################################################
##############external reporting for the Princeton Review 2022-8-19 due##############


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



######################################################################################
##############external reporting for the US News 2022-10-14 due##############
##############question: Amount of curriculum for undergrad completion/online program###########
library(rvest)
#read html and parse it into R readable contents
pg<-read_html("https://www.lasell.edu/graduate-studies/academics/bsba.html#Curriculum-Section")
#look at it (a list object that contains the tree-like structure)
pg
#search using nodes (html tags or css class) and print text
#pg %>% html_nodes("body")%>%html_text()#indeed all contents of body
#search using css class as nodes
my.title<-pg %>% html_nodes(".code")%>%html_text() 
  my.title%>%length()#we must add a . before the class name
  
#save to df
my.df<-data.frame(BSBA=my.title)



##############web2
pg.2<-read_html("https://www.lasell.edu/graduate-studies/academics/psychology.html#Curriculum-Section")
#make a list of course titles
my.title2<-c()#initialize a empty list
for (i in 1:30 )  { #estimate 30 i
  my.title2[[i]]<-#have to use [[]] to indicate the list index
    pg.2%>%html_nodes(
      xpath=paste0("/html/body/div[1]/main/div/div[1]/div[2]/div[2]/table/tbody/tr[",i,"]/td[1]"#paste the xpath using i
      )) %>% html_text()# print out corresponding text to nodes
  i<-i+1 #add one to go to next index
}

library(stringr)
#remove empty and not in format of -- using any capital letters follow by any number
my.title2<-my.title2[str_detect(my.title2,"[A-Z][0-9]")]
my.title2%>%length()

#save to df
length(my.title2)=length(my.title)
my.df<-my.df%>%mutate(Psych=my.title2)

##############web3
pg.3<-read_html("https://www.lasell.edu/graduate-studies/academics/communication.html")
#make a list of course titles
my.title3<-c()#initialize a empty list
for (i in 1:30 )  { #estimate 30 i
  my.title3[[i]]<-#have to use [[]] to indicate the list index
    pg.3%>%html_nodes(
      xpath=paste0("/html/body/div[1]/main/div/div[1]/div[2]/div[2]/table/tbody/tr[",i,"]/td[1]"#paste the xpath using i
      )) %>% html_text()# print out corresponding text to nodes
  i<-i+1 #add one to go to next index
}

library(stringr)
#remove empty and not in format of -- using any capital letters follow by any number
my.title3<-my.title3[str_detect(my.title3,"[A-Z][0-9]")]
my.title3%>%length()


#save to df
length(my.title3)=length(my.title)
my.df<-my.df%>%mutate(Com=my.title3)






