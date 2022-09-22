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






###############################################################################
###############################################################################
##############ENROLLMENT (REGISTRAR BACKUP) 07/01/2021-06/30-2022##############
###############################################################################
###############################################################################

#library
library(readxl)
library(dplyr)
library(tidyverse)
library(writexl)
library(openxlsx)

#################load data
##2021 Summer II
ug21sum2<-read_excel("/Volumes/lasellshare/Faculty_Staff_Shares$/IR/Registrar Reports/2021 Summer/Summer II/Undergraduate Backup Data  Report 21SUII.xlsx")
gd21sum2<-read_excel("/Volumes/lasellshare/Faculty_Staff_Shares$/IR/Registrar Reports/2021 Summer/Summer II/GR/Graduate Backup Data  Report.xlsx")
##2021 Fall
ug21fa<-read_excel("/Volumes/lasellshare/Faculty_Staff_Shares$/IR/Registrar Reports/2021 Fall/FA2021 Undergraduate Backup Data  Report.xlsx")
g21fa<-read_excel("/Volumes/lasellshare/Faculty_Staff_Shares$/IR/Registrar Reports/2021 Fall/Grad/main.SI.S2 GR backup data.xlsx")
##2022 Winter
ug22wi<-read_excel("/Volumes/lasellshare/Faculty_Staff_Shares$/IR/Registrar Reports/2022 Winter/WI2022 UG Backup Data  Report.xlsx")
gd22wi<-read_excel("/Volumes/lasellshare/Faculty_Staff_Shares$/IR/Registrar Reports/2022 Winter/Grad Backup data WI22.xlsx")
##2022 Spring
ug22sp<-read_excel("/Volumes/lasellshare/Faculty_Staff_Shares$/IR/Registrar Reports/2022 Spring/UG/Spring 2022 UG Backup Data.xlsx")
gd22sp<-read_excel("/Volumes/lasellshare/Faculty_Staff_Shares$/IR/Registrar Reports/2022 Spring/Grad/Graduate SP22 Final Backup Data  Report.xlsx")
##2022 Summer Main&I
ug22sum.main.1<-read_excel("/Volumes/lasellshare/Faculty_Staff_Shares$/IR/Registrar Reports/2022 Summer/Main and I/SUM22 SES1 and Main UG Backup Data  Report - 2022-06-02T080930.592.xlsx")
gd22sum.main.1<-read_excel("/Volumes/lasellshare/Faculty_Staff_Shares$/IR/Registrar Reports/2022 Summer/Main and I/SUM22 MainSES1-Graduate Backup Data  Report (23).xlsx")

####################clean data headers
#investigate names 1)view df 2)find no headers and tackling those headers
#tackling ug21sum2
names(ug21sum2)<-ug21sum2[3,]#header is the third row
n<-nrow(ug21sum2)#prep the length
ug21sum2<-ug21sum2[4:n,]#select from 4th row to the end
ug21sum2<-ug21sum2%>%select(-`NA`)#remove NA col
#since no need to merge all, no need to care about each and every col
##compare larger col df to smaller col df; and select from larger col to see what's not exist
##names(ug21fa)%in%names(ug21sum2)
##names(ug21fa)[names(ug21fa) %in% names(ug21sum2) == "FALSE"]#no Age; SCHOOL that in larger data but not in smaller data

#tackling ug22wi
names(ug22wi)<-ug22wi[3,]#header is the third row
n<-nrow(ug22wi)#prep the length
ug22wi<-ug22wi[4:n,]#select from 4th row to the end
ug22wi<-ug22wi%>%select(-`NA`)#remove NA col

#tackling gd22sum.main.1
names(gd22sum.main.1)<-gd22sum.main.1[3,]#header is the third row
n<-nrow(gd22sum.main.1)#prep the length
gd22sum.main.1<-gd22sum.main.1[4:n,]#select from 4th row to the end
gd22sum.main.1<-gd22sum.main.1%>%select(-`NA`)#remove NA col

#tackling g21fa
names(g21fa)<-g21fa[6,]#header is the 6th row
n<-nrow(g21fa)#prep the length
g21fa<-g21fa[7:n,]#select from 7th row to the end
g21fa<-g21fa%>%select(-`NA`)#remove NA col

#tackling gd21sum2
names(gd21sum2)<-gd21sum2[6,]#header is the 6th row
n<-nrow(gd21sum2)#prep the length
gd21sum2<-gd21sum2[7:n,]#select from 7th row to the end
gd21sum2<-gd21sum2%>%select(-`NA`)#remove NA col

#tackling gd22wi
names(gd22wi)<-gd22wi[6,]#header is the 6th row
n<-nrow(gd22wi)#prep the length
gd22wi<-gd22wi[7:n,]#select from 7th row to the end
gd22wi<-gd22wi%>%select(-`NA`)#remove NA col


########################################################################################
####################selecting all variables needed#######################################
########################################################################################

##################################################################
####################ug data############
ug1<-ug21fa%>%select(`People Code Id`,`gov id`,`FT/PT`,Degree,`Transfer YN`,`College Attend`,Ethnicity,Gender,`Term Credits`,`Cum Credits`,Curriculum,`Birth Date`)%>%rename(Program=Curriculum)%>%
  mutate(Level="UG",term="21fall")#mutate identification col
ug2<-ug21sum2%>%select(`People Code Id`,`gov id`,`FT/PT`,Degree,`Transfer YN`,`College Attend`,Ethnicity,Gender,`Term Credits`,`Cum Credits`,Curriculum,`Birth Date`)%>%rename(Program=Curriculum)%>%mutate(Level="UG",term="21summer2")
ug3<-ug22sp%>%select(`People Code Id`,`gov id`,`FT/PT`,Degree,`Transfer YN`,`College Attend`,Ethnicity,Gender,`Term Credits`,`Cum Credits`,Curriculum,`Birth Date`)%>%rename(Program=Curriculum)%>%mutate(Level="UG",term="22spring")
ug4<-ug22sum.main.1%>%select(`People Code Id`,`gov id`,`FT/PT`,Degree,`Transfer YN`,`College Attend`,Ethnicity,Gender,`Term Credits`,`Cum Credits`,Curriculum,`Birth Date`)%>%rename(Program=Curriculum)%>%mutate(Level="UG",term="22summer.1main")
ug5<-ug22wi%>%select(`People Code Id`,`gov id`,`FT/PT`,Degree,`Transfer YN`,`College Attend`,Ethnicity,Gender,`Term Credits`,`Cum Credits`,Curriculum,`Birth Date`)%>%rename(Program=Curriculum)%>%mutate(Level="UG",term="22winter")
#check selected vars
sapply(list(ug1,ug2,ug3,ug4,ug5), function(x) ncol(x))#same 14

#merge to one
ug.ipeds<-plyr::join_all(list(ug1,ug2,ug3,ug4,ug5),type="full")%>%unique()#remove rows that has exact same value across all columns
#check
sapply(list(ug1,ug2,ug3,ug4,ug5), function(x) nrow(x))%>%sum()#2935
nrow(ug.ipeds)#match="first" #2935
  
#create degree-seeking col
ug.ipeds<-ug.ipeds%>% mutate(degree.t=case_when(Degree %in% c("NON","Non Matriculated")~"Non-degree",Degree!="NON" & Degree!="Non Matriculated" ~"Degree-seeking"))%>%select(-Degree)
#check
names(ug.ipeds)

##################################################################
####################gd data############
gd1<-g21fa%>%select(`People Code Id`,`gov id`,`Class level`,Gender,`FT/PT`,Ethnicity,`Term Credits`,Degree,`Birth Date`)%>%rename(Program=Degree)%>%#have no transfer and college attend
  mutate(Level="GD",term="21fall")#mutate identification col
gd2<-gd21sum2%>%select(`People Code Id`,`gov id`,`Class level`,Gender,`FT/PT`,Ethnicity,`Term Credits`,Degree,`Birth Date`)%>%rename(Program=Degree)%>%mutate(Level="GD",term="21summer2")
gd3<-gd22sp%>%select(`People Code Id`,`gov id`,`Class level`,Gender,`FT/PT`,Ethnicity,`Term Credits`,Degree,`Birth Date`)%>%rename(Program=Degree)%>%mutate(Level="GD",term="22spring")
gd4<-gd22sum.main.1%>%select(`People Code Id`,`gov id`,`Class level`,Gender,`FT/PT`,Ethnicity,`Term Credits`,Degree,`Birth Date`)%>%rename(Program=Degree)%>%mutate(Level="GD",term="22summer.1main")
gd5<-gd22wi%>%select(`People Code Id`,`gov id`,`Class level`,Gender,`FT/PT`,Ethnicity,`Term Credits`,Degree,`Birth Date`)%>%rename(Program=Degree)%>%mutate(Level="GD",term="22winter")
#check selected vars: compare to ug, do not have transfer, college attend, and cum credit
sapply(list(gd1,gd2,gd3,gd4,gd5), function(x) ncol(x))#same 11

#merge to one
gd.ipeds<-plyr::join_all(list(gd1,gd2,gd3,gd4,gd5),type="full")%>%unique()#remove rows that has exact same value across all columns
#check
sapply(list(gd1,gd2,gd3,gd4,gd5), function(x) nrow(x))%>%sum()#1513
nrow(gd.ipeds)#match="first" #1512, removed one duplicated (across all values)

#create degree-seeking col
gd.ipeds<-gd.ipeds%>%mutate(degree.t=case_when(`Class level`!="GR"~"Non-degree",`Class level`=="GR" ~"Degree-seeking"))%>%select(-`Class level`)
#check
names(gd.ipeds)

#now we have all needed var in both ug/gd
#merge them:
##################################################################
####################large enrollment data: one dataset############
ug.gd<-plyr::join_all(list(ug.ipeds,gd.ipeds),type="full")%>%unique()
#check
sapply(list(ug.ipeds,gd.ipeds), function(x) nrow(x))%>%sum()#4447
nrow(ug.gd)#4443, removed 4 duplicated rows
ncol(ug.gd)#14

######basic cleaning
#NA rows/cols
ug.gd<-ug.gd%>%janitor::remove_empty(c("rows", "cols"))#remove all-NA rows/cols!!
  #distinct(`People Code Id`,.keep_all = TRUE)%>%#unique ppid and keep all other variables
nrow(ug.gd)#4443

#NA ppids
##look at the NA value
ug.gd[is.na(ug.gd$`People Code Id`),]#looks all can be excluded
ug.gd<-ug.gd[!is.na(ug.gd$`People Code Id`),]
nrow(ug.gd)#4422
#check
ug.gd[is.na(ug.gd$`People Code Id`),]#no na for ppid

#NA gov ids
##look at the NA value
ug.gd[is.na(ug.gd$`gov id`),]%>%select(term,Level,`People Code Id`,`gov id`)# summers terms and a few fall do not have gov id info; should not remove NA for gov ids

#FT/PT
##check all values
ug.gd%>%group_by(`FT/PT`)%>%count()#find 2 weird numeric value
#check werid rows
ug.gd%>%filter(`FT/PT`!="FT" & `FT/PT`!="PT")
#assign value

##################################################################
####################cleaning: check col by col########################
names(ug.gd)
str(ug.gd)

#change to numeric
ug.gd$`Term Credits`<-as.numeric(ug.gd$`Term Credits`)

######change to factor: ordering values for easier order match with ipeds form
#FT/PT


#ethnicity
ug.gd%>%group_by(Ethnicity)%>%count()
ug.gd$Ethnicity=factor(ug.gd$Ethnicity, levels=c("Non Resident Alien","Hispanic","American Indian or Alaska Native","Asian","Black or African American","White","Two or more Races"))#not mention unknown so that it merge with NA 

#transfer
ug.gd$`Transfer YN`=factor(ug.gd$`Transfer YN`,levels = c("Y","N"))

# degree.t
ug.gd%>%group_by(degree.t)%>%count()#origional order looks good
ug.gd$degree.t=factor(ug.gd$degree.t)

#term
ug.gd%>%group_by(term)%>%count()#origional order looks good
ug.gd$term=factor(ug.gd$term)


#####check others


#skim cols: find unreasonable NA, weird values
for (names in names(ug.gd) ) {}
tally(gd.ipeds$`People Code Id`)

sapply(names(ug.gd), function(x, df=ug.gd) df%>%group_by(x)%>%count())

sapply(names(ug.gd), function(x, df=ug.gd) is.na(df$x))

gd.ipeds%>%group_by(`FT/PT`)%>%count()#no NA
#do not have islander;; 

#credit
ug.gd%>%mutate(creditUG=case_when(Level=="UG"~`Term Credits`,Level=="GD"~"0"),
               creditGD=case_when(Level=="GD"~`Term Credits`,Level=="UG"~"0"))




####################################################################################
################## US NEWS 07/01/2021-06/30-2022, due 2022-10-15########
########################################################################################

#########search and figure out values for online programs (UG's bussadmin, completion + GD's MBA, MSM, MSCJ)
#remove not valid values (number values for degree)
ssn%>%filter(!grepl("^[0-9]",ssn$Degree))%>%group_by(Degree)%>%count()
#search for business administration
ssn%>%filter(grepl("[Bb][Uu]",ssn$Degree))%>%group_by(Degree)%>%count()#see "Business Administration"
#search for completion programs
ssn%>%filter(grepl("[Cc]omp",ssn$Degree))%>%group_by(Degree)%>%count()#see completion programs

##########filter using searched results
ssn<-ssn%>%filter(Degree %in% c("MSCJ","MSM","MBA","Business Administration","Communication Bachelors Completion","Interdisciplinary Bachelors Completion","Psychology Bachelors Completion"))%>%
  select(`gov id`)%>%#keep ssn only
  unique()%>%na.omit()#remove NA or duplicated id
#save list of students and send to  financial aid
write.xlsx(list("SSN"=ssn), file="/Volumes/lasellshare/Faculty_Staff_Shares$/IR/Fin Aid Sharing/2022 US News/StudentLoanInfo_USNewsReport.xlsx")


#bachelor online program
df%>%filter(Curriculum %in% c("Business Administration","Communication Bachelors Completion","Interdisciplinary Bachelors Completion","Psychology Bachelors Completion"))%>%unique()
#gd merge
df%>%filter(Degree %in% c("MSCJ","MSM","MBA"))%>%unique()

##########age/birth date of UG, question 70###########
#calculate age from birth date
library(lubridate)
age <- function(dob, age.day = "2021-07-01", units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)}
#count age category
usn.ug %>% mutate(age= age(mdy(usn.ug$`Birth Date`))) %>% mutate(age.cat=case_when(
  age<=22 ~ "22 or younger",
  age>=23 & age<=29 ~ "23-29",
  age>=30 & age <=39 ~ "30-39",
  age>=40 & age <=49 ~ "40-49",
  age>=50 & age <=59 ~ "50-59",
  age>-60 ~"60 or older"
))%>%filter(!is.na(age.cat))%>%group_by(age.cat)%>%count()

##########GD age question 49 or 52############
usn.gd%>%mutate(age=age(mdy(`Birth Date`)))%>%filter(!is.na(age))%>%group_by(Degree)%>%summarise(mean.age=mean(age))

##########international students of UG, question 53,54###########
usn.ug%>%group_by(Ethnicity)%>%count()

##########non-transfer of UG, question 55###########
usn.ug%>%group_by(`Transfer YN`)%>%count()

##########cum credit progress out of 120 credits of UG, question 57###########
usn.ug%>%mutate(CreditPrt=as.numeric(`Cum Credits`)/120)%>%
  mutate(Progress=case_when(
    CreditPrt<.25~"<25%",
    CreditPrt>=.25 & CreditPrt<.50~"25-49%",
    CreditPrt>=.50 & CreditPrt<=.74~"50-49%",
    CreditPrt>.75~">75%",
  ))%>%group_by(Progress)%>%count()

##############################GD data###############################
##########GD gender question 47 or 50############
usn.gd%>%filter(Degree=="MSCJ")%>%group_by(Gender)%>%count()
usn.gd%>%filter(Degree=="MSM")%>%group_by(Gender)%>%count()
usn.gd%>%filter(Degree=="MBA")%>%group_by(Gender)%>%count()



########################################################################################
####################################################################################
################## IPEDS 12-month enrollment and Completions (due 2022 Oct 19)########
########################################################################################

#####################################################################################
#####################12-month Unduplicated Count by Race/Ethnicity and Gender########
             
#Full-time Undergraduate Students:MEN
##########DEGREE: FIRST TIME (College Attend=NEW)
##report first-time student in DEGREE-SEEKING column of the MEN table of UG
t1<-ug.ipeds%>%filter(`FT/PT`=="FT",Gender=="M",degree.t=="Degree-seeking",`College Attend`=="NEW")%>%
  group_by(Ethnicity,.drop = FALSE)%>%summarise(firsttime=n())

##########DEGREE&NON FIRST TIME: TRANSFER VS RETURNING in "Transfer YN"
## focusing on return student, whether they are transfer or not
t2<-ug.ipeds%>%filter(`FT/PT`=="FT",Gender=="M",degree.t=="Degree-seeking", `College Attend`=="RET")%>%
  group_by(`Transfer YN`,Ethnicity,.drop = FALSE)%>%count()%>%
  #transferY&N side by side
  pivot_wider(names_from = `Transfer YN`,values_from =  n, names_glue = paste0("Transfer","{`Transfer YN`}_{.value}"))

##########NON DEGREE
#report NON-DEGREE column
t3<-ug.ipeds%>%filter(`FT/PT`=="FT",Gender=="M",degree.t=="Non-degree")%>%
  group_by(Ethnicity)%>%summarize(nondegree=n())

t<-plyr::join_all(list(t1,t2,t3),type="full",match="first")
t[is.na(t)]<-0
View(t)

#Full-time Undergraduate Students:WOMEN
##########DEGREE: FIRST TIME (College Attend=NEW)
##report first-time student in DEGREE-SEEKING column of the MEN table of UG
t1<-ug.ipeds%>%filter(`FT/PT`=="FT",Gender=="F",degree.t=="Degree-seeking",`College Attend`=="NEW")%>%
  group_by(Ethnicity,.drop = FALSE)%>%summarise(firsttime=n())

##########DEGREE&NON FIRST TIME: TRANSFER VS RETURNING in "Transfer YN"
## focusing on return student, whether they are transfer or not
t2<-ug.ipeds%>%filter(`FT/PT`=="FT",Gender=="F",degree.t=="Degree-seeking", `College Attend`=="RET")%>%
  group_by(`Transfer YN`,Ethnicity,.drop = FALSE)%>%count()%>%
  #transferY&N side by side
  pivot_wider(names_from = `Transfer YN`,values_from =  n, names_glue = paste0("Transfer","{`Transfer YN`}_{.value}"))

##########NON DEGREE
#report NON-DEGREE column
t3<-ug.ipeds%>%filter(`FT/PT`=="FT",Gender=="F",degree.t=="Non-degree")%>%
  group_by(Ethnicity)%>%summarize(nondegree=n())

t<-plyr::join_all(list(t1,t2,t3),type="full",match="first")
t[is.na(t)]<-0
View(t)

#Part-time Undergraduate Students:MEN
##########DEGREE: FIRST TIME (College Attend=NEW)
##report first-time student in DEGREE-SEEKING column of the MEN table of UG
t1<-ug.ipeds%>%filter(`FT/PT`=="PT",Gender=="M",degree.t=="Degree-seeking",`College Attend`=="NEW")%>%
  group_by(Ethnicity,.drop = FALSE)%>%summarise(firsttime=n())

##########DEGREE&NON FIRST TIME: TRANSFER VS RETURNING in "Transfer YN"
## focusing on return student, whether they are transfer or not
t2<-ug.ipeds%>%filter(`FT/PT`=="PT",Gender=="M",degree.t=="Degree-seeking", `College Attend`=="RET")%>%
  group_by(`Transfer YN`,Ethnicity,.drop = FALSE)%>%count()%>%
  #transferY&N side by side
  pivot_wider(names_from = `Transfer YN`,values_from =  n, names_glue = paste0("Transfer","{`Transfer YN`}_{.value}"))

##########NON DEGREE
#report NON-DEGREE column
t3<-ug.ipeds%>%filter(`FT/PT`=="PT",Gender=="M",degree.t=="Non-degree")%>%
  group_by(Ethnicity)%>%summarize(nondegree=n())

t<-plyr::join_all(list(t1,t2,t3),type="full",match="first")#%>%as.data.frame()#have to convert to df for is.na step
t[is.na(t)]<-0
View(t)


#Part-time Undergraduate Students:WOMEN
##########DEGREE: FIRST TIME (College Attend=NEW)
##report first-time student in DEGREE-SEEKING column of the MEN table of UG
t1<-ug.ipeds%>%filter(`FT/PT`=="PT",Gender=="F",degree.t=="Degree-seeking",`College Attend`=="NEW")%>%
  group_by(Ethnicity,.drop = FALSE)%>%summarise(firsttime=n())

##########DEGREE&NON FIRST TIME: TRANSFER VS RETURNING in "Transfer YN"
## focusing on return student, whether they are transfer or not
t2<-ug.ipeds%>%filter(`FT/PT`=="PT",Gender=="F",degree.t=="Degree-seeking", `College Attend`=="RET")%>%
  group_by(`Transfer YN`,Ethnicity,.drop = FALSE)%>%count()%>%
  #transferY&N side by side
  pivot_wider(names_from = `Transfer YN`,values_from =  n, names_glue = paste0("Transfer","{`Transfer YN`}_{.value}"))

##########NON DEGREE
#report NON-DEGREE column
t3<-ug.ipeds%>%filter(`FT/PT`=="PT",Gender=="F",degree.t=="Non-degree")%>%
  group_by(Ethnicity)%>%summarize(nondegree=n())

t<-plyr::join_all(list(t1,t2,t3),type="full",match="first")#%>%as.data.frame()#have to convert to df for is.na step
t[is.na(t)]<-0
View(t)

#GD MEN
##report MEN's ethnicity by gender
t<-gd.ipeds%>%filter(Gender=="M")%>%group_by(Ethnicity,`FT/PT`,.drop = FALSE)%>%count()%>%
  pivot_wider(names_from = `FT/PT`, values_from = n)
t$FT[is.na(t$FT)]<-0
t$PT[is.na(t$PT)]<-0
View(t)

#GD WOMEN
##report WOMEN's ethnicity by gender
t<-gd.ipeds%>%filter(Gender=="F")%>%group_by(Ethnicity,`FT/PT`,.drop = FALSE)%>%count()%>%
  pivot_wider(names_from = `FT/PT`, values_from = n)
t$FT[is.na(t$FT)]<-0
t$PT[is.na(t$PT)]<-0
View(t)

#GENDER UNKNOWN
#UG unknown
ug.ipeds%>%group_by(Gender)%>%count()#31
#GD unknown
gd.ipeds%>%group_by(Gender)%>%count()#2
#Another gender blank - no value as another gender


#CREDIT


####instructional activity question: add up all gd/ug term credit

####anyone having ug credit do not count them in gd headcount for ethnicity




####distance education: student info by course file // no option student courses file -- long to wide: the section type col to multiple cols, each contain one type of section format
####
####us_rent_income %>% pivot_wider(names_from = variable, values_from = mutated_countof1_eachrow,values_fill = 0)