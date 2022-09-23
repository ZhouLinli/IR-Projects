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
ug1<-ug21fa%>%select(`People Code Id`,`gov id`,`FT/PT`,Degree,`Transfer YN`,`College Attend`,Ethnicity,Gender,`Term Credits`,`Cum Credits`,Curriculum,`Birth Date`)%>%
  mutate(Level="UG",term="21fall")#mutate identification col

ug2<-ug21sum2%>%select(`People Code Id`,`gov id`,`FT/PT`,Degree,`Transfer YN`,`College Attend`,Ethnicity,Gender,`Term Credits`,`Cum Credits`,Curriculum,`Birth Date`)%>%
  mutate(Level="UG",term="21summer2")

ug3<-ug22sp%>%select(`People Code Id`,`gov id`,`FT/PT`,Degree,`Transfer YN`,`College Attend`,Ethnicity,Gender,`Term Credits`,`Cum Credits`,Curriculum,`Birth Date`)%>%
  mutate(Level="UG",term="22spring")

ug4<-ug22sum.main.1%>%select(`People Code Id`,`gov id`,`FT/PT`,Degree,`Transfer YN`,`College Attend`,Ethnicity,Gender,`Term Credits`,`Cum Credits`,Curriculum,`Birth Date`)%>%
  mutate(Level="UG",term="22summer.1main")

ug5<-ug22wi%>%select(`People Code Id`,`gov id`,`FT/PT`,Degree,`Transfer YN`,`College Attend`,Ethnicity,Gender,`Term Credits`,`Cum Credits`,Curriculum,`Birth Date`)%>%
  mutate(Level="UG",term="22winter")

#check selected vars
sapply(list(ug1,ug2,ug3,ug4,ug5), function(x) ncol(x))#same 14

##########################merge to one ug dataset
ug.ipeds<-plyr::join_all(list(ug1,ug2,ug3,ug4,ug5),type="full")%>%
  unique()%>%#remove rows that has exact same value across all columns
  rename(Program=Curriculum,`New Ret Term YN`=`College Attend`)%>%#rename for ug-gd consistency
  mutate(degree.t=case_when(Degree %in% c("NON","Non Matriculated")~"Non-degree",Degree!="NON" & Degree!="Non Matriculated" ~"Degree-seeking"))%>%select(-Degree)#create degree.t (and remove old)

#check
sapply(list(ug1,ug2,ug3,ug4,ug5), function(x) nrow(x))%>%sum()#2935
nrow(ug.ipeds)#match="first" #2935
ncol(ug.ipeds)#14
names(ug.ipeds)

##################################################################
####################gd data############
gd1<-g21fa%>%select(`People Code Id`,`gov id`,`Class level`,Gender,`FT/PT`,Ethnicity,`Term Credits`,`Cum Credits`,Degree,`Birth Date`,`New Ret Term YN`,`Transfer YN`)%>%
  mutate(Level="GD",term="21fall")#mutate identification col

gd2<-gd21sum2%>%select(`People Code Id`,`gov id`,`Class level`,Gender,`FT/PT`,Ethnicity,`Term Credits`,`Cum Credits`,Degree,`Birth Date`,`New Ret Term YN`,`Transfer YN`)%>%
  mutate(Level="GD",term="21summer2")

gd3<-gd22sp%>%select(`People Code Id`,`gov id`,`Class level`,Gender,`FT/PT`,Ethnicity,`Term Credits`,`Cum Credits`,Degree,`Birth Date`,`New Ret Term YN`,`Transfer YN`)%>%
  mutate(Level="GD",term="22spring")

gd4<-gd22sum.main.1%>%select(`People Code Id`,`gov id`,`Class level`,Gender,`FT/PT`,Ethnicity,`Term Credits`,`Cum Credits`,Degree,`Birth Date`,`New Ret Term YN`,`Transfer YN`)%>%
  mutate(Level="GD",term="22summer.1main")

gd5<-gd22wi%>%select(`People Code Id`,`gov id`,`Class level`,Gender,`FT/PT`,Ethnicity,`Term Credits`,`Cum Credits`,Degree,`Birth Date`,`New Ret Term YN`,`Transfer YN`)%>%
  mutate(Level="GD",term="22winter")

#check selected vars: compare to ug, do not have transfer, college attend, and cum credit
sapply(list(gd1,gd2,gd3,gd4,gd5), function(x) ncol(x))#same 14

#merge to one
gd.ipeds<-plyr::join_all(list(gd1,gd2,gd3,gd4,gd5),type="full")%>%
  unique()%>%#remove rows that has exact same value across all columns
  rename(Program=Degree)%>%#renmae for ug-gd consistency
  mutate(degree.t=case_when(`Class level`!="GR"~"Non-degree",`Class level`=="GR" ~"Degree-seeking"))%>%select(-`Class level`)#create degree.t (and remove old)
  
#check
sapply(list(gd1,gd2,gd3,gd4,gd5), function(x) nrow(x))%>%sum()#1513
nrow(gd.ipeds)#match="first" #1512, removed 1 duplicated (across all values)
ncol(gd.ipeds)
names(gd.ipeds)%in%names(ug.ipeds)%>%sum()#14 all same

##################################################################
####################merge ug.ipeds and gd ipeds###################
ug.gd<-plyr::join_all(list(ug.ipeds,gd.ipeds),type="full")%>%unique()
#check
sapply(list(ug.ipeds,gd.ipeds), function(x) nrow(x))%>%sum()#4447
nrow(ug.gd)#4443, removed 4 duplicated rows
ncol(ug.gd)#14
names(ug.gd)
str(ug.gd)


##############################################################################################
####################cleaning col by col: NA, recode values, num/level class###################
#NA rows/cols
ug.gd<-ug.gd%>%janitor::remove_empty(c("rows", "cols"))#remove all-NA rows/cols!!
  #distinct(`People Code Id`,.keep_all = TRUE)%>%#unique ppid and keep all other variables
nrow(ug.gd)#4443

########################ppids
##look at the NA value
ug.gd[is.na(ug.gd$`People Code Id`),]#looks all can be excluded
ug.gd<-ug.gd[!is.na(ug.gd$`People Code Id`),]
nrow(ug.gd)#4422
#check
ug.gd[is.na(ug.gd$`People Code Id`),]#no na for ppid

#########################gov ids
##look at the NA value
ug.gd[is.na(ug.gd$`gov id`),]%>%select(term,Level,`People Code Id`,`gov id`)# summers terms and a few fall do not have gov id info; should NOT remove NA for gov ids

#########################FT/PT
##check all values
ug.gd%>%group_by(`FT/PT`)%>%count()#find 2 weird numeric value
#check weird rows
rows<-ug.gd%>%filter(`FT/PT`!="FT" & `FT/PT`!="PT")
rows#view weird rows

##find missing values from other data source
gdinfo22sp<-read.csv("/Volumes/lasellshare/Faculty_Staff_Shares$/IR/Registrar Reports/2022 Spring/Grad/Graduate Student Information.csv")
#find rows matched-ppid
replaces<-gdinfo22sp%>%filter(people_code_id%in% rows$`People Code Id`)#using ppid to find values in gdinfo22sp

##assign each missing value (different col names) one by one, for matched ppids
#ensure same ppid (with same order)
rows$`People Code Id`==replaces$people_code_id
#assign missing FT/PT
rows$`FT/PT`<-replaces$FTE
#assign missing Ethnicity
rows$Ethnicity<-replaces$Ethnicity
#assign missing cum credit
rows$`Cum Credits`<-replaces$cum_credits

#assign rows back to ug.gd
ug.gd[ug.gd$`FT/PT`!="FT" & ug.gd$`FT/PT`!="PT",]<-rows
#check
ug.gd%>%group_by(`FT/PT`)%>%count()#find two 0 from FTE
#make FTE right
ug.gd<-ug.gd%>%mutate(`FT/PT`=case_when(`FT/PT`=="FT"~"FT",`FT/PT`=="PT"~"PT",`FT/PT`=="0"~"PT"))
#check
ug.gd%>%group_by(`FT/PT`)%>%count()#find 2 weird numeric value
str(ug.gd)

#factorize
ug.gd$`FT/PT`<-factor(ug.gd$`FT/PT`)
str(ug.gd)#check

#############TransferYN
#explore
ug.gd%>%group_by(`Transfer YN`)%>%count()#no NA
#factorize
ug.gd$`Transfer YN`=factor(ug.gd$`Transfer YN`,labels =c("Transfer","Non-transfer"),levels = c("Y","N"))
#check
str(ug.gd)

#############New Ret Term YN
#explore
ug.gd%>%group_by(`New Ret Term YN`)%>%count()#no NA, just need to factorize
#align values
ug.gd<-ug.gd%>%mutate(`New Ret Term YN`=case_when(`New Ret Term YN`=="N"~"new",#"no" college attend="new"
                                                  `New Ret Term YN`=="NEW"~"new",
                                                  `New Ret Term YN`=="Y"~"return",#"yes" college attend="return"
                                                  `New Ret Term YN`=="RET"~"return",
                                                  `New Ret Term YN`=="RETS"~"return"))
#check
ug.gd%>%group_by(`New Ret Term YN`)%>%count()
#factorize
ug.gd$`New Ret Term YN`=factor(ug.gd$`New Ret Term YN`,levels = c("new","return"))
#check
str(ug.gd)

################ethnicity
#explore
ug.gd%>%group_by(Ethnicity)%>%count()#9 with two refer to the same, no NA
#fix one value
ug.gd<-ug.gd%>%mutate(Ethnicity=if_else(Ethnicity=="Non Resident Alien/International",
                                        "Non Resident Alien",Ethnicity))
#check
ug.gd%>%group_by(Ethnicity)%>%count()#8 unique values now
#factorize 8 values
ug.gd$Ethnicity=factor(ug.gd$Ethnicity, levels=c("Non Resident Alien","Hispanic","American Indian or Alaska Native","Asian","Black or African American","White","Two or more Races","Race and Ethnicity Unknown"))
#check
str(ug.gd)

############gender
#explore
ug.gd%>%group_by(Gender)%>%count()#no NA
#factorize
ug.gd$Gender<-factor(ug.gd$Gender,levels = c("M","F","U"))
#check
ug.gd%>%group_by(Gender)%>%count()
str(ug.gd)


#################term credits
#change to numeric
ug.gd$`Term Credits`<-as.numeric(ug.gd$`Term Credits`)
#check factor class
str(ug.gd)
#NA
ug.gd[is.na(ug.gd$`Term Credits`),]#non NA
#explore--default rm.na=T
summary(ug.gd$`Term Credits`)#all students enrolled for credits


#################cum credits:FOR MULTIPLE ENTRY, TAKE THE LATTER
#change to numeric
ug.gd$`Cum Credits`<-as.numeric(ug.gd$`Cum Credits`)
#ug.gd%>%mutate(`Cum Credits`=max(which (`Cum Credits`)),CreditPrt=`Cum Credits`/120,CreditPrt=case_when(CreditPrt<.25~"<25%",CreditPrt>=.25 & CreditPrt<.50~"25-49%",CreditPrt>=.50 & CreditPrt<=.74~"50-74%",CreditPrt>.75~">75%"))%>%group_by(CreditPrt)%>%count()
#check 
str(ug.gd)
#explore 
summary(ug.gd$`Cum Credits`)
#NA
ug.gd[is.na(ug.gd$`Cum Credits`),]#a lot of NAs, all comes from UG 21 fall!
#should NOT exclude those rows -- other cols valid
#remember to rm.na=T when calculate using cum credits

#########Program
ls<-ug.gd%>%group_by(Program)%>%count()
View(ls)#no NA

#########Birth Date
library(lubridate)
#change to date format
ug.gd$`Birth Date`<-mdy(ug.gd$`Birth Date`)
str(ug.gd)#check
#check NA
ug.gd[is.na(ug.gd$`Birth Date`),]#find many, come from different file

###calculate age function
age.calc <- function(dob, age.day = "2021-07-01", units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age))) #floor=round down to the nearest interger
  return(calc.age)}#return has to be its own row

#create age based on birth date
ug.gd<-ug.gd %>% mutate(age= age.calc(ug.gd$`Birth Date`)) %>% #mutate age
  mutate(age.cat=case_when(age<=22 ~ "22 or younger",age>=23 & age<=29 ~ "23-29",age>=30 & age <=39 ~ "30-39",
  age>=40 & age <=49 ~ "40-49",age>=50 & age <=59 ~ "50-59",age>-60 ~"60 or older"))#change way to value age
#check age
summary(ug.gd$age)
#check age.cat
ug.gd%>%group_by(age.cat)%>%count()#29 NAs from 29 NA birth date; origional order looks good
#factorize
ug.gd$age.cat<-factor(ug.gd$age.cat)
str(ug.gd)#check

#########Level
#explore
ug.gd%>%group_by(Level)%>%count()#no NA
#factorize
ug.gd$Level<-factor(ug.gd$Level,levels = c("UG","GD"))
#check
ug.gd%>%group_by(Level)%>%count()
str(ug.gd)

#########term
#explore
ug.gd%>%group_by(term)%>%count()#no NA
#factorize
ug.gd$term=factor(ug.gd$term,levels = c("21summer2","21fall","22winter","22spring","22summer.1main"))
#check
ug.gd%>%group_by(term)%>%count()
str(ug.gd)

#########degree.t
#explore
ug.gd%>%group_by(degree.t)%>%count()#no NA; origional order looks good
#factorize
ug.gd$degree.t=factor(ug.gd$degree.t)
#check
ug.gd%>%group_by(degree.t)%>%count()
str(ug.gd)

#all cols are either factor (with correct values and orders), numeric, or stay as character (id without NA)

##############################################################################################
####################cleaning based on instruction###################
#remove not needed cols
ug.gd.did<-ug.gd%>%select(-CreditPrt,-`Cum Credits`,-Program)

###investigate duplicated ppid
dup.id<-ug.gd[duplicated(ug.gd$`People Code Id`),]
#show all duplicated ppid - eyeballing where the conflict occurs
ug.gd[ug.gd$`People Code Id` %in% dup.id$`People Code Id`,]#######it's b/c cum credits
#keep one for duplicated ids


############FT/PT status decided by entry to the institution for the first full term
str(ug.gd)

ug.gd.did<-ug.gd%>%mutate(`FT/PT`=case_when(
  `New Ret Term YN`=="new"~`FT/PT`,
  `New Ret Term YN`=="return"~"find new term"))

##################################################################
####################unduplicated###########################

#credit
ug.gd%>%mutate(creditUG=case_when(Level=="UG"~`Term Credits`,Level=="GD"~"0"),
               creditGD=case_when(Level=="GD"~`Term Credits`,Level=="UG"~"0"))





















####################################################################################
################## US NEWS 07/01/2021-06/30-2022, due 2022-10-15########
########################################################################################

##################################################################
####################create USNEWS dataset (UG's bussadmin, completion + GD's MBA, MSM, MSCJ)###########################
#search for business administration
ug.gd%>%filter(grepl("[Bb][Uu]",ug.gd$Program))%>%group_by(Program)%>%count()#see "Business Administration"
#search for completion programs
ug.gd%>%filter(grepl("[Cc]omp",ug.gd$Program))%>%group_by(Program)%>%count()#see completion programs

##############################filter using searched results and crete usn.online
usn.online<-ug.gd%>%filter(Program %in% c("MSCJ","MSM","MBA","Business Administration","Communication Bachelors Completion","Interdisciplinary Bachelors Completion","Psychology Bachelors Completion"))%>%
  select(`People Code Id`,`gov id`,Level,Program,age,age.cat,Ethnicity,`Transfer YN`,`Cum Credits`,Gender,term,`New Ret Term YN`)%>%
  unique()#remove duplicated rows

#explore
str(usn.online)
nrow(usn.online)#418
#distinct ppid
usn.online%>%distinct(`People Code Id`,.keep_all = TRUE)%>%nrow()#unique 316 ppid (and keep all other variables)

###investigate duplicated ppid
dup.id<-usn.online[duplicated(usn.online$`People Code Id`),]
#show all duplicated ppid - eyeballing where the conflict occurs
  usn.online[usn.online$`People Code Id` %in% dup.id$`People Code Id`,]#######it's b/c cum credits
#keep one for duplicated ids
usn.online<-usn.online%>%#aim: assign one value for duplicated id (to resolve conflict)    
  arrange(desc(`Cum Credits`))%>%arrange(`People Code Id`)%>%#largest cumcredit at the top, same id large-low
  distinct(`People Code Id`,.keep_all = TRUE)#distinct will keep the first row (the correct cum credit)
#explore
str(usn.online)
nrow(usn.online)#316
usn.online[is.na(usn.online$`Cum Credits`),]# from across terms and across new/tern


###calculate progress based on cum credits
usn.online<-usn.online%>%mutate(CreditPrt=`Cum Credits`/120)%>%#total is 120 credits for most programs NEEDS TO CONFIRM WITH ERIC
  mutate(CreditPrt=case_when(CreditPrt<.25~"<25%",CreditPrt>=.25 & CreditPrt<.50~"25-49%",CreditPrt>=.50 & CreditPrt<=.74~"50-74%",CreditPrt>.75~">75%"))
#check
usn.online%>%group_by(CreditPrt)%>%count()#reorder needed
#factorize
usn.online$CreditPrt<-factor(usn.online$CreditPrt,levels = c("<25%","25-49%","50-74%",">75%"))
#check
usn.online%>%group_by(CreditPrt)%>%count()#78 NAs from NA cum credits
str(usn.online)

usn.online[is.na(usn.online$`gov id`),]#one NA
#save list of students and send to  financial aid
write.xlsx(list("PPID"=usn.online), file="/Volumes/lasellshare/Faculty_Staff_Shares$/IR/Fin Aid Sharing/2022 US News/StudentLoanInfo_USNewsReport.xlsx")


##########age/birth date of UG, question 70###########
#calculate age from birth date
usn.online%>%filter(Level=="UG")%>%group_by(age.cat)%>%count()

##########GD age question 49 or 52############
usn.online%>%filter(Level=="UG")%>%group_by(Program)%>%summarise(mean.age=mean(age))#auto remove NA

##########international students of UG, question 53,54###########
usn.online%>%filter(Level=="UG")%>%group_by(Ethnicity)%>%count()

##########non-transfer of UG, question 55###########
usn.online%>%filter(Level=="UG")e%>%group_by(`Transfer YN`)%>%count()

##########cum credit progress out of 120 credits of UG, question 57###########
usn.online%>%filter(Level=="UG")%>%group_by(CreditPrt)%>%count()

##########GD gender question 47 or 50############
usn.online%>%filter(Program=="MSCJ")%>%group_by(Gender)%>%count()
usn.online%>%filter(Program=="MSM")%>%group_by(Gender)%>%count()
usn.online%>%filter(Program=="MBA")%>%group_by(Gender)%>%count()


########################################################################################
####################################################################################
################## IPEDS 12-month enrollment and Completions (due 2022 Oct 19)########
########################################################################################

#####################################################################################
#####################12-month Unduplicated Count by Race/Ethnicity and Gender########
             
#Full-time Undergraduate Students:MEN
##########DEGREE: FIRST TIME (College Attend=NEW)
##report first-time student in DEGREE-SEEKING column of the MEN table of UG
t1<-ug.gd%>%filter(Level=="UG")%>%filter(`FT/PT`=="FT",Gender=="M",degree.t=="Degree-seeking",`College Attend`=="NEW")%>%
  group_by(Ethnicity,.drop = FALSE)%>%summarise(firsttime=n())

##########DEGREE&NON FIRST TIME: TRANSFER VS RETURNING in "Transfer YN"
## focusing on return student, whether they are transfer or not
t2<-ug.gd%>%filter(Level=="UG")%>%filter(`FT/PT`=="FT",Gender=="M",degree.t=="Degree-seeking", `College Attend`=="RET")%>%
  group_by(`Transfer YN`,Ethnicity,.drop = FALSE)%>%count()%>%
  #transferY&N side by side
  pivot_wider(names_from = `Transfer YN`,values_from =  n, names_glue = paste0("Transfer","{`Transfer YN`}_{.value}"))

##########NON DEGREE
#report NON-DEGREE column
t3<-ug.gd%>%filter(Level=="UG")%>%filter(`FT/PT`=="FT",Gender=="M",degree.t=="Non-degree")%>%
  group_by(Ethnicity)%>%summarize(nondegree=n())

t<-plyr::join_all(list(t1,t2,t3),type="full",match="first")
t[is.na(t)]<-0
View(t)

#Full-time Undergraduate Students:WOMEN
##########DEGREE: FIRST TIME (College Attend=NEW)
##report first-time student in DEGREE-SEEKING column of the MEN table of UG
t1<-ug.gd%>%filter(Level=="UG")%>%filter(`FT/PT`=="FT",Gender=="F",degree.t=="Degree-seeking",`College Attend`=="NEW")%>%
  group_by(Ethnicity,.drop = FALSE)%>%summarise(firsttime=n())

##########DEGREE&NON FIRST TIME: TRANSFER VS RETURNING in "Transfer YN"
## focusing on return student, whether they are transfer or not
t2<-ug.gd%>%filter(Level=="UG")%>%filter(`FT/PT`=="FT",Gender=="F",degree.t=="Degree-seeking", `College Attend`=="RET")%>%
  group_by(`Transfer YN`,Ethnicity,.drop = FALSE)%>%count()%>%
  #transferY&N side by side
  pivot_wider(names_from = `Transfer YN`,values_from =  n, names_glue = paste0("Transfer","{`Transfer YN`}_{.value}"))

##########NON DEGREE
#report NON-DEGREE column
t3<-ug.gd%>%filter(Level=="UG")%>%filter(`FT/PT`=="FT",Gender=="F",degree.t=="Non-degree")%>%
  group_by(Ethnicity)%>%summarize(nondegree=n())

t<-plyr::join_all(list(t1,t2,t3),type="full",match="first")
t[is.na(t)]<-0
View(t)

#Part-time Undergraduate Students:MEN
##########DEGREE: FIRST TIME (College Attend=NEW)
##report first-time student in DEGREE-SEEKING column of the MEN table of UG
t1<-ug.gd%>%filter(Level=="UG")%>%filter(`FT/PT`=="PT",Gender=="M",degree.t=="Degree-seeking",`College Attend`=="NEW")%>%
  group_by(Ethnicity,.drop = FALSE)%>%summarise(firsttime=n())

##########DEGREE&NON FIRST TIME: TRANSFER VS RETURNING in "Transfer YN"
## focusing on return student, whether they are transfer or not
t2<-ug.gd%>%filter(Level=="UG")%>%filter(`FT/PT`=="PT",Gender=="M",degree.t=="Degree-seeking", `College Attend`=="RET")%>%
  group_by(`Transfer YN`,Ethnicity,.drop = FALSE)%>%count()%>%
  #transferY&N side by side
  pivot_wider(names_from = `Transfer YN`,values_from =  n, names_glue = paste0("Transfer","{`Transfer YN`}_{.value}"))

##########NON DEGREE
#report NON-DEGREE column
t3<-ug.gd%>%filter(Level=="UG")%>%filter(`FT/PT`=="PT",Gender=="M",degree.t=="Non-degree")%>%
  group_by(Ethnicity)%>%summarize(nondegree=n())

t<-plyr::join_all(list(t1,t2,t3),type="full",match="first")#%>%as.data.frame()#have to convert to df for is.na step
t[is.na(t)]<-0
View(t)


#Part-time Undergraduate Students:WOMEN
##########DEGREE: FIRST TIME (College Attend=NEW)
##report first-time student in DEGREE-SEEKING column of the MEN table of UG
t1<-ug.gd%>%filter(Level=="UG")%>%filter(`FT/PT`=="PT",Gender=="F",degree.t=="Degree-seeking",`College Attend`=="NEW")%>%
  group_by(Ethnicity,.drop = FALSE)%>%summarise(firsttime=n())

##########DEGREE&NON FIRST TIME: TRANSFER VS RETURNING in "Transfer YN"
## focusing on return student, whether they are transfer or not
t2<-ug.gd%>%filter(Level=="UG")%>%filter(`FT/PT`=="PT",Gender=="F",degree.t=="Degree-seeking", `College Attend`=="RET")%>%
  group_by(`Transfer YN`,Ethnicity,.drop = FALSE)%>%count()%>%
  #transferY&N side by side
  pivot_wider(names_from = `Transfer YN`,values_from =  n, names_glue = paste0("Transfer","{`Transfer YN`}_{.value}"))

##########NON DEGREE
#report NON-DEGREE column
t3<-ug.gd%>%filter(Level=="UG")%>%filter(`FT/PT`=="PT",Gender=="F",degree.t=="Non-degree")%>%
  group_by(Ethnicity)%>%summarize(nondegree=n())

t<-plyr::join_all(list(t1,t2,t3),type="full",match="first")#%>%as.data.frame()#have to convert to df for is.na step
t[is.na(t)]<-0
View(t)

#GD MEN
##report MEN's ethnicity by gender
t<-ug.gd%>%filter(Level=="GD")%>%filter(Gender=="M")%>%group_by(Ethnicity,`FT/PT`,.drop = FALSE)%>%count()%>%
  pivot_wider(names_from = `FT/PT`, values_from = n)
t$FT[is.na(t$FT)]<-0
t$PT[is.na(t$PT)]<-0
View(t)

#GD WOMEN
##report WOMEN's ethnicity by gender
t<-ug.gd%>%filter(Level=="GD")%>%filter(Gender=="F")%>%group_by(Ethnicity,`FT/PT`,.drop = FALSE)%>%count()%>%
  pivot_wider(names_from = `FT/PT`, values_from = n)
t$FT[is.na(t$FT)]<-0
t$PT[is.na(t$PT)]<-0
View(t)

#GENDER UNKNOWN
#UG unknown
ug.gd%>%filter(Level=="UG")%>%group_by(Gender)%>%count()#31
#GD unknown
ug.gd%>%filter(Level=="GD")%>%group_by(Gender)%>%count()#2
#Another gender blank - no value as another gender


#CREDIT


####instructional activity question: add up all gd/ug term credit

####anyone having ug credit do not count them in gd headcount for ethnicity




####distance education: student info by course file // no option student courses file -- long to wide: the section type col to multiple cols, each contain one type of section format
####
####us_rent_income %>% pivot_wider(names_from = variable, values_from = mutated_countof1_eachrow,values_fill = 0)