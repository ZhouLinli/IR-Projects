#8-10-2022  
#in excel, I manually copied dashboard col names to survey col names
#The manual process can be referred when automizing the process, see notes below:
##mostly col match and can simple copy and paste (i.e., rename survey col name using the dashboard name)
##a bunch of survey cols (timestart, status, etc.) are removed

##several cols are recoded/mutated cols in dashboard data, and thus need to be inserted in the survey 
###these newly created col in dashbaord data (and inserted in the survey data to keep it consistent for later row-binding) are:
####col1:3: pcid, program, degree
####col f:g: employed/unemployed, employment status
####col r:s: salary recode, advancementatleastone
####col as: atleastoneactivity
####col -6:-1 datasource, survey, linkedin,gradedate,gradyear,employed/ed


######Except col1 which is a rename 
######except 2:3 which need to do vloopup from datamart
######other cols can be directly mutated using the current data in other cols

########now I am filling in col2:3 using ipeds_completion report in 2017 fall for grads in 2016-2017 (same as vlookup in excel, but the datamart is too large to load I suppose)


#survey.data
survey<-readxl::read_excel("/Users/linlizhou/Documents/LASELL/alumnicareer/data/GD5yrSurvey_clean.xlsx")


#ipeds.complete data
library(readxl)
#see which sheet contain all the raw data
excel_sheets("/Users/linlizhou/Documents/LASELL/alumnicareer/data/Merged Completions.xlsx")
#read that sheet
ipeds.complete<-read_excel("/Users/linlizhou/Documents/LASELL/alumnicareer/data/Merged Completions.xlsx",sheet = "Merged_ALL")


#find the program and degree cols
library(dplyr)
ipeds.complete.gd<-ipeds.complete%>%filter(Program=="GRAD")
ipeds.complete.gd%>%group_by(Degree)%>%count()#found degree col
ipeds.complete.gd%>%group_by(Curriculum)%>%count()#similar but not exactly the program names
ipeds.complete.gd%>%group_by(`CIP Description`)%>%count()#maybe this is the program names

#all useful information (from ipeds.complete) in one dataset
ipeds.complete.gd<-ipeds.complete.gd%>%select(`People Code ID`,`CIP Description`,Degree)%>%rename(PCID=`People Code ID`)

#match PCID and merge all cols
survey_m<-left_join(survey,ipeds.complete.gd)

#assign appended new cols (from ipeds) to the blank corresponding cols in survey
survey_m$Program<-survey_m$`CIP Description`
survey_m$Degree...3<-survey_m$Degree
#check
head(survey_m$Degree...3)
head(survey_m$Program)

#remove used cols from ipeds
survey.clean<-survey_m%>%select(-c(`CIP Description`,Degree))

#export the file
library("writexl")
write_xlsx(survey.clean,"/Users/linlizhou/Documents/LASELL/alumnicareer/data/GD5yrSurvey_clean.xlsx")

