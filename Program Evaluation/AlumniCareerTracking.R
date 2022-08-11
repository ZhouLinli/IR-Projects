###########################################manual matching survey-dashboard data's col names r###############################
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


######################################################read data files: grad alum 5yr#########################################################
library(readxl)
#dashboard data
db<-read_excel("/Users/linlizhou/Documents/LASELL/data/alumni/grad5y_historic.xlsx")
#survey.data 
survey<-read_excel("/Users/linlizhou/Documents/LASELL/data/alumni/GD5yrSurvey_clean.xlsx")
#linkedin data
linkedin<-read_excel("/Users/linlizhou/Documents/LASELL/data/alumni/GD5yrLinkedin_clean.xlsx")

#ipeds.complete17f data
##see which sheet contain all the raw data
excel_sheets("/Users/linlizhou/Documents/LASELL/alumnicareer/data/Merged Completions.xlsx")
##read that sheet
ipeds.complete17f<-read_excel("/Users/linlizhou/Documents/LASELL/data/completion/2017ipedsFComp_2016grad.xlsx",sheet = "Merged_ALL")
##see which sheet contain all the raw data
excel_sheets("/Users/linlizhou/Documents/LASELL/data/completion/2018ipedsFComp_2017grad.xlsx.xlsx")
##read that sheet
ipeds.complete18f<-read_excel("/Users/linlizhou/Documents/LASELL/data/completion/2018ipedsFComp_2017grad.xlsx.xlsx",sheet="Merged")
#select useful cols
ipeds.complete17f<-ipeds.complete17f%>%select(`People Code ID`,Degree)%>%rename(PCID=`People Code ID`,degree=Degree)
ipeds.complete18f<-ipeds.complete18f%>%select(people_code_id,degree)%>%rename(PCID=people_code_id)
#merge 17f and 18f
ipeds.complete<-merge(ipeds.complete17f,ipeds.complete18f)
#remove used dataframe
rm(ipeds.complete17f,ipeds.complete18f)




########################################vlookup ipeds.complete17f for alum's degree#####################################################
#find the program and degree cols
library(dplyr)
ipeds.complete17f.gd<-ipeds.complete17f%>%filter(Program=="GRAD")
ipeds.complete17f.gd%>%group_by(Degree)%>%count()#found degree col
#ipeds.complete17f.gd%>%group_by(Curriculum)%>%count()#similar but not exactly the program names
#ipeds.complete17f.gd%>%group_by(`CIP Description`)%>%count()#maybe this is the program names
#later found out program names are directly recoded from degree names, instead of vlookuped

#all useful information (from ipeds.complete17f) in one dataset
ipeds.complete17f.gd<-ipeds.complete17f.gd%>%select(`People Code ID`,Degree)%>%rename(PCID=`People Code ID`)
#match PCID and merge all cols
survey_m<-left_join(survey,ipeds.complete17f.gd)
#assign appended new cols (from ipeds) to the blank corresponding cols in survey
survey_m$Degree...3<-survey_m$Degree
#check
head(survey_m$Degree...3)
#remove used cols from ipeds
survey.clean<-survey_m%>%select(-c(Degree))
#export the file
library("writexl")
write_xlsx(survey.clean,"/Users/linlizhou/Documents/LASELL/alumnicareer/data/GD5yrSurvey_clean.xlsx")

########################################vlookup ipeds.complete18f for alum's degree#####################################################
#find the program and degree cols
library(dplyr)
ipeds.complete18f<-ipeds.complete18f%>%filter(Program=="GRAD")
ipeds.complete18f%>%group_by(Degree)%>%count()#found degree col
#ipeds.complete18f%>%group_by(Curriculum)%>%count()#similar but not exactly the program names
#ipeds.complete18f%>%group_by(`CIP Description`)%>%count()#maybe this is the program names
#later found out program names are directly recoded from degree names, instead of vlookuped

#all useful information (from ipeds.complete18f) in one dataset
ipeds.complete18f<-ipeds.complete18f%>%select(`People Code ID`,Degree)%>%rename(PCID=`People Code ID`)
#match PCID and merge all cols
survey_m<-left_join(survey,ipeds.complete18f)
#assign appended new cols (from ipeds) to the blank corresponding cols in survey
survey_m$Degree...3<-survey_m$Degree
#check
head(survey_m$Degree...3)
#remove used cols from ipeds
survey.clean<-survey_m%>%select(-c(Degree))
#export the file
library("writexl")
write_xlsx(survey.clean,"/Users/linlizhou/Documents/LASELL/alumnicareer/data/GD5yrSurvey_clean.xlsx")










########################################with degree, fill program values#####################################################
db%>%group_by(Degree...3,Program)%>%count()#got the list of degree - program pairs
#recode program given degree, in survey
survey<-survey%>%mutate(program.from.degree=case_when(
  Degree...3=="CER"~"Certificate",
  Degree...3=="MEDEL"~"Degree Elementary Education MED",
  Degree...3=="MEDMD"~"Degree Moderate Disabilities",
  Degree...3=="MSC"~"Degree Communication",
  Degree...3=="MSM"~"Degree Management",
  Degree...3=="MSSM"~"Degree Sport Management",
  Degree...3=="PMBA"~"Degree Business Administration"
))
#check
survey%>%group_by(program.from.degree)%>%count()#15 NAs
survey%>%group_by(Degree...3,program.from.degree)%>%count()#their degree are NA too
survey$Program<-survey$
#recode program given degree, in linkedin



















