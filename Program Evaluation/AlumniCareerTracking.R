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


######################################################load r packages#########################################################
library(readxl)
library(dplyr)

######################################################read data files: grad alum 5yr#########################################################
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
excel_sheets("/Users/linlizhou/Documents/LASELL/data/completion/2018ipedsFComp_2017grad.clean.xlsx")
##read that sheet
ipeds.complete18f<-read_excel("/Users/linlizhou/Documents/LASELL/data/completion/2018ipedsFComp_2017grad.clean.xlsx")
#select useful cols
ipeds.complete17f<-ipeds.complete17f%>%select(`People Code ID`,Degree)%>%rename(PCID=`People Code ID`,degree=Degree)
ipeds.complete18f<-ipeds.complete18f%>%select(people_code_id,degree)%>%rename(PCID=people_code_id)
#merge 17f and 18f
ipeds.complete<-full_join(ipeds.complete17f,ipeds.complete18f)
#remove used dataframe
rm(ipeds.complete17f,ipeds.complete18f)


########################################vlookup ipeds.complete for alum's degree#####################################################
#match PCID and merge all cols
survey<-left_join(survey,ipeds.complete)#got some duplicated
survey<-survey[!duplicated(survey),]#removed duplicated
#assign appended new cols (from ipeds) to the blank corresponding cols in survey
survey$Degree...3<-survey$degree
#check
survey%>%group_by(Degree...3)%>%count()#find all degrees
#remove used cols from ipeds
survey<-survey%>%select(-c(degree))
#export the file
library("writexl")
write_xlsx(survey,"/Users/linlizhou/Documents/LASELL/data/alumni/GD5yrSurvey_clean.xlsx")


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
  Degree...3=="PMBA"~"Degree Business Administration",
  Degree...3=="MSCJ"~"Degree Criminal Justice"
))
#check
survey%>%group_by(program.from.degree)%>%count()#should find no NAs

#do the same recode program process in linkedin
linkedin<-linkedin%>%mutate(program.from.degree=case_when(
  Degree...3=="CER"~"Certificate",
  Degree...3=="MEDEL"~"Degree Elementary Education MED",
  Degree...3=="MEDMD"~"Degree Moderate Disabilities",
  Degree...3=="MSC"~"Degree Communication",
  Degree...3=="MSM"~"Degree Management",
  Degree...3=="MSSM"~"Degree Sport Management",
  Degree...3=="PMBA"~"Degree Business Administration",
  Degree...3=="MSCJ"~"Degree Criminal Justice"
))
#check
linkedin%>%group_by(Degree...3,program.from.degree)%>%count()#should find no NAs




#complete all col value fill and col name matches, it's ready to do a rbind for survey and linkedin into dashboard historical data.




ipeds.complete18f%>%filter(curriculum=="HSGJ")

mscj











