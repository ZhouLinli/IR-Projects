#############################load package#############################
library(readxl)
library(dplyr)
library(writexl)

#############################read data files##########################
#dashbaord
db<-read_excel("/Users/linlizhou/Documents/LASELL/data/alumni/Grad6m_historic.xlsx")
#survey
survey<-read_excel("/Users/linlizhou/Documents/LASELL/data/alumni/Grad 6-month survey Results 2021.xlsx")
ipeds.complete<-read_excel("/Users/linlizhou/Documents/LASELL/data/completion/17f18f.xlsx")

######remove auto-generated survey cols (timestart, status, etc.)#######
#find out the index
names(survey)
survey<-survey%>%select(c(`First Name`,`Last Name`,`Invite Custom Field 1`,53:94))



#####rename PCID
survey<-survey%>%rename(PCID=`Invite Custom Field 1`)
#####vlookup program and degree
survey<-left_join(survey,ipeds.complete)
#########col f:g: employed/unemployed, employment status
####col r:s: salary recode, advancementatleastone
####col as: atleastoneactivity
####col -6:-1 datasource, survey, linkedin,gradedate,gradyear,employed/ed



##########mutated cols using current cols#####
