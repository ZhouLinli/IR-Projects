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

###############view(data) check value for var correspondence#############
#rename PCID
survey<-survey%>%rename(PCID=`Invite Custom Field 1`)
#rename a bunch more
names(db)#saved to an excel for quick index reference
names(survey)#saved to an excel for quick index reference
#employment match with what is your current employment status
names(survey)[7]<-names(db)[4]
names(survey)[8]<-names(db)[5]
names(survey)[10]<-names(db)[20]
names(survey)[9]<-names(db)[22]
names(survey)[11]<-names(db)[26]
names(survey)[15]<-names(db)[30]
names(survey)[17]<-names(db)[35]
names(survey)[19]<-names(db)[36]
names(survey)[20]<-names(db)[38]
names(survey)[22]<-names(db)[42]
names(survey)[34:37]<-names(db)[73:76]
names(survey)[39:45]<-names(db)[77:83]





#####vlookup program and degree

#########col f:g: employed/unemployed, employment status

####col r:s: salary recode, advancementatleastone
####col as: atleastoneactivity
####col -6:-1 datasource, survey, linkedin,gradedate,gradyear,employed/ed



##########mutated cols using current cols#####
