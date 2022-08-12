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
#rename prep
names(db)#saved to an excel for quick index reference
names(survey)#saved to an excel for quick index reference
#view(data) side by side (eyeballing value) for var correspondence

#rename
names(survey)[3]<-names(db)[1]
names(survey)[4]<-names(db)[85]
names(survey)[5]<-names(db)[6]
names(survey)[6]<-names(db)[7]
names(survey)[16]<-names(db)[45]
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

#check what are not used in survey data
names(survey)[names(survey)%in%names(db)=="FALSE"]

#19 names not used - no match in db data - that's the best I can do, those 19 cols will remain in appended data as new cols


#####vlookup program and degree
survey%>%group_by(Grad_date)%>%count()#it is 20-21 graduates
#try 2021 fall ipeds.complete
ipeds.complete21f<-read_excel("/Users/linlizhou/Documents/LASELL/data/completion/2021IPEDScompletions.xlsx",sheet = "Degrees - Combined")
# - if find all pcid in survey for degree
survey$PC_ID %in% ipeds.complete21f$textbox41#partially true
#investigate those not true
survey[survey$PC_ID %in% ipeds.complete21f$textbox41=="FALSE",c(3,4)]#maybe they're from 2020 graduate data, then needs to find ipeds.complete20f

#########col f:g: employed/unemployed, employment status

####col r:s: salary recode, advancementatleastone
####col as: atleastoneactivity
####col -6:-1 datasource, survey, linkedin,gradedate,gradyear,employed/ed



##########mutated cols using current cols#####
