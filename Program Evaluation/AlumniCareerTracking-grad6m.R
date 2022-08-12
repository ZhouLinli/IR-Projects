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
#check which sheet to read
excel_sheets("/Users/linlizhou/Documents/LASELL/data/completion/2021IPEDScompletions.xlsx")
#read data
ipeds.complete21f<-read_excel("/Users/linlizhou/Documents/LASELL/data/completion/2021IPEDScompletions.xlsx",sheet = "Degrees - Combined")
#check degree codes
ipeds.complete21f%>%group_by(DEGREE)%>%count()#all degree codes seem fine
# - if find all pcid in survey for degree
survey$PC_ID %in% ipeds.complete21f$textbox41#partially true
#investigate those not true
survey[survey$PC_ID %in% ipeds.complete21f$textbox41=="FALSE",c(3,4)]#maybe they're from 2020 graduate data, then needs to find ipeds.complete20f

#try 2020 fall ipeds.complete
#find the right sheet to read
excel_sheets("/Users/linlizhou/Documents/LASELL/data/completion/2020Merged Completions.xlsx")
#read data
ipeds.complete20f<-read_excel("/Users/linlizhou/Documents/LASELL/data/completion/2020Merged Completions.xlsx",sheet = "Merged")
#check degree codes
ipeds.complete20f%>%group_by(Degree)%>%count()#all degree codes seem fine
# if find pcid in survey for degree
survey$PC_ID %in% ipeds.complete20f$`People Code ID`#only two true
#then there might be students who did not receive degree (not in the complete dataset, but is in the survey)

#anyway,merge ipeds.complete20/21f
#select useful cols
ipeds.complete20f<-ipeds.complete20f%>%select(`People Code ID`,Degree)
ipeds.complete21f<-ipeds.complete21f%>%select(textbox41,DEGREE)
#want to merge col to col exactly append one to the other
#then need to have exact same names:
names(ipeds.complete21f)<-names(ipeds.complete20f)
#merge
ipeds.complete<-full_join(ipeds.complete20f,ipeds.complete21f)
#remove used datasets
rm(ipeds.complete20f,ipeds.complete21f)

#can vlookup now by merging ipeds.complete with survey, but only those have a match in survey
survey<-left_join(survey,ipeds.complete,by=c("PC_ID"="People Code ID"))
#assign the newly appended degree info (from ipeds.complete) to the correct exicted degree col
survey$Degree.x<-survey$Degree.y
survey<-survey%>%select(-Degree.y)

