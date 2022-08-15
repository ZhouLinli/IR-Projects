#This file is a simplified version of "AlumniCareerTracking-grad6m.R". It merges only linkedin and survey data (i.e., disregarding historical data).
#############################load package#############################
library(readxl)
library(dplyr)
library(writexl)
#############################read data files##########################
#survey
survey<-read_excel("/Users/linlizhou/Documents/LASELL/data/alumni/Grad 6-month survey Results 2021.xlsx")

###########################################################################
######################survey data: to match db cols########################
######remove auto-generated survey cols (timestart, status, etc.)#######
#find out the index
names(survey)
survey<-survey%>%select(c(`First Name`,`Last Name`,`Invite Custom Field 1`,53:94))%>%rename(PCID="Invite Custom Field 1")

#############matching survey degree info in pop#######################
#read data
pop<-read_excel("/Users/linlizhou/Documents/LASELL/data/alumni/Class of 2021 GR Population.xlsx")
#select useful cols
pop<-pop%>%select(`Power Campus ID`,`Degree: Alumni Degree Name`)%>%rename(PCID="Power Campus ID")
#merge
survey<-left_join(survey,pop)