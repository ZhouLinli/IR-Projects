#16-17 graduates are in 2017 fall ipeds.complete report
#17-18 graduates are in 2018 fall ipeds.compelte
#19-20 graduates are in 2020 fall ipeds.compelte
#20-21 graduates are in 2021 fall ipeds.complete


###########################load R package###########################
library(readxl)
library(dplyr)
library(plyr)#for join_all
library(writexl)

#################################################################
#####################merge term for annual data##################
#################################################################
#read term data (from report manager-graduation-final list of graduates by term degree)
grad21.8<-read.csv("/Users/linlizhou/Documents/LASELL/data/completion/RptMgr.grdbydg/2021.8.gradreport.csv")
grad21.12<-read.csv("/Users/linlizhou/Documents/LASELL/data/completion/RptMgr.grdbydg/2021.12.gradreport.csv")
grad22.5<-read.csv("/Users/linlizhou/Documents/LASELL/data/completion/RptMgr.grdbydg/2022.5.gradreport.csv")
#check names
names(c(grad21.8,grad21.12,grad22.5))#needs rename
#rename
grad21.12<-grad21.12%>%
select(textbox41,textbox14,DEGREE,major,textbox4)%>%
  rename(PCID=textbox41,level=textbox14,degree=DEGREE,gradd=textbox4)

grad21.8<-grad21.8%>%
select(textbox41,textbox14,DEGREE,major,textbox4)%>%
  rename(PCID=textbox41,level=textbox14,degree=DEGREE,gradd=textbox4)

grad22.5<-grad22.5%>%
  select(textbox41,textbox14,DEGREE,major,textbox4)%>%
  rename(PCID=textbox41,level=textbox14,degree=DEGREE,gradd=textbox4)
#join_all by full
ipeds.complete22f<-join_all(list(grad21.8,grad21.12,grad22.5),type="full",match="first")#548 observations
#check
sum(sapply(list(grad21.12,grad21.8,grad22.5), nrow))#matched 548 observations
#remove
rm(grad21.12,grad21.8,grad22.5)
#save
write_xlsx(ipeds.complete22f,"/Users/linlizhou/Documents/LASELL/data/completion/2022IPEDScompletions.xlsx")

#################################################################
#####read, select/rename useful vars from annual merged data#####
#################################################################

###########################2021 fall ipeds.complete
#check which sheet to read
excel_sheets("/Users/linlizhou/Documents/LASELL/data/completion/2021IPEDScompletions.xlsx")
#read data
ipeds.complete21f<-read_excel("/Users/linlizhou/Documents/LASELL/data/completion/2021IPEDScompletions.xlsx",sheet = "Degrees - Combined")
#select useful cols and rename (prep for merge)
ipeds.complete21f<-ipeds.complete21f%>%select(textbox41,textbox14,DEGREE,major,`Grad Date`)%>%rename(PCID=textbox41,level=textbox14,degree=DEGREE,gradd=`Grad Date`)
#check names - prep for merge
names(ipeds.complete21f)#do not have race and gender in raw data

############################2020 fall ipeds.complete#
#find the right sheet to read
excel_sheets("/Users/linlizhou/Documents/LASELL/data/completion/2020Merged Completions.xlsx")
#read data
ipeds.complete20f<-read_excel("/Users/linlizhou/Documents/LASELL/data/completion/2020Merged Completions.xlsx",sheet = "Merged")
#select useful cols and rename (prep for merge)
ipeds.complete20f<-ipeds.complete20f%>% select(`People Code ID`,Program,Degree,Curriculum,GradDate,Race,Gender) %>% 
  rename(PCID=`People Code ID`, level=Program, degree=Degree, major=Curriculum, gradd=GradDate, race=Race, gender=Gender)
#check names - prep for merge
names(ipeds.complete20f)#keep a cip description for later use


############################2019 fall ipeds.complete
#find the right sheet to read
excel_sheets("/Users/linlizhou/Documents/LASELL/data/completion/2019Merged Completions.xlsm")
#read data
ipeds.complete19f<-read_excel("/Users/linlizhou/Documents/LASELL/data/completion/2019Merged Completions.xlsm",sheet = "AllMerged")
#select useful cols and rename (prep for merge)
ipeds.complete19f<-ipeds.complete19f%>%select(people_code_id,program,degree,curriculum,graduation_date,Race,gender)%>%rename(PCID=people_code_id,level=program,major=curriculum,gradd=graduation_date,race=Race)
#check names - prep for merge
names(ipeds.complete19f)



############################2018 fall ipeds.complete
#find the right sheet to read
excel_sheets("/Users/linlizhou/Documents/LASELL/data/completion/2018ipedsFComp_2017grad.clean.xlsx")
#read data
ipeds.complete18f<-read_excel("/Users/linlizhou/Documents/LASELL/data/completion/2018ipedsFComp_2017grad.clean.xlsx")
#select useful cols and rename (prep for merge)
ipeds.complete18f<-ipeds.complete18f%>%select(people_code_id,program,degree,curriculum,graduation_date,Race,gender)%>%
  rename(PCID=people_code_id,level=program,major=curriculum,gradd=graduation_date,race=Race)
#check names - prep for merge
names(ipeds.complete18f)


############################2017 fall ipeds.complete
#find the right sheet to read
excel_sheets("/Users/linlizhou/Documents/LASELL/data/completion/2017ipedsFComp_2016grad.xlsx")
#read data
ipeds.complete17f<-read_excel("/Users/linlizhou/Documents/LASELL/data/completion/2017ipedsFComp_2016grad.xlsx",sheet = "Merged_ALL")
#select useful cols and rename (prep for merge)
ipeds.complete17f<-ipeds.complete17f%>%select(`People Code ID`,Program,Degree,Curriculum,`Graduation Date`,Race,Gender)%>%rename(PCID=`People Code ID`,level=Program,degree=Degree,major=Curriculum,gradd=`Graduation Date`,race=Race,gender=Gender)
#check names - prep for merge
names(ipeds.complete17f)




#################################################################
############merge the above selected ipeds.compeltes#############
#################################################################

#########################explore the different functions using 17-20f data
#rbind needs col of same length
ncol(ipeds.complete17f)
ncol(ipeds.complete18f)
ncol(ipeds.complete19f)
ncol(ipeds.complete20f)
#rbind
ipeds.complete<-rbind(ipeds.complete17f,ipeds.complete18f,ipeds.complete19f,ipeds.complete20f)#2421 observations
#exactly the same with adding all rows
nrow(ipeds.complete17f)+nrow(ipeds.complete18f)+nrow(ipeds.complete19f)+nrow(ipeds.complete20f)#2421 observations

#rbind is the same with join by all matched cols
ipeds.complete<-join_all(list(ipeds.complete17f,ipeds.complete18f,ipeds.complete19f,ipeds.complete20f),type="full",match="first")#2420 observations; closest to rbind

#by=PCID limit to merge (turn into one) if find PCID in other dataset; match=first, only takes care of first match
ipeds.complete<-join_all(list(ipeds.complete17f,ipeds.complete18f,ipeds.complete19f,ipeds.complete20f),by="PCID",type="full",match="first")#2368 observations
#by=PCID limit to (turn into one) if find PCID in other dataset; match = all merge create a copy for all match
ipeds.complete<-join_all(list(ipeds.complete17f,ipeds.complete18f,ipeds.complete19f,ipeds.complete20f),by="PCID",type="full",match="all")#2379 observations

#########################use join all for merging 17-21f data
ipeds.complete<-join_all(list(ipeds.complete17f,ipeds.complete18f,ipeds.complete19f,ipeds.complete20f,ipeds.complete21f),type="full",match="first")#3020 observations
#save merged data
write_xlsx(ipeds.complete,"/Users/linlizhou/Documents/LASELL/data/completion/17-21f.xlsx")





#################################################################
######################cleaning ipeds.compeltes###################
#################################################################

####################################level
ipeds.complete%>%group_by(level)%>%count()
#print all unique values
table(ipeds.complete$level)
#if any na
sum(is.na(ipeds.complete$level))
####################################degree
ipeds.complete%>%group_by(degree)%>%count()
#print all unique values
table(ipeds.complete$degree)
#if any na
sum(is.na(ipeds.complete$degree))
#clean

####################################major
####################################gradd
####################################race
####################################gender



#################################################################
######################archive codes for cleaning "degree" values###################
#################################################################
#check conc (invalid code in degree)
ipeds.complete18f%>%group_by(degree)%>%count()#here is where conc exists
#list all curriculum under conc
ipeds.complete18f%>%filter(degree=="CONC")%>%group_by(curriculum)%>%count()

#replace conc with the correct degree by looking the student up in powercampus
##powercampus lookup results show that we need to replace all CONC with MSM expect HSGJ as MSCJ
###replace HSGJ with MSCJ
ipeds.complete18f[ipeds.complete18f$curriculum=="HSGJ",11]<-"MSCJ"#no headers/var names anymore so must use 11 as index
#check
ipeds.complete18f%>%filter(degree=="CONC")%>%group_by(curriculum)%>%count()#no more hsgj
###replace all other CONC with MSM
ipeds.complete18f[ipeds.complete18f$degree=="CONC",11]<-"MSM"
#check
ipeds.complete18f%>%group_by(degree)%>%count()#no more conc


#check trk (invalid code in degree)
ipeds.complete18f%>%filter(degree=="TRK")%>%group_by(curriculum)%>%count()
#lookedup in powercampus, replace all INLICM in TRK as MEDMD
ipeds.complete18f[ipeds.complete18f$curriculum=="INLICM",11]<-"MEDMD"
#check
ipeds.complete18f%>%filter(degree=="TRK")%>%group_by(curriculum)%>%count()#no more INLICM
#lookedup in powercampus, replace all PRLICM in TRK as MEDMD
ipeds.complete18f[ipeds.complete18f$curriculum=="PRLICM",11]<-"MEDMD"
#check
ipeds.complete18f%>%filter(degree=="TRK")%>%group_by(curriculum)%>%count()#no more prlicm
#lookedup in powercampus, replace INLICE as MEDEL
ipeds.complete18f[ipeds.complete18f$curriculum=="INLICE",11]<-"MEDEL"
#check
ipeds.complete18f%>%group_by(degree)%>%count()#no more TRK


