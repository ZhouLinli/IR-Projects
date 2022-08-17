#16-17 graduates are in 2017 fall ipeds.complete report
#17-18 graduates are in 2018 fall ipeds.compelte
#19-20 graduates are in 2020 fall ipeds.compelte
#20-21 graduates are in 2021 fall ipeds.complete


###########################load R package###########################
library(readxl)
library(plyr)#for join_all

#################################################################
###################read, select/rename useful vars###############
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
ipeds.complete20f<-ipeds.complete20f%>%select(`People Code ID`,Program,Degree,Curriculum,GradDate,`CIP description`,Race,Gender)%>%rename(PCID=`People Code ID`,level=Program,degree=Degree,major=Curriculum,gradd=GradDate,race=Race,gender=Gender)
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
ipeds.complete18f<-ipeds.complete18f%>%select(people_code_id,program,degree,curriculum,graduation_date,Race,gender)%>%rename(PCID=people_code_id,level=program,major=curriculum,gradd=graduation_date,race=Race)
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

ipeds.complete<-join_all(list(ipeds.complete17f,ipeds.complete18f,ipeds.complete19f,ipeds.complete20f,ipeds.complete21f),type="full",match="first")#3020 observations

ipeds.complete<-join_all(list(ipeds.complete17f,ipeds.complete18f,ipeds.complete19f,ipeds.complete20f,ipeds.complete21f),by="PCID",type="full",match="first")#2915 observations

ipeds.complete<-rbind(ipeds.complete17f,ipeds.complete18f,ipeds.complete19f,ipeds.complete20f,ipeds.complete21f)#2915 observations

nrow(ipeds.complete17f)+nrow(ipeds.complete18f)+nrow(ipeds.complete19f)+nrow(ipeds.complete20f)+nrow(ipeds.complete21f)#3021 observations

####################################cleaning ipeds.complete18f data#######################################


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

#Save the corrected degree codes file
library(writexl)
write_xlsx(ipeds.complete18f,"/Users/linlizhou/Documents/LASELL/data/completion/2018ipedsFComp_2017grad.xlsx.xlsx")




#prep merge ipeds.complete

#want to merge col to col exactly append one to the other
#then need to have exact same names:
names(ipeds.complete21f)<-names(ipeds.complete20f)
#merge
ipeds.complete<-full_join(ipeds.complete20f,ipeds.complete21f)
#remove used datasets
rm(ipeds.complete20f,ipeds.complete21f)


