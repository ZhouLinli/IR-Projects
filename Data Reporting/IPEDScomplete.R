#16-17 graduates are in 2017 fall ipeds.complete report
#17-18 graduates are in 2018 fall ipeds.compelte
#20-21 graduates are in 2021 fall ipeds.complete

library(readxl)
####################################cleaning ipeds.complete18f data#######################################
##read data
ipeds.complete18f<-read_excel("/Users/linlizhou/Documents/LASELL/data/completion/2018ipedsFComp_2017grad.xlsx.xlsx",sheet="Merged")

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







