#cleaning ipeds.complete18f data
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
#lookedup in powercampus, replace all INLICM and PRLICM in TRK as MEDMD
#lookedup in powercampus, replace INLICE as MEDEL
