## Started April 2019 ##
## by Ailene ##
###cleaning- this should be moved to a cleaning file- ask Ole what to do about this
d$Anglers[which(is.na(d$Anglers) & d$ChinCaught>=1)]<-1#fixes 34
d$Anglers[which(is.na(d$Anglers) & d$CohoCaught>=1)]<-1#fixes 70

## Source file for reading in the data and
## formatting datafile for stan

#add week to dataset
d$week<-strftime(strptime(d$date,format= "%Y-%m-%d"), format = "%V")#new weeks start on mondays
d<-d[which(is.na(d$CRCArea)==FALSE),]
d<-d[d$date!="NANA",]
# separate fish by species to put the fish caught 
anglers = aggregate(d$Anglers, list(paste(d$year,d$week,d$CRCArea, sep="")),sum, na.rm=TRUE)
chinook = aggregate(d$ChinCaught, list(paste(d$year,d$week,d$CRCArea, sep="")),sum, na.rm=TRUE)
coho= aggregate(d$CohoCaught, list(paste(d$year,d$week,d$CRCArea, sep="")),sum, na.rm=TRUE)
#add other species from d$OtherSpCaught
fishsum<-cbind(anglers,chinook$x,coho$x)
colnames(fishsum)<-c("yr.wk.area","anglers","chin","coho")
fishsum$year<-substr(fishsum$yr.wk.area,1,4)
fishsum$week<-substr(fishsum$yr.wk.area,5,6)
fishsum$area<-substr(fishsum$yr.wk.area,7,nchar(fishsum$yr.wk.area))
dim(fishsum)
#fishsum<- fishsum [apply(fishsum , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#there are 1799 rows when anglers = 0
tapply(fishsum$chin[fishsum$anglers==0],list(fishsum$year[fishsum$anglers==0]),length)
#when some fish are caught there must be atleast 1 angler...what
unique(fishsum$chin[fishsum$anglers==0])
#a lot more weeks when anglers =0s in 1991 compared to 1987 and 1989
#should I add one to these
#for an annual sum, across all fishing areas
# separate fish by species to put the fish caught 
anglers.yr = aggregate(d$Anglers, list(paste(d$year,d$week, sep="")),sum, na.rm=TRUE)
chinook.yr = aggregate(d$ChinCaught, list(paste(d$year,d$week, sep="")),sum, na.rm=TRUE)
coho.yr= aggregate(d$CohoCaught, list(paste(d$year,d$week, sep="")),sum, na.rm=TRUE)
#add other species from d$OtherSpCaught
fishsum.yr<-cbind(anglers.yr,chinook.yr$x,coho.yr$x)
colnames(fishsum.yr)<-c("yr.wk.area","anglers","chin","coho")
fishsum.yr$year<-substr(fishsum.yr$yr.wk.area,1,4)
fishsum.yr$week<-substr(fishsum.yr$yr.wk.area,5,6)
dim(fishsum.yr)
tapply(fishsum.yr$chin,list(fishsum.yr$year),sum)
tapply(fishsum.yr$anglers,list(fishsum.yr$year),sum)

#fishsum<- fishsum [apply(fishsum , 1, function(x) all(!is.na(x))),] # only keep rows of all not na

#now sum by area instead of year
# separate fish by species to put the fish caught 
anglers.area = aggregate(d$Anglers, list(paste(d$CRCArea,d$week, sep="")),sum, na.rm=TRUE)
chinook.area = aggregate(d$ChinCaught, list(paste(d$CRCArea,d$week, sep="")),sum, na.rm=TRUE)
coho.area= aggregate(d$CohoCaught, list(paste(d$CRCArea,d$week, sep="")),sum, na.rm=TRUE)
#add other species from d$OtherSpCaught
fishsum.area<-cbind(anglers.area,chinook.area$x,coho.area$x)
colnames(fishsum.area)<-c("areawk","anglers","chin","coho")
fishsum.area$week<-substr(fishsum.area$areawk,nchar(fishsum.area$areawk)-1,nchar(fishsum.area$areawk))
fishsum.area$area<-substr(fishsum.area$areawk,1,nchar(fishsum.area$areawk)-2)

head(fishsum)
#fishsum<- fishsum [apply(fishsum , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
