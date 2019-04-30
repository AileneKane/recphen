#Exploring the recreational fishing data
#By Ailene, with some code taken from Eric Ward
#Started October 15, 2018
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/fishphen/analyses")


# Load libraries
library(mgcv)
#Read in the WDFW recreational fishing data
dat<-read.csv("../data/2001-2013PSChinookLandings.csv", header=TRUE)
#get dates in day-of-year (doy)
head(dat)
dim(dat)#69322 rows
dat$doy<-strftime(strptime(paste(dat$Month, dat$Day, dat$Year, sep="."),format= "%m.%d.%Y"),format= "%j")
dat$week<-strftime(strptime(paste(dat$Month, dat$Day, dat$Year, sep="."),format= "%m.%d.%Y"), format = "%V")#new weeks start on mondays
length(unique(dat$CRCarea))#there are 30 different numbers by ole says there should be only 15 or so, i think...(e.g. 8 has been subdivided into 8.1, 8.2, etc)
sort(unique(dat$CRCarea))

#puget sound is areas 5:13 (including 81, 82). I'm curious what the other area numbers are...
#for now, include outer coast and puget sound/salish sea
dat<-dat[dat$CRCarea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82"),]
dim(dat)# now 62132 rows
#proportion ChinAdClipped- does this indicate hatchery fish?
dat$propclip= as.numeric(dat$ChinAdClipped)/as.numeric(dat$ChinookCaught)
clipped.p<-aggregate(dat$propclip,list(dat$Year,dat$CRCarea), mean, NA.rm=TRUE)
colnames(clipped.p)<-c("Year","CRCarea","pclip.avg")
boxplot(dat$propclip~dat$CRCarea)#areas vary in the proportion of clipped fish- this is due to regulations that vary by area.
boxplot(dat$propclip~dat$Year)#looks like a definite trend toward higher propr clipped...ole says this is partly due changes in fishing regulations (in some areas, you are only allowed to keep clipped fish)

#Eric's code splits out the records into boat data and fish data
chin= dat[which(is.na(dat$Anglers)==FALSE),]
fish = dat[which(is.na(dat$Anglers)==TRUE),]
# Put the fish caught into the effort database
chin$Chinook = 0
indx = match(paste(fish$SampleDate,fish$LoCode,fish$CRCarea),
             paste(chin$SampleDate,chin$LoCode,chin$CRCarea))
fishCaught = fish$ChinookCaught[-which(is.na(indx))]
chin$Chinook[indx[-which(is.na(indx))]] = fishCaught

#Try plotting data by week, to remove strong patterns in week vs weekends, with different colors for different CRC areas
#two runs of chinook- spring and fall

fmin=2 #minimum number of fish, used to calculate first obs, last obs
fst = 20# for fall runs, look after week 20
fend = 40 # and before week 40
wst = 40#some years there seems to be a winter run too...
wend = 52
chinphen.yrs<-data.frame(year=numeric(length(2001:2013)), 
                     spfirst=numeric(length(2001:2013)), 
                     sppk=numeric(length(2001:2013)), 
                     splast=numeric(length(2001:2013)),
                     fafirst=numeric(length(2001:2013)), 
                     fapk=numeric(length(2001:2013)), 
                     falast=numeric(length(2001:2013)),
                     wifirst=numeric(length(2001:2013)), 
                     wipk=numeric(length(2001:2013)), 
                     wilast=numeric(length(2001:2013)),
                     stringsAsFactors=FALSE)
# Loop over years
quartz()
par(mfrow=c(4,4))
for(y in 2001:2013) {
  # Sum up numbers of chinook by week for each year, across areas
  chinookYear = aggregate(Chinook ~ week, data = chin[which(chin$Year==y),],sum)
  w = as.numeric(chinookYear$week)
  c= as.numeric(chinookYear$Chinook)
  #plot the data
  plot(w,c, pch=21,bg="gray",xlab = "Week", ylab = "Chinook caught", main = paste("Year: ",y), bty="l")
  #fit a gam to weekly data
  g = gam(log(c+1) ~ s(w))
  lines(w,exp(g$fitted.values),lwd=3)
  #fall run
  #add line for peak abundance week
  pk<-max(g$fitted.values)
  pkdoy<-w[which.max(g$fitted.values)]
  abline(v=pkdoy, col="red", lwd=2)
  #add line for start of season
  chinweeks<-which(exp(g$fitted.values) > fmin)#weeks with chiinook numbers greater than min
  chinweeks<-chinweeks[which(chinweeks>=fst)]
  fallst<-w[min(chinweeks[which(chinweeks>=fst)])]
  abline(v=fallst, col="red", lty=3,lwd=2)
  #add line for end of season
  fallend<-w[max(chinweeks[which(chinweeks<fend)])]
  abline(v=fallend, col="red", lty=3,lwd=2)
  #spring run
  #add line for peak spring
  sppk<-max(g$fitted.values[1:fst])
  sppkdoy<-w[which(g$fitted.values==sppk)]
  abline(v=sppkdoy, col="green", lwd=2)
  #add line for start of season
  spchinweeks<-which(exp(g$fitted.values) > fmin)#weeks with chinook numbers greater than min
  spchinweeks<-spchinweeks[which(spchinweeks<=fst)]
  springst<-w[min(spchinweeks)]
  abline(v=springst, col="green", lty=3,lwd=2)
  #add line for end of season
  springend<-w[max(spchinweeks)]
  abline(v=springend, col="green", lty=3,lwd=2)
  #winter run
  wichinweeks<-which(exp(g$fitted.values) > fmin)#weeks with chinook numbers greater than min
  winst<-w[min(wichinweeks[which(wichinweeks>=wst)])]
  abline(v=winst, col="blue", lty=3,lwd=2)
  #add line for end of season
  winend<-w[max(wichinweeks[which(wichinweeks<=wend & wichinweeks>=wst)])]
  abline(v=winend, col="blue", lty=3,lwd=2)
  winpk<-max(g$fitted.values[wst:wend])
  winpkdoy<-w[which(g$fitted.values==winpk)]
  abline(v=winpkdoy, col="blue", lwd=2)
  #save phenophase weeks into dataframe
  chinphen.yrs$year[y-2000]<-y
  chinphen.yrs$spfirst[y-2000]<-springst
  chinphen.yrs$sppk[y-2000]<-sppkdoy
  chinphen.yrs$splast[y-2000]<-springend
  chinphen.yrs$fafirst[y-2000]<-fallst
  chinphen.yrs$fapk[y-2000]<-pkdoy
  chinphen.yrs$falast[y-2000]<-fallend
  chinphen.yrs$wifirst[y-2000]<-winst
  chinphen.yrs$wipk[y-2000]<-winpkdoy
  chinphen.yrs$wilast[y-2000]<-winend
}  

#Look at phenophase week for all 6 phenophases and correlations between them
chinphen.yrs
pairs(chinphen.yrs[,2:10])
#No clear relationships among phases
#One patterns that becomes apparent is that spring run is more variable (first spans 8 weeks, mid spans 3 weeks, last spans 2)
#than fall run (first spans 4, peak spans 4, and last spans 2 weeks)

#Now try making a separate plot for each CRC area, across all years
areas<-unique(chin$CRCarea)
chinphen.area<-data.frame(area=numeric(length(areas)), 
                         spfirst=numeric(length(areas)), 
                         sppk=numeric(length(areas)), 
                         splast=numeric(length(areas)),
                         fafirst=numeric(length(areas)), 
                         fapk=numeric(length(areas)), 
                         falast=numeric(length(areas)),
                         wifirst=numeric(length(areas)), 
                         wipk=numeric(length(areas)), 
                         wilast=numeric(length(areas)),
                         stringsAsFactors=FALSE)
#
quartz()
par(mfrow=c(3,4))
for(i in 1:length(areas)){# Loop over areas
  crcdat = chin[chin$CRCarea==areas[i],]
    # Sum up numbers of chinook by week for each year, across areas
    chinookYear = aggregate(Chinook ~ week, data = crcdat,sum)
    w = as.numeric(chinookYear$week)
    c= as.numeric(chinookYear$Chinook)
    #plot the data
    plot(w,c, pch=21,bg="gray",xlab = "Week", ylab = "Chinook caught", main = paste("Area",areas[i]))
    #fit a gam to weekly data
    g = gam(log(c+1) ~ s(w))
    lines(w,exp(g$fitted.values),lwd=3)
    #fall run
    
    #add line for start of season
    chinweeks<-which(exp(g$fitted.values) > fmin)#weeks with chiinook numbers greater than min
    #chinweeks<-chinweeks[which(chinweeks>fst & chinweeks<=fend)]
    #add line for peak abundance week
    fapk<-max(exp(g$fitted.values)[fst:fend])
    fapkdoy<-w[which(exp(g$fitted.values)==fapk)]
    abline(v=fapkdoy, col="red", lwd=2)
    fallst<-w[min(chinweeks[chinweeks>fst & chinweeks<=fend])]
    abline(v=fallst, col="red", lty=3,lwd=2)
    #add line for end of season
    fallend<-w[max(chinweeks[which(chinweeks<fend)])]
    abline(v=fallend, col="red", lty=3,lwd=2)
    #spring run
    #add line for peak spring
    sppk<-max(exp(g$fitted.values)[1:fst])
    sppkdoy<-w[which(exp(g$fitted.values)==sppk)]
    abline(v=sppkdoy, col="green", lwd=2)
    #add line for start of season
    spchinweeks<-which(exp(g$fitted.values) > fmin)#weeks with chinook numbers greater than min
    spchinweeks<-spchinweeks[which(spchinweeks<=fst)]
    springst<-w[min(spchinweeks)]
    abline(v=springst, col="green", lty=3,lwd=2)
    #add line for end of season
    springend<-max(spchinweeks)
    abline(v=springend, col="green", lty=3,lwd=2)
    #winter run
    wichinweeks<-which(exp(g$fitted.values) > fmin)#weeks with chinook numbers greater than min
    if(length(wichinweeks[wichinweeks>wst])>0){winst<-min(wichinweeks[wichinweeks>wst])}
    if(length(wichinweeks[wichinweeks>wst])==0){winst<-NA}  
    abline(v=winst, col="blue", lty=3,lwd=2)
    #add line for end of season
    if(length(wichinweeks[wichinweeks>wst])>0){winend<-max(wichinweeks[wichinweeks<=wend & wichinweeks>wst])}
    if(length(wichinweeks[wichinweeks>wst])==0){winend<-NA}  
    abline(v=winend, col="blue", lty=3,lwd=2)
    winpk<-max(exp(g$fitted.values)[wst:wend])
    winpkdoy<-which(exp(g$fitted.values)==winpk)
    if(length(winpkdoy)==0){winpkdoy<-NA}  
    abline(v=winpkdoy, col="blue", lwd=2)
    #save phenophase weeks into dataframe
    chinphen.area$area[i]<-areas[i]
    chinphen.area$spfirst[i]<-springst
    chinphen.area$sppk[i]<-sppkdoy
    chinphen.area$splast[i]<-springend
    chinphen.area$fafirst[i]<-fallst
    chinphen.area$fapk[i]<-fapkdoy
    chinphen.area$falast[i]<-fallend
    chinphen.area$wifirst[i]<-winst
    chinphen.area$wipk[i]<-winpkdoy
    chinphen.area$wilast[i]<-winend
  }  
  
chinphen.area
pairs(chinphen.area[,2:10])
#Now try making a separate plot for each CRC area AND year
for(i in 1:length(areas)){
crcdat = chin[chin$CRCarea==areas[i],]
  # Loop over years
  quartz()
  par(mfrow=c(4,4))
  for(y in 2001:2013) {
    # Sum up numbers of chinook by week for each year, across areas
    chinookYear = aggregate(Chinook ~ week, data = crcdat[which(crcdat$Year==y),],sum)
    w = as.numeric(chinookYear$week)
    c= as.numeric(chinookYear$Chinook)
    #plot the data
    plot(w,c, pch=21,bg="gray",xlab = "Week", ylab = "Chinook caught", main = paste("Year: ",y,"Area",areas[i]))
    #fit a gam to weekly data
    g = gam(log(c+1) ~ s(w))
    lines(w,exp(g$fitted.values),lwd=3)
  }  
  
}

#The below code keeps the daily patterns in place and accounts for effort

chinphen.yrs.cpue<-data.frame(year=numeric(length(2001:2013)), 
                              spfirst=numeric(length(2001:2013)), 
                              sppk=numeric(length(2001:2013)), 
                              splast=numeric(length(2001:2013)),
                              fafirst=numeric(length(2001:2013)), 
                              fapk=numeric(length(2001:2013)), 
                              falast=numeric(length(2001:2013)),
                              wifirst=numeric(length(2001:2013)), 
                              wipk=numeric(length(2001:2013)), 
                              wilast=numeric(length(2001:2013)),
                              stringsAsFactors=FALSE)

quartz()
par(mfrow=c(4,4))
# Loop over years
for(y in 2001:2013) {
  # Sum up effort for each calendar day across areas
  anglersYear = aggregate(Anglers ~ week, data = chin[which(chin$Year==y),],sum)
  # Sum up Chinook for each calendar day across areas
  chinookYear = aggregate(Chinook ~ week, data = chin[which(chin$Year==y),],sum)
  # Fit the gam, using log(effort) as offset
  x = as.numeric(chinookYear$week)
  effort = anglersYear$Anglers
  g = gam(log(chinookYear$Chinook+1) ~ s(x) + offset(log(effort)))
  plot(x,exp(g$fitted.values), type="l",lwd=2,xlab = "Week",
       ylab = "Expected recreational catch", main = paste("Year: ",y))
  #add line for peak abundance week
  #pk<-max(g$fitted.values)``
  #pkdoy<-x[which.max(g$fitted.values)]
  #abline(v=pkdoy, col="red", lwd=3)
  #add line for start of season
  chinweeks<-which(exp(g$fitted.values) > fmin)#weeks with chiinook numbers greater than min
  chinweeks<-chinweeks[which(chinweeks>fst)]
  fallst<-min(chinweeks[which(chinweeks>fst)])
  abline(v=fallst, col="red", lty=3,lwd=2)
  #add line for end   lines(w,exp(g$fitted.values),lwd=3)
  #fall run
  #add line for peak abundance week
  pk<-max(g$fitted.values)
  pkdoy<-x[which.max(g$fitted.values)]
  abline(v=pkdoy, col="red", lwd=2)
  #add line for start of season
  chinweeks<-which(exp(g$fitted.values) > fmin)#weeks with chiinook numbers greater than min
  chinweeks<-chinweeks[which(chinweeks>=fst)]
  fallst<-x[min(chinweeks[which(chinweeks>=fst)])]
  abline(v=fallst, col="red", lty=3,lwd=2)
  #add line for end of season
  fallend<-x[max(chinweeks[which(chinweeks<fend)])]
  abline(v=fallend, col="red", lty=3,lwd=2)
  #spring run
  #add line for peak spring
  sppk<-max(g$fitted.values[1:fst])
  sppkdoy<-x[which(g$fitted.values==sppk)]
  abline(v=sppkdoy, col="green", lwd=2)
  #add line for start of season
  spchinweeks<-which(exp(g$fitted.values) > fmin)#weeks with chinook numbers greater than min
  spchinweeks<-spchinweeks[which(spchinweeks<=fst)]
  springst<-x[min(spchinweeks)]
  abline(v=springst, col="green", lty=3,lwd=2)
  #add line for end of season
  springend<-x[max(spchinweeks)]
  abline(v=springend, col="green", lty=3,lwd=2)
  #winter run
  wichinweeks<-which(exp(g$fitted.values) > fmin)#weeks with chinook numbers greater than min
  winst<-x[min(wichinweeks[which(wichinweeks>=wst)])]
  abline(v=winst, col="blue", lty=3,lwd=2)
  #add line for end of season
  winend<-x[max(wichinweeks[which(wichinweeks<=wend & wichinweeks>=wst)])]
  abline(v=winend, col="blue", lty=3,lwd=2)
  winpk<-max(g$fitted.values[wst:wend])
  winpkdoy<-x[which(g$fitted.values==winpk)]
  abline(v=winpkdoy, col="blue", lwd=2)
  #save phenophase weeks into dataframe
  chinphen.yrs.cpue$year[y-2000]<-y
  chinphen.yrs.cpue$spfirst[y-2000]<-springst
  chinphen.yrs.cpue$sppk[y-2000]<-sppkdoy
  chinphen.yrs.cpue$splast[y-2000]<-springend
  chinphen.yrs.cpue$fafirst[y-2000]<-fallst
  chinphen.yrs.cpue$fapk[y-2000]<-pkdoy
  chinphen.yrs.cpue$falast[y-2000]<-fallend
  chinphen.yrs.cpue$wifirst[y-2000]<-winst
  chinphen.yrs.cpue$wipk[y-2000]<-winpkdoy
  chinphen.yrs.cpue$wilast[y-2000]<-winend
}  


chinphen.yrs.cpue
pairs(chinphen.yrs.cpue[,2:10])

#Eric's code:
#The CRC area field represents Catch record Card areas – probably the finest spatial scale we want to deal
#with, because there are only 30. For now, we’ll say we just aggregate across spatial areas, but we can apply
#more complicated models.
#For now, we can just fit some GAMs to each of the years independently. We have data from 2001-2013, so we
#can make separate plots for each. Each of the plots includes a clear weekly signal, with peaks on weekends
#(when effort is higher).
#Across all years:
anglers = aggregate(Anglers ~ doy, data = chin,sum)
# Sum up Chinook for each calendar day across areas
chinook = aggregate(Chinook ~ doy, data = chin,sum)
# Fit the gam, using log(effort) as offset
x = as.numeric(chinook$doy)
effort = anglers$Anglers
quartz()
g = gam(log(chinook$Chinook+1) ~ s(x) + offset(log(effort)))
plot(x,exp(g$fitted.values), type="l",lwd=3,xlab = "Day of Year",
     ylab = "Expected recreational catch", col="salmon")

# Loop over years
for(y in 2001:2013) {
  # Sum up effort for each calendar day across areas
  anglersYear = aggregate(Anglers ~ doy, data = chin[which(chin$Year==y),],sum)
  # Sum up Chinook for each calendar day across areas
  chinookYear = aggregate(Chinook ~ doy, data = chin[which(chin$Year==y),],sum)
  # Fit the gam, using log(effort) as offset
  x = as.numeric(chinookYear$doy)
  effort = anglersYear$Anglers
  g = gam(log(chinookYear$Chinook+1) ~ s(x) + offset(log(effort)))
  plot(x,exp(g$fitted.values), type="l",lwd=3,xlab = "Day of Year",
       ylab = "Expected recreational catch", main = paste("Year: ",y))
  
}  

