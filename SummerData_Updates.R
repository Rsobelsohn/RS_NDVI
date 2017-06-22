library(plyr)
library(lubridate)
setwd("c:\\Users\\Rob\\Desktop\\NDVI_Research\\net-R\\netR\\NDVI_Summer")
datSDC <- read.csv("Dav_canopy.csv")
datSDC5 <- read.csv("Dav_canopy_2015.csv")
datSDU <- read.csv ("Dav_understory_veg.csv")
datSLC <- read.csv("LDF2_canopy.csv")
datSLC5 <- read.csv("LDF2_canopy_2015.csv")
datSLU <- read.csv("LDF2_understory_veg.csv")
################ A 5 means 2015 data ##################

################################################################################
###Make sure to keep the Time Stamp consistent for merging later################
###I tried excluding the first column in colnames but kept recieving an error###
###Two columns for timestamp in SLU, get rid of the non-corrected one###########
################################################################################

colnames(datSDC)[1:14] <- paste0(colnames(datSDC)[1:14],"SDC")
names(datSDC)[1] <- "TimeStamp"
colnames(datSDU)[1:12] <- paste0(colnames(datSDU)[1:12],"SDU")
names(datSDU)[1] <- "TimeStamp"
colnames(datSLC)[1:14] <- paste0(colnames(datSLC)[1:14],"SLC")
names(datSLC)[1] <- "TimeStamp"
colnames(datSLU)[1:9] <- paste0(colnames(datSLU)[1:9],"SLU")
datSLU <- datSLU[,-2]
names(datSLU)[1] <- "TimeStamp"

yday
####DATES ALREADY CORRECTED####
Merge1 <- merge (datSLU,datSLC, by= "TimeStamp", all=T)
Merge2 <- merge(datSDC,datSDU,by="TimeStamp",all=T)
Mergedat <- merge(Merge1,Merge2, by="TimeStamp", all=T)

################################################################################
#Converting TimeStamp into a POSIX?
#TimeStamp <- as.POSIXct(unlist(TimeStamp), format = "%y-%m-%d H")
#TimeStamp <- as.POSIXct(TimeStamp)
#as.POSIXlt(strptime(as.character(Mergedat[colnames(TimeStamp) %in% "timestamp"]), 
#                    format = "%Y-%m-%d %H"),
#           format = "%Y-%m-%d %H")


#Error "origin must be supplied"
#yday(252, Mergedat$TimeStamp)
#TimeStamp <- format(as.POSIXct(strptime(Mergedat$TimeStamp,
#                                  "%m/%d/%y %I:%M %p", tz="")), 
#                                  format="%m/%d/%y %I:%M %p")
#strptime(Mergedat$TimeStamp, "%I,%M %p)

Dates <- format(as.POSIXct(strptime
                           (Mergedat$TimeStamp,"%m/%d/%y %H:%M",tz="est")) ,
                            format = "%m/%d/%y")
Hours <- format(as.POSIXct(strptime
                           (Mergedat$TimeStamp,"%m/%d/%y %H:%M",tz="est")) ,
                            format = "%H:%M")


Mergedat$Dates <- Dates
Mergedat$Hours <- Hours

doy <- format(as.POSIXct(strptime(Mergedat$Dates, "%m/%d/%y", tz="est")) , format = "%j")
Mergedat$doy <- doy

#################################################################
### The Dates column and doy do not work unless time ############
### is converted to 24 hours instead of AM/PM ###################
### Why will it not display? ####################################
#################################################################


#x <- as.Date(Mergedat$TimeStamp,"%m/%d/%Y")
#Mergedat$Date_V1 = x
#year(x)
#month(x)

#doy <- strftime("TimeStamp", format = "%j")
#as.numeric (doy)

#w <- as.Date(Mergedat$TimeStamp,"%m")
#x <- as.Date(Mergedat$TimeStamp, "%d")
#y <- as.Date(Mergedat$TimeStamp, "%Y")
#z <- as.Date(Mergedat$TimeStamp, "%H")
#Mergedat$Month = w
#Mergedat$Day = x
#Mergedat$Year = y
#Mergedat$Hour = z


#Mergedat$TimeStamp <- as.Date(TimeStamp)
#Mergedat$Time <- as.POSIXlt(x,sep= ".",format="%Y.%M.%j.")

#as.POSIXct.date(Mergedat[1])
##How would I display TimeStamp as year.month.day.hour##
##since I didn't join columns?##
#yday(Mergedat)



#a=mdy(Dates)
#year(a)

##Pull day of year from the Dates Column## 

#yday(Mergedat$Dates)
#doy <- strftime(Dates, format = "%j")
#doy

#doy <- strftime(Mergedat$TimeStamp, format = "%j")
#Mergedat$doy=Mergedat


#################################################################
##All data is now in one environment tied together by TimeStamp##
##Still need to correct NDVI of upper canopies###################
###### Correct the data for upper canopy ########################
##Calculate new NDVI using NDVI= (NIR-R)/(NIR+R)#################
## NDVI as (alpha*800nm-630nm)/(alpha*800nm+630nm) ##############
## Need to correct LDF2_canopy and DAV_canopy_2015###############
#################################################################

Mergedat$ndvi.1SLC <- (Mergedat$up.alphaSDC*Mergedat$down1.800SLC-
                             Mergedat$down1.630SLC)/
  (Mergedat$up.alphaSDC*Mergedat$down1.800SLC+Mergedat$down1.630SLC)
Mergedat$Ndvi.2SLC <- (Mergedat$up.alphaSDC*Mergedat$down2.800SLC-
                             Mergedat$down2.630SLC)/
  (Mergedat$up.alphaSDC*Mergedat$down2.800SLC+Mergedat$down2.630SLC)

##Setting Conditions##
##Eliminate NDVI less than 0 and greater than 1##
Mergedat_NEWSDC <- ifelse(Mergedat$ndvi.1SDC<0|Mergedat$ndvi.1SDC>1, NA,
                          Mergedat$ndvi.1SDC)
Mergedat_NEWSDU <- ifelse(Mergedat$ndvi.1SDU<0|Mergedat$ndvi.1SDU>1, NA,
                          Mergedat$ndvi.1SDU)
Mergedat_NEWSLC <- ifelse(Mergedat$ndvi.1SLC<0|Mergedat$ndvi.1SLC>1, NA,
                          Mergedat$ndvi.1SLC)
Mergedat_NEWSLU <- ifelse(Mergedat$ndvi.1SLU<0|Mergedat$ndvi.1SLU>1, NA,
                          Mergedat$ndvi.1SLU)

###########################
##Daily Mean based on doy##
##Calculate mean per day###
###########################
AveNDVISDC <- aggregate(Mergedat_NEWSDC, by=list
                        (Mergedat$doy), FUN="mean", 
                        na.action=na.omit, na.rm=TRUE)
AveNDVISDU <- aggregate(Mergedat_NEWSDU, by=list
                        (Mergedat$doy), FUN="mean",
                        na.action=na.omit, na.rm=TRUE) 
AveNDVISLC <- aggregate(Mergedat_NEWSLC, by=list
                        (Mergedat$doy), FUN="mean",
                        na.action=na.omit, na.rm=TRUE)
AveNDVISLU <- aggregate(Mergedat_NEWSLU, by=list
                        (Mergedat$doy), FUN="mean",
                        na.action=na.omit, na.rm=TRUE)


## SDU comes up as all NA, could this be from the doy read in issue##

colnames(AveNDVISDC) [1] <- "doy"
colnames(AveNDVISDC) [2] <- "NDVI.sdc"
colnames(AveNDVISDU) [1] <- "doy"
colnames(AveNDVISDU) [2] <- "NDVI.sdu"
colnames(AveNDVISLC) [1] <- "doy"
colnames(AveNDVISLC) [2] <- "NDVI.slc"
colnames(AveNDVISLU) [1] <- "doy"
colnames(AveNDVISLU) [2] <- "NDVI.slu"

Join_S1 <- join(AveNDVISDC, AveNDVISDU, by=c("doy"), type="full")
Join_S2 <- join(AveNDVISLC, AveNDVISLU, by=c("doy"), type="full")
datJoinS <- join(Join_S1, Join_S2, by=c("doy"), type="full")

plot(datJoinS$doy, datJoinS$NDVI.sdc, type="l",
     col="black",xlab = "Day of Year", ylab = "NDVI", 
     ylim = c(0,1))
points(datJoinS$doy, datJoinS$NDVI.sdu, type="l", col="blue")
points(datJoinS$doy, datJoinS$NDVI.slc, type="l", col="red")
points(datJoinS$doy, datJoinS$NDVI.slu, type="l", col="green")

### DU is absent from the plot ###

### Looking at TLC ####

AveTLC_SDU <- aggregate(Mergedat$t.airSDC, by=list (Mergedat$doy),
                        FUN="mean", na.action=na.omit, na.rm=TRUE)
AveTLC_SLC <- aggregate(Mergedat$t.airSLC, by=list (Mergedat$doy),
                        FUN="mean", nea.action=na.omit, na.rm=TRUE)
colnames(AveTLC_SDU) [1] <- "doy"
colnames(AveTLC_SDU) [2] <- "TLC.sdu"
colnames(AveTLC_SLC) [1] <- "doy"
colnames(AveTLC_SLC) [2] <- "TLC.slc"
Join_TLC <- join(AveTLC_SDU, AveTLC_SLC, by=c("doy"), type="full")
plot(Join_TLC$doy, Join_TLC$TLC.sdu, type="l",
     col="red",xlab = "Day of Year", ylab = "TLC", 
     ylim = c(-50,50))
points(Join_TLC$doy, Join_TLC$TLC.slc, type="l", col="green")

### Looking at LWS ####
AveLWS_SDU <- aggregate(Mergedat$lws.count.y, by=list (Mergedat$doy),
                       FUN="mean", na.action=na.omit, na.rm=TRUE)
AveLWS_SLC <- aggregate(Mergedat$lws.count.x, by=list (Mergedat$doy),
                       FUN="mean", na.action=na.omit, na.rm=TRUE)
colnames(AveLWS_SDU)[1] <- "doy"
colnames(AveLWS_SDU)[2] <- "LWS.sdu"
colnames(AveLWS_SLC)[1] <- "doy"
colnames(AveLWS_SLC)[2] <- "LWS.slc"
Join_LWS <- join(AveLWS_SDU, AveLWS_SLC, by=c("doy"), type="full")
plot(Join_LWS$doy, Join_LWS$LWS.sdu, type="l",
     col="black",xlab = "Day of Year", ylab = "TLC", 
     ylim = c(,900))
points(Join_LWS$doy, Join_LWS$LWS.slc, type="l", col="white")

