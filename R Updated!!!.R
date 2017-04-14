library(plyr)
setwd("c:\\Users\\Rob\\Desktop\\NDVI_Research\\net-R\\netR\\NDVI")
datDC <- read.csv("Dav_Canopy_out.csv")
datDU <- read.csv("Dav_understory_out.csv")
datLC <- read.csv("LDF2_canopy_out.csv")
datLU <- read.csv("LDF2_understory_out.csv")

#combines to colnames specific data set (excluding hour, doy, and year)
colnames(datDC)[1:13] <- paste0(colnames(datDC)[1:13],"DC")
colnames(datDU)[1:11] <- paste0(colnames(datDU)[1:11],"DU")
colnames(datLC)[1:9] <- paste0(colnames(datLC)[1:9],"LC")
colnames(datLU)[1:9] <- paste0(colnames(datLU)[1:9],"LU")

#colnames(datLC) <- paste(colnames(datLC),colnames(datLU))
datJoin1 <- join(datDC, datDU, by=c("hour","doy","year"), type="full")
datJoin2 <- join(datLC, datLU, by=c("hour","doy","year"), type="full")
datAll <- join(datJoin1, datJoin2, by=c("hour","doy","year"), type="full")

#Calculate new NDVI
datAll$ndvi1LC.cor <- (datAll$alpha1DC*datAll$ni800.nm1LC-datAll$ni630.nm1LC)/
				(datAll$alpha1DC*datAll$ni800.nm1LC+datAll$ni630.nm1LC)
datAll$ndvi2LC.cor <- (datAll$alpha1DC*datAll$ni80.nm2LC-datAll$ni630.nm2LC)/
				(datAll$alpha1DC*datAll$ni80.nm2LC+datAll$ni630.nm2LC)
names(datAll)

datDC$date <- as.POSIXlt(paste(datDC$year,datDC$doy,datDC$hour,sep="."),
				format="%Y.%j.%H")
datDU$date <- as.POSIXlt(paste(datDU$year,datDU$doy,datDU$hour,sep="."),
				format="%Y.%j.%H")
datLC$date <- as.POSIXlt(paste(datLC$year,datLC$doy,datLC$hour,sep="."),
				format="%Y.%j.%H")
datLU$date <- as.POSIXlt(paste(datLU$year,datLU$doy,datLU$hour,sep="."),
				format="%Y.%j.%H")
Alldat <- merge(datDC,datDU,by="date",all=T)
##Setting Conditions##
datAll_NEWDC <- ifelse(datAll$ndviDC<0|datAll$ndviDC>1, NA, datAll$ndviDC)
datAll_NEWDU <- ifelse(datAll$ndviDU<0|datAll$ndviDU>1, NA, datAll$ndviDU)
datAll_NEWLC <- ifelse(datAll$ndvi1LC<0|datAll$ndvi1LC>1, NA, datAll$ndvi1LC)
datAll_NEWLU <- ifelse(datAll$ndvi1LU<0|datAll$ndvi1LU>1, NA, datAll$ndvi1LU)

##Daily Mean based on doy##
AveNDVIDC <- aggregate(datAll_NEWDC, by=list(datAll$doy), FUN="mean", na.action=na.omit, na.rm=TRUE)
AveNDVIDU <- aggregate(datAll_NEWDU, by=list(datAll$doy), FUN="mean", na.action=na.omit, na.rm=TRUE)
AveNDVILC <- aggregate(datAll_NEWLC, by=list(datAll$doy), FUN="mean", na.action=na.omit, na.rm=TRUE)
AveNDVILU <- aggregate(datAll_NEWLU, by=list(datAll$doy), FUN="mean", na.action=na.omit, na.rm=TRUE)

colnames(AveNDVIDC) [1] <- "doy"
colnames(AveNDVIDC) [2] <- "NDVI.dc"
colnames(AveNDVIDU) [1] <- "doy"
colnames(AveNDVIDU) [2] <- "NDVI.du"
colnames(AveNDVILC) [1] <- "doy"
colnames(AveNDVILC) [2] <- "NDVI.lc"
colnames(AveNDVILU) [1] <- "doy"
colnames(AveNDVILU) [2] <- "NDVI.lu"

Join1 <- join(AveNDVIDC, AveNDVIDU, by=c("doy"), type="full")
Join2 <- join(AveNDVILC, AveNDVILU, by=c("doy"), type="full")
datJoin <- join(Join1, Join2, by=c("doy"), type="full")


plot(datJoin$doy, datJoin$NDVI.dc, type="l",
     col="black",xlab = "Day of Year", ylab = "NDVI", 
     ylim = c(0,1))
points(datJoin$doy, datJoin$NDVI.du, type="l", col="blue")
points(datJoin$doy, datJoin$NDVI.lc, type="l", col="red")
points(datJoin$doy, datJoin$NDVI.lu, type="l", col="green")
#Legend
legend()
#Black= NDVI of Dav Canopy
#Blue= NDVI of Dav Understory
#Red= NDVI of LDF Canopy
#Green= NDVI of LDF Understory
legend(datJoin$doy, datJoin$NDVI),
      c("Dav Canopy","Dav Understory",
        "LDF Canopy", "LDF Understory"),
        col=c("black", "blue", "red", "green")

##Compare NDVI and LWS##
########################
datAll.LWS <- ifelse(datAll$LWS.minDU<0|datAll$LWS.minDU>250, NA, datAll$ndviDC)
AveLWSDU <- aggregate(datAll.LWS, by=list(datAll$doy), FUN="mean", na.action=na.omit, na.rm=TRUE)
colnames(AveLWSDU) [1] <- "doy"
colnames(AveLWSDU) [2] <- "LWS"
##How do I incorporate both NDVI and LWS on a graph)



