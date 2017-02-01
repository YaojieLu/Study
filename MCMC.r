
library(lattice)
library(mgcv)

# Data
Seals <- read.table("Data/Seals.txt", header=TRUE, dec=".")
Seals$fSite <- factor(Seals$Site)
Seals$Time <- Seals$Year+(Seals$Week-1)/52

Seals$fSeason <- Seals$Month
I1<-Seals$Month == 1 | Seals$Month == 2 | Seals$Month == 12
I2<-Seals$Month == 3 | Seals$Month == 4 | Seals$Month == 5
I3<-Seals$Month == 6 | Seals$Month == 7 | Seals$Month == 8
I4<-Seals$Month == 9 | Seals$Month == 10 | Seals$Month == 11
Seals$fSeason[I1] <- "a"
Seals$fSeason[I2] <- "b"
Seals$fSeason[I3] <- "c"
Seals$fSeason[I4] <- "d"
Seals$fSeason <- as.factor(Seals$fSeason)

Seals$fWind2 <- Seals$Winddir
Seals$fWind2[Seals$Winddir == 1 | Seals$Winddir == 2] <- 1
Seals$fWind2[Seals$Winddir == 3 | Seals$Winddir == 4] <- 2
Seals$fWind2[Seals$Winddir == 5 | Seals$Winddir == 6] <- 3
Seals$fWind2[Seals$Winddir == 7 | Seals$Winddir == 8] <- 4
Seals$fWind2 <- factor(Seals$fWind2)

Seals$TDay <- Seals$Timeofday

# Statistics
# GAM
fSeason2 <- Seals$fSeason
M1 <- gamm(Abun ~ s(Month, TDay)+fWind2+fSite,
           weights=varIdent(form=~1 | fSeason2),
           data=Seals)
plot(M1$gam, pers=TRUE)
anova(M1$gam)

# Figures
# LOESS
xyplot(Abun ~ Time | fSite, data=Seals,
       ylab="Abundance", xlab="Time(years)", panel=function(x, y){
         panel.loess(x, y, span=0.3, col=1)
         panel.xyplot(x, y, col=1)
       })

