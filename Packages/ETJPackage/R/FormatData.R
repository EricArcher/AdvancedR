

#Bring in data to view

setwd("~/SIO_Classes/SIO296_AdvancedR/AdvancedR/Packages/ETJPackage/Data")
raw <- read.csv("1968TagRecaps.csv", stringsAsFactors = FALSE)
str (raw)

#freq <- table(raw$TagMonth, raw$TagYear)
# my <- expand.grid(1:12, 1968:1971)
#my$fac <- factor(paste(my$year, my$month)
#df$my <- paste(df$year, df$month)

#Change fields to numeric



raw$TagMonth <- as.numeric(raw$TagMonth)

str (raw)

#Add leading zero to TagMonth, RecapMo1, and RecapMo2 for up to 2 digits

sprintf("%02d",raw$TagMonth)
sprintf("%02d",raw$RecapMo1)
sprintf("%02d",raw$RecapMo2)

#Create new Fields by joining the Month and Year Fields

raw$TagEvent <- paste(raw$TagMonth,raw$TagYear, sep = "/")
raw$RecapEvent1 <- paste(raw$RecapMo1,raw$RecapYr1, sep = "/")
raw$RecapEvent2 <- paste(raw$RecapMo2,raw$RecapYr2, sep = "/")

#Format New Fields as Date Fields MMYYYY --- something is not working here....keep getting<NA>


Datetest <- as.yearmon(paste(raw$TagYear,raw$TagMonth))
head(Datetest)


