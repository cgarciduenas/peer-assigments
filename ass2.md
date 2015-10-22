
##Synopsis 
In this report I try to describe the impact of weather events in the United States from to august 2007. It summarizes the morbimortality and total cost burden of the weather events.
For the data analisys I used a cluster analisys to agglomerate the events since the same event was reported with many similar words.
In the health related damage I added together the fatalities and injuries un one variable called MorbiMortality. 
The costs variables was registered in four variables the first in whole numbers and the second as an multiplier. To calculate the total cost I multiplied the DMG and DMGEXP variables, and finally added together in a variable called TotalCost. 

##Data Processing
The information was obtained from the NOAA Satellite Information Service. The data was suministred by the National Weather Service (NWS) after the end of each month. 

The file can be downloaded from 
The data is a delimited file were fields are comma delimited, the miising values are not coded uniformly and I supposed the "?", "+", "-" and " " as NAs since there was not crearly stated in the report
```{r dowloadfile, cache = TRUE}
fileurl <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileurl, destfile = "storm.csv.bz2")
Storm_down <- read.csv("storm.csv.bz2", sep=",", header=T, na.strings = c("?", " ", "-", "+"))
```

first select columns to work with
```{r columns}
library(plyr)
library(dplyr)
Storm <- select(Storm_down,EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
str(Storm)
```

the first question is, across the United States, which types of events are most harmful with respect to population health?

The morbi-mortality includes fatality cases and injuries, there is no need to weigth one over the other for epidemiologic purpouses. The morbimortality is simply the sum of fatalities and injuries.
Type of storm event. The database includes similar storm events that can be listed using different wording e.g. "coastal flood" and "coastal flooding." Before any further analysis I exclude the cases with zero fatalities and injuries, and aggregate by event type, even that, there was more than 200 events types.
To solve the problem of different wording I used a clustering approach wit the "adist" function. The Dentogram helps to decide the high threshold, I used a threshold of 1.8.
The final table includes some "outliers" like wintry mix, that could be mixed with any other winter variable (storm, weather, or weather mix), but I decided not to modify this final output because the National Weather Service includes winter storm and winter weather but not winter mixed events. There are events that do not appear in the report like rough wave or seas and hail, for non English speakers or even for those who live in tropical areas can be very difficult to distinguish or classify it by hand.

```{r mutatemorb}
#
Health <- mutate (Storm, MorbiMortality= INJURIES + FATALITIES)
HealthByEvent <- aggregate(MorbiMortality ~ EVTYPE, sum, data=Health) 
MostHealth <- filter (HealthByEvent,MorbiMortality>0 )


lev_dist <- adist(MostHealth$EVTYPE, MostHealth$EVTYPE, partial=TRUE, ignore.case=TRUE)
hc <- hclust(as.dist(lev_dist), method='single')
plot(hc, labels=MostHealth$EVTYPE)
threshold <- 1.8
cluster <- cutree(hc, h=threshold)
cluster_labels <- sapply(unique(cluster), function(i) MostHealth$EVTYPE[min(which(cluster == i))])
(new_names <- cluster_labels[cluster])

clustered <- cbind(MostHealth, new_names)  
clustered <- aggregate(MorbiMortality~new_names, sum, data=clustered)
colnames(clustered) <- c("Event","MorbiMortality") 
```
The final table of morbimortality by event is:
´´´{r morbility}
arrange (clustered, desc(MorbiMortality))
´´´
In the National Weather Service report, Tornado, Avalanche and Winter storm are the top 3 events with the highest morbi-mortality. 

#Across the United States, which types of events have the greatest economic consequences?
The report uses an DMG variable for property and crop damage in whole 
numbers and hundredths and has a multiplier factor for hundreds(H), thousand(K), million (M) and billions (B), besides there are another multipliers from 0 to 9 and signs, that used as a factor of 10 (10^n). The sign "-", "+", "?" wehre considered as NAs.

First I select the cases with a cost >0
```{r cost}
library(data.table)
Burden<- filter (Storm,PROPDMG>0 | CROPDMG>0)
str(Burden)
´´´


´´´{r NAs}
summary(Burden$CROPDMGEXP)
summary(Burden$PROPDMGEXP)
#transform exp to number
Burden$CROPDMGEXP <- revalue(Burden$CROPDMGEXP, c("B"="9", "K"="3", "k"="3", "m"="9", "M"="9" ))
Burden$PROPDMGEXP <- revalue(Burden$PROPDMGEXP, c("B"="9", "h"="2", "H"="2", "K"="3", "m"="9", "M"="9" ))

#create a new variable multiplyin PROPDMG by 10^PROPDMGEXP and CROPDMG by 10^CROPDMGEXP
falta cambiar las variables a numericas
BurdenTotal <- mutate (Burden, PropTotal= PROPDMG*(exp(PROPDMGEXP))



