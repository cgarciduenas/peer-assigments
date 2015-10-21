
descargar archivo y abrir
seleccionar columnas útiles 
transformear datos especialmente los de dinero

preguntas
Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

para esta pregunta  agregar por tipo de evento o EVTYPE [8,] y FATALITIES+INJURIES [c(23,24)]

Across the United States, which types of events have the greatest economic consequences?

agregar por tipo de evento o EVTYPE y PROPDMG*PROPDMGEXP [c(25,26,)]

```{r dowloadfile, cache = TRUE}
fileurl <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileurl, destfile = "storm.csv.bz2")
Storm_down <- read.csv("storm.csv.bz2", sep=",", header=T)
head(Storm_down)
```

select columns to work with
```{r columns}
library(dplyr)
Storm <- select(Storm_down,EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP)
str(Storm)
```

The morbi-mortality includes fatality cases and injuries, there is no need to weigth one over the other for epidemiologic purpouses. 
The top 30 event with the highest morbimortality across United States since 1950 are listed in the next table.
```{r mutatemorb}
Health <- mutate (Storm, Event = EVTYPE, MorbiMortality= INJURIES + FATALITIES)
HealthByEvent <- aggregate(MorbiMortality ~ Event, sum, data=Health) 
MostHealth <- arrange(HealthByEvent, desc(MorbiMortality))
head (MostHealth, 30)
```
if we consider fatalities and injuries separately the top 30 event with the highest morbimortality are

```{r mortality}
HealthSep <- mutate (Storm, Event = EVTYPE)
head(HealthSep)
HealthSep <- aggregate(cbind(FATALITIES, INJURIES) ~ Event, sum, data=HealthSep) 
MostSep <- arrange(HealthSep, desc(FATALITIES))
head (MostSep, 30)
``` 

When we consider the economic burden of these events we foun that
```{r cost}
summary(Storm)
