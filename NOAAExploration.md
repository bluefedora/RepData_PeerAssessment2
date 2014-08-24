## Effects of Severe Weather on Property and Population in the US for the Time Period 1950 to November 2011

blueFedora

8/24/2014

### Synopsis

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This analysis involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

The data were downloaded and subsetted to contain only the pertinent variables.  Some minor cleanup was performed and the data then summarized and the top 20 event types for damage and casualties are listed and the top 10 event types are graphed for your convenience.

### Data Processing
#### Setup the needed libraries and environment.


```r
library(utils)
library(plyr)
library(ggplot2)
library(gridExtra)
```

```
## Loading required package: grid
```

```r
opts_chunk$set(echo=TRUE, fig.width=11, fig.height=6)
events <- 20 # number of event types for which to report results
```

#### Download data and documentation files


```r
Download <- function (fileURL, destfile, method="auto", quiet=TRUE) {
    if(!file.exists("./data")) {
        dir.create("./data")
    }
    destfile <- paste0("./data/", destfile)
    if (!file.exists(destfile)) {
        if (grepl("https:", fileURL) & method=="auto") {
            method <- "curl"
        }
        print(sprintf("DL file: %s on %s", destfile, date()))
        download.file(fileURL, destfile=destfile, method=method, quiet=quiet)
    }
}
#download data file
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
dest <- "StormData.bz2"
Download(url, dest)

#download storm data documentation
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf"
dest <- "pd0101600tcurr.pdf"
Download(url, dest)

#download storm events FAQ
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf"
dest <- "StormEventsFAQ.pdf"
Download(url, dest)
```

#### Read data into StormData data.frame


```r
StormData <- read.csv("./data/StormData.bz2")
```

Exploring the storm data:


```r
dim(StormData)
```

```
## [1] 902297     37
```

```r
names(StormData)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

#### Build a working data.frame and minor data clean up

We'll build a working data.frame and include only the pertinent columns, EVTYPE, CROPDMG, CROPDMGEXP, PROPDMG, PROPDMGEXP, FATALITIES and INJURIES. Also, there are "Summary" EVTYPEs.  These are not "storms" as such and will be removed from the working data. 


```r
workData <- subset(StormData, !grepl("Summary", StormData$EVTYPE), select=c('EVTYPE', 'CROPDMG', 'CROPDMGEXP', 'PROPDMG', 'PROPDMGEXP', 'FATALITIES', 'INJURIES'))
```

#### More clean up and damage calculations

Doing a summary of the exponent data for crops and property damage, CROPDMGEXP and PROPMGEXP variables, we get:


```r
summary(workData$CROPDMGEXP)
```

```
##             0      2      ?      B      K      M      k      m 
## 618340     19      1      7      9 281832   1994     21      1
```

```r
summary(workData$PROPDMGEXP)
```

```
##             +      -      0      1      2      3      4      5      6 
## 465861      5      1    216     25     13      4      4     28      4 
##      7      8      ?      B      H      K      M      h      m 
##      5      1      8     40      6 424665  11330      1      7
```

The NOAA documentation states that the exponent values of the crop and property damage (CROPDMGEXP and PROPDMGEXP) are alpha and are: "K" for thousands, "M" for millions, "B" for billions, other values such as numbers and symbols are not defined.

Characters such as the sequence 0 through 8, "H", "+", "-", "?" and lower case m, k, h are not mentioned in the documentation.  This analysis in general takes a conservative approach toward the data, does NOT make assumptions and will ignore the observations for all of these undocumented exponent characters.

New fields for the crop and property damage exponents, CEXP and PEXP, will be set to the proper power of ten based on the valid exponent value.  An invalid exponent value will result in a value of NA which utlimately results in that observation being excluded from the results.


```r
#first fill the EXP variables with NAs
workData$CEXP <- NA
workData$PEXP <- NA

#then set the exponents to the proper power of ten based on the letters K, M, B

workData$CEXP <- ifelse(grepl('K', workData$CROPDMGEXP), 10^3, workData$CEXP)
workData$CEXP <- ifelse(grepl('M', workData$CROPDMGEXP), 10^6, workData$CEXP)
workData$CEXP <- ifelse(grepl('B', workData$CROPDMGEXP), 10^9, workData$CEXP)

workData$PEXP <- ifelse(grepl('K', workData$PROPDMGEXP), 10^3, workData$PEXP)
workData$PEXP <- ifelse(grepl('M', workData$PROPDMGEXP), 10^6, workData$PEXP)
workData$PEXP <- ifelse(grepl('B', workData$PROPDMGEXP), 10^9, workData$PEXP)

#calculate the damages to crops and property per event type observation
workData$CROPDMG <- workData$CROPDMG * workData$CEXP
workData$PROPDMG <- workData$PROPDMG * workData$PEXP

#calculate totals by event type
totals <- ddply(workData, ~ EVTYPE, summarize,
                CropDmgImpact=sum(CROPDMG, na.rm=TRUE),
                PropDmgImpact=sum(PROPDMG, na.rm=TRUE),
                TotalDmgImpact=sum(CROPDMG, PROPDMG, na.rm=TRUE),
                Fatalities=sum(FATALITIES, na.rm=TRUE),
                Injuries=sum(INJURIES, na.rm=TRUE),
                TotalHealthImpact=sum(FATALITIES, INJURIES, na.rm=TRUE))
```

### Results

#### Health Impact Reports

Top 20 events for fatalities, injuries and total health impact (fatalities + injuries) per event type.


```r
print(totals[order(-totals$Fatalities),c(1,5)][1:events, ], row.names=FALSE)
```

```
##                   EVTYPE Fatalities
##                  TORNADO       5633
##           EXCESSIVE HEAT       1903
##              FLASH FLOOD        978
##                     HEAT        937
##                LIGHTNING        816
##                TSTM WIND        504
##                    FLOOD        470
##              RIP CURRENT        368
##                HIGH WIND        248
##                AVALANCHE        224
##             WINTER STORM        206
##             RIP CURRENTS        204
##                HEAT WAVE        172
##             EXTREME COLD        160
##        THUNDERSTORM WIND        133
##               HEAVY SNOW        127
##  EXTREME COLD/WIND CHILL        125
##              STRONG WIND        103
##                 BLIZZARD        101
##                HIGH SURF        101
```

```r
print(totals[order(-totals$Injuries),c(1,6)][1:events, ], row.names=FALSE)
```

```
##              EVTYPE Injuries
##             TORNADO    91346
##           TSTM WIND     6957
##               FLOOD     6789
##      EXCESSIVE HEAT     6525
##           LIGHTNING     5230
##                HEAT     2100
##           ICE STORM     1975
##         FLASH FLOOD     1777
##   THUNDERSTORM WIND     1488
##                HAIL     1361
##        WINTER STORM     1321
##   HURRICANE/TYPHOON     1275
##           HIGH WIND     1137
##          HEAVY SNOW     1021
##            WILDFIRE      911
##  THUNDERSTORM WINDS      908
##            BLIZZARD      805
##                 FOG      734
##    WILD/FOREST FIRE      545
##          DUST STORM      440
```

```r
print(totals[order(-totals$TotalHealthImpact),c(1,7)][1:events, ], row.names=FALSE)
```

```
##              EVTYPE TotalHealthImpact
##             TORNADO             96979
##      EXCESSIVE HEAT              8428
##           TSTM WIND              7461
##               FLOOD              7259
##           LIGHTNING              6046
##                HEAT              3037
##         FLASH FLOOD              2755
##           ICE STORM              2064
##   THUNDERSTORM WIND              1621
##        WINTER STORM              1527
##           HIGH WIND              1385
##                HAIL              1376
##   HURRICANE/TYPHOON              1339
##          HEAVY SNOW              1148
##            WILDFIRE               986
##  THUNDERSTORM WINDS               972
##            BLIZZARD               906
##                 FOG               796
##         RIP CURRENT               600
##    WILD/FOREST FIRE               557
```


```r
#plotData <- data.frame()
#plotData[, 1] <- totals[order(-totals$Fatalities),c(1)][1:5]
#plotData[, 2] <- totals[order(-totals$Fatalities),c(5)][1:5]
#qplot(plotData[, 1], plotData[, 2], geom='bar', stat='identity')
plot1 <- 
    qplot(totals[order(-totals$Fatalities),c(1)] [1:5],
      totals[order(-totals$Fatalities),c(5)] [1:5]/1000, 
      main='Fatalities',
      geom='bar', 
      xlab="Event Type",
      ylab="Thousands of Fatalities",
      stat='identity')
plot1 <- plot1 + theme(axis.text.x = element_text(angle=90))

plot2 <- 
    qplot(totals[order(-totals$Injuries),c(1)] [1:5],
      totals[order(-totals$Injuries),c(6)] [1:5]/1000, 
      main='Injuries',
      geom='bar', 
      xlab="Event Type",
      ylab="Thousands of Injuries",
      stat='identity')
plot2 <- plot2 + theme(axis.text.x = element_text(angle=90))

plot3 <- 
    qplot(totals[order(-totals$TotalHealthImpact),c(1)] [1:5],
      totals[order(-totals$TotalHealthImpact),c(7)] [1:5]/1000, 
      main="Total Health Impact",
      geom='bar', 
      xlab="Event Type",
      ylab='Thousands of (Injuries + Fatalities)',
      stat='identity')
plot3 <- plot3 + theme(axis.text.x = element_text(angle=90))


#x <- totals[order(-totals$Fatalities),c(1)][1:5]

#y <- x[1:5]
#y
#x
#d <- totals[order(-totals$Fatalities),c(1)][1:5]
#str(d)
#d
grid.arrange(plot1, plot2, plot3, ncol=3)
```

![plot of chunk healthGraph](figure/healthGraph.png) 
We see from the above printed tables and graphs that the top threats for human fatalities are TORNADO, EXCESSIVE HEAT and FLASH FLOOD.  The top threats for human injuries are TORNADO, TSTM WIND and FLOOD.  The top combined (fatalities and injuries) health threats come from TORNADO, EXCESSIVE HEAT and TSTM WIND.

We notice that there are event types which are similar; e.g., Hurricane and Hurricane/Typhoon; Excessive Heat and Heat; and Flood, River Flood and Flash Flood.  Although it may seem logical, we have no evidence that these event types are actually the same and made the decision to not combine them. 

#### Damage Impact Reports
Top 20 events for crop damage, property damage and total damage (crop damage + property damage)

```r
print(totals[order(-totals$CropDmgImpact), c(1,2)][1:events, ], row.names=FALSE)
```

```
##             EVTYPE CropDmgImpact
##            DROUGHT     1.397e+10
##              FLOOD     5.662e+09
##        RIVER FLOOD     5.029e+09
##          ICE STORM     5.022e+09
##               HAIL     3.026e+09
##          HURRICANE     2.742e+09
##  HURRICANE/TYPHOON     2.608e+09
##        FLASH FLOOD     1.421e+09
##       EXTREME COLD     1.293e+09
##       FROST/FREEZE     1.094e+09
##         HEAVY RAIN     7.334e+08
##     TROPICAL STORM     6.783e+08
##          HIGH WIND     6.386e+08
##          TSTM WIND     5.540e+08
##     EXCESSIVE HEAT     4.924e+08
##             FREEZE     4.462e+08
##            TORNADO     4.150e+08
##  THUNDERSTORM WIND     4.148e+08
##               HEAT     4.015e+08
##           WILDFIRE     2.955e+08
```

```r
print(totals[order(-totals$PropDmgImpact), c(1,3)][1:events, ], row.names=FALSE)
```

```
##                     EVTYPE PropDmgImpact
##                      FLOOD     1.447e+11
##          HURRICANE/TYPHOON     6.931e+10
##                    TORNADO     5.693e+10
##                STORM SURGE     4.332e+10
##                FLASH FLOOD     1.614e+10
##                       HAIL     1.573e+10
##                  HURRICANE     1.187e+10
##             TROPICAL STORM     7.704e+09
##               WINTER STORM     6.688e+09
##                  HIGH WIND     5.270e+09
##                RIVER FLOOD     5.119e+09
##                   WILDFIRE     4.765e+09
##           STORM SURGE/TIDE     4.641e+09
##                  TSTM WIND     4.485e+09
##                  ICE STORM     3.945e+09
##          THUNDERSTORM WIND     3.483e+09
##             HURRICANE OPAL     3.153e+09
##           WILD/FOREST FIRE     3.002e+09
##  HEAVY RAIN/SEVERE WEATHER     2.500e+09
##         THUNDERSTORM WINDS     1.733e+09
```

```r
print(totals[order(-totals$TotalDmgImpact),c(1,4)][1:events, ], row.names=FALSE)
```

```
##                     EVTYPE TotalDmgImpact
##                      FLOOD      1.503e+11
##          HURRICANE/TYPHOON      7.191e+10
##                    TORNADO      5.734e+10
##                STORM SURGE      4.332e+10
##                       HAIL      1.875e+10
##                FLASH FLOOD      1.756e+10
##                    DROUGHT      1.502e+10
##                  HURRICANE      1.461e+10
##                RIVER FLOOD      1.015e+10
##                  ICE STORM      8.967e+09
##             TROPICAL STORM      8.382e+09
##               WINTER STORM      6.715e+09
##                  HIGH WIND      5.909e+09
##                   WILDFIRE      5.061e+09
##                  TSTM WIND      5.039e+09
##           STORM SURGE/TIDE      4.642e+09
##          THUNDERSTORM WIND      3.898e+09
##             HURRICANE OPAL      3.162e+09
##           WILD/FOREST FIRE      3.109e+09
##  HEAVY RAIN/SEVERE WEATHER      2.500e+09
```



We see from the above printed tables and graphs that the top threats for crop damage are DROUGHT, FLOOD and RIVER FLOOD.  The top threats for property damage are FLOOD, HURRICANE/TYPHOON and TORNADO.  The top combined (crop and property damage) threats come from FLOOD, HURRICANE/TYPHOON and TORNADO.

### Computing Environment

```r
sessionInfo()
```

```
## R version 3.0.2 (2013-09-25)
## Platform: x86_64-redhat-linux-gnu (64-bit)
## 
## locale:
## [1] C
## 
## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
## [1] gridExtra_0.9.1 ggplot2_1.0.0   plyr_1.8.1      knitr_1.6      
## 
## loaded via a namespace (and not attached):
##  [1] MASS_7.3-33      Rcpp_0.11.2      codetools_0.2-8  colorspace_1.2-4
##  [5] digest_0.6.4     evaluate_0.5.5   formatR_0.10     gtable_0.1.2    
##  [9] labeling_0.2     munsell_0.4.2    proto_0.3-10     reshape2_1.4    
## [13] scales_0.2.4     stringr_0.6.2    tools_3.0.2
```
