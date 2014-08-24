## Effects of Severe Weather on Property and Population in the US for the Time Period 1950 to November 2011

### Synopsis

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This analysis involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

The data was downloaded and read into a data.frame.  The data.frame was subsetted into a DF that contained the pertinent data.  Some minor cleanup was performed and the data then summarized and the top 10 event types for damage and casualties are listed and graphed for your convenience.

### Data Processing
#### Setup the needed libraries and environment.


```r
library(utils)
library(plyr)
opts_chunk$set(echo=TRUE, fig.width=6, fig.height=6)
```

Download the database along with the documentation files

#### Download data and documentation files

```r
blueFedoraDownload <- function (fileURL, destfile, method="auto", quiet=TRUE) {
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
blueFedoraDownload(url, dest)

#download storm data documentation
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf"
dest <- "pd0101600tcurr.pdf"
blueFedoraDownload(url, dest)

#download storm events FAQ
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf"
dest <- "StormEventsFAQ.pdf"
blueFedoraDownload(url, dest)
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

```r
#str(StormData)
#head(StormData)
```

The documentation refers to "Summary" EVTYPEs.  These are not "storms" as such and will be removed from the data.set.  Also only the pertinent columns, EVTYPE, CROPDMG, CROPDMGEXP, PROPDMG, PROPDMGEXP, FATALITIES and INJURIES will be included.


```r
workData <- subset(StormData, !grepl("Summary", StormData$EVTYPE), select=c('EVTYPE', 'CROPDMG', 'CROPDMGEXP', 'PROPDMG', 'PROPDMGEXP', 'FATALITIES', 'INJURIES'))
```

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

It may be a reasonable assumption that the sequence 0 thru 8 would be powers of ten and "H" is hundreds but they not mentioned in the documentation. Other characters such as "+", "-", "?" and lower case m, k, h are also not mentioned in the documentation.  This analyis in general takes a conservative approach toward the data, does NOT make assumptions and will ignore all of these undocumented exponent characters.

New fields for the crop and property damage exponents, CEXP and PEXP, will be set to the proper power of ten based on the valid exponent value.  An invalid exponent value will result in a value NA that results in that observation being excluded from the results.


```r
#first fill the EXP value with NAs
workData$CEXP <- NA
workData$PEXP <- NA

#then set the exponents to the proper power of ten based on the letters K, M, B

workData$CEXP <- ifelse(grepl('K', workData$CROPDMGEXP), 10^3, workData$CEXP)
workData$CEXP <- ifelse(grepl('M', workData$CROPDMGEXP), 10^6, workData$CEXP)
workData$CEXP <- ifelse(grepl('B', workData$CROPDMGEXP), 10^9, workData$CEXP)

workData$PEXP <- ifelse(grepl('K', workData$PROPDMGEXP), 10^3, workData$PEXP)
workData$PEXP <- ifelse(grepl('M', workData$PROPDMGEXP), 10^6, workData$PEXP)
workData$PEXP <- ifelse(grepl('B', workData$PROPDMGEXP), 10^9, workData$PEXP)
summary(workData)
```

```
##                EVTYPE          CROPDMG        CROPDMGEXP    
##  HAIL             :288661   Min.   :  0.0          :618340  
##  TSTM WIND        :219940   1st Qu.:  0.0   K      :281832  
##  THUNDERSTORM WIND: 82563   Median :  0.0   M      :  1994  
##  TORNADO          : 60652   Mean   :  1.5   k      :    21  
##  FLASH FLOOD      : 54277   3rd Qu.:  0.0   0      :    19  
##  FLOOD            : 25326   Max.   :990.0   B      :     9  
##  (Other)          :170805                   (Other):     9  
##     PROPDMG       PROPDMGEXP       FATALITIES     INJURIES     
##  Min.   :   0          :465861   Min.   :  0   Min.   :   0.0  
##  1st Qu.:   0   K      :424665   1st Qu.:  0   1st Qu.:   0.0  
##  Median :   0   M      : 11330   Median :  0   Median :   0.0  
##  Mean   :  12   0      :   216   Mean   :  0   Mean   :   0.2  
##  3rd Qu.:   0   B      :    40   3rd Qu.:  0   3rd Qu.:   0.0  
##  Max.   :5000   5      :    28   Max.   :583   Max.   :1700.0  
##                 (Other):    84                                 
##       CEXP               PEXP         
##  Min.   :1.00e+03   Min.   :1.00e+03  
##  1st Qu.:1.00e+03   1st Qu.:1.00e+03  
##  Median :1.00e+03   Median :1.00e+03  
##  Mean   :3.97e+04   Mean   :1.19e+05  
##  3rd Qu.:1.00e+03   3rd Qu.:1.00e+03  
##  Max.   :1.00e+09   Max.   :1.00e+09  
##  NA's   :618389     NA's   :466189
```

```r
#calculate the property damages to crops and property
workData$CROPDMG <- workData$CROPDMG * workData$CEXP
workData$PROPDMG <- workData$PROPDMG * workData$PEXP

totals <- ddply(workData, ~ EVTYPE, summarize,
                CropDmgImpact=sum(CROPDMG, na.rm=TRUE),
                PropDmgImpact=sum(PROPDMG, na.rm=TRUE),
                TotalDmgImpact=sum(CROPDMG, PROPDMG, na.rm=TRUE),
                Fatalities=sum(FATALITIES, na.rm=TRUE),
                Injuries=sum(INJURIES, na.rm=TRUE),
                TotalHealth=sum(FATALITIES, INJURIES, na.rm=TRUE))
summary(totals)
```

```
##                    EVTYPE    CropDmgImpact      PropDmgImpact     
##     HIGH SURF ADVISORY:  1   Min.   :0.00e+00   Min.   :0.00e+00  
##   COASTAL FLOOD       :  1   1st Qu.:0.00e+00   1st Qu.:0.00e+00  
##   FLASH FLOOD         :  1   Median :0.00e+00   Median :0.00e+00  
##   LIGHTNING           :  1   Mean   :5.33e+07   Mean   :4.64e+08  
##   TSTM WIND           :  1   3rd Qu.:0.00e+00   3rd Qu.:7.70e+04  
##   TSTM WIND (G45)     :  1   Max.   :1.40e+10   Max.   :1.45e+11  
##  (Other)              :915                                        
##  TotalDmgImpact       Fatalities      Injuries      TotalHealth   
##  Min.   :0.00e+00   Min.   :   0   Min.   :    0   Min.   :    0  
##  1st Qu.:0.00e+00   1st Qu.:   0   1st Qu.:    0   1st Qu.:    0  
##  Median :0.00e+00   Median :   0   Median :    0   Median :    0  
##  Mean   :5.17e+08   Mean   :  16   Mean   :  153   Mean   :  169  
##  3rd Qu.:1.05e+05   3rd Qu.:   0   3rd Qu.:    0   3rd Qu.:    0  
##  Max.   :1.50e+11   Max.   :5633   Max.   :91346   Max.   :96979  
## 
```

```r
#dam <- melt(workData, id.vars="EVTYPE", 
#                measure.vars=c("PROPDMG", "CROPDMG"))
#hea <- melt(workData, id.vars="EVTYPE",
#                measure.vars=c("FATALITIES", "INJURIES"))
#
#damage <- ddply(dam, ~ EVTYPE, summarize, value=sum(value, na.rm=TRUE))
#health <- ddply(hea, ~ EVTYPE, summarize, value=sum(value, na.rm=TRUE))
#
#summary(damage)
#summary(health)
```

### Results

* Health Impact Reports


```r
print("Top fifteen fatalities event types")
```

```
## [1] "Top fifteen fatalities event types"
```

```r
print(totals[order(-totals$Fatalities),c(1,5)][1:15, ], row.names=FALSE)
```

```
##             EVTYPE Fatalities
##            TORNADO       5633
##     EXCESSIVE HEAT       1903
##        FLASH FLOOD        978
##               HEAT        937
##          LIGHTNING        816
##          TSTM WIND        504
##              FLOOD        470
##        RIP CURRENT        368
##          HIGH WIND        248
##          AVALANCHE        224
##       WINTER STORM        206
##       RIP CURRENTS        204
##          HEAT WAVE        172
##       EXTREME COLD        160
##  THUNDERSTORM WIND        133
```

```r
print("Top fifteen injuries event types")
```

```
## [1] "Top fifteen injuries event types"
```

```r
print(totals[order(-totals$Injuries),c(1,6)][1:15, ], row.names=FALSE)
```

```
##             EVTYPE Injuries
##            TORNADO    91346
##          TSTM WIND     6957
##              FLOOD     6789
##     EXCESSIVE HEAT     6525
##          LIGHTNING     5230
##               HEAT     2100
##          ICE STORM     1975
##        FLASH FLOOD     1777
##  THUNDERSTORM WIND     1488
##               HAIL     1361
##       WINTER STORM     1321
##  HURRICANE/TYPHOON     1275
##          HIGH WIND     1137
##         HEAVY SNOW     1021
##           WILDFIRE      911
```

```r
print("Top fifteen total health impact event types")
```

```
## [1] "Top fifteen total health impact event types"
```

```r
print(totals[order(-totals$TotalHealth),c(1,7)][1:15, ], row.names=FALSE)
```

```
##             EVTYPE TotalHealth
##            TORNADO       96979
##     EXCESSIVE HEAT        8428
##          TSTM WIND        7461
##              FLOOD        7259
##          LIGHTNING        6046
##               HEAT        3037
##        FLASH FLOOD        2755
##          ICE STORM        2064
##  THUNDERSTORM WIND        1621
##       WINTER STORM        1527
##          HIGH WIND        1385
##               HAIL        1376
##  HURRICANE/TYPHOON        1339
##         HEAVY SNOW        1148
##           WILDFIRE         986
```

We see from the above printed table that the top threats for human fatalities are , and .  The top threats for human injuries are , and .  The top combined health threats come from , and .

We notice that there are event types which are similar; e.g., Hurricane and Hurrican/Typhoon and Flood, River Flood and Flash Flood.  We have no evidence that these event types are actually the same and made the decision to not combine them.

* Damage Impact Reports


```r
print("Top fifteen crop damage event types")
```

```
## [1] "Top fifteen crop damage event types"
```

```r
print(totals[order(-totals$CropDmgImpact),c(1,2)][1:15, ], row.names=FALSE)
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
```

```r
print("Top fifteen property damage event types")
```

```
## [1] "Top fifteen property damage event types"
```

```r
print(totals[order(-totals$PropDmgImpact), c(1,3)][1:15, ], row.names=FALSE)
```

```
##             EVTYPE PropDmgImpact
##              FLOOD     1.447e+11
##  HURRICANE/TYPHOON     6.931e+10
##            TORNADO     5.693e+10
##        STORM SURGE     4.332e+10
##        FLASH FLOOD     1.614e+10
##               HAIL     1.573e+10
##          HURRICANE     1.187e+10
##     TROPICAL STORM     7.704e+09
##       WINTER STORM     6.688e+09
##          HIGH WIND     5.270e+09
##        RIVER FLOOD     5.119e+09
##           WILDFIRE     4.765e+09
##   STORM SURGE/TIDE     4.641e+09
##          TSTM WIND     4.485e+09
##          ICE STORM     3.945e+09
```

```r
print("Top fifteen total damage (crop + property)")
```

```
## [1] "Top fifteen total damage (crop + property)"
```

```r
print(totals[order(-totals$TotalDmgImpact), c(1,4)][1:15, ], row.names=FALSE)
```

```
##             EVTYPE TotalDmgImpact
##              FLOOD      1.503e+11
##  HURRICANE/TYPHOON      7.191e+10
##            TORNADO      5.734e+10
##        STORM SURGE      4.332e+10
##               HAIL      1.875e+10
##        FLASH FLOOD      1.756e+10
##            DROUGHT      1.502e+10
##          HURRICANE      1.461e+10
##        RIVER FLOOD      1.015e+10
##          ICE STORM      8.967e+09
##     TROPICAL STORM      8.382e+09
##       WINTER STORM      6.715e+09
##          HIGH WIND      5.909e+09
##           WILDFIRE      5.061e+09
##          TSTM WIND      5.039e+09
```


### Environment

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
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] plyr_1.8.1 knitr_1.6 
## 
## loaded via a namespace (and not attached):
## [1] Rcpp_0.11.2    digest_0.6.4   evaluate_0.5.5 formatR_0.10  
## [5] stringr_0.6.2  tools_3.0.2
```
