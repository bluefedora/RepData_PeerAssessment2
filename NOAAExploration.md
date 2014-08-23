## NOAA Storm Database Exploration
### Synopsis


```r
library(utils)
library(reshape2)
library(plyr)
opts_chunk$set(echo=TRUE, fig.width=6, fig.height=6)
```

### Data Processing
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
str(StormData)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 4242 11116 2224 2224 2260 383 3980 3980 ...
##  $ BGN_TIME  : Factor w/ 3608 levels "000","0000","0001",..: 152 167 2645 1563 2524 3126 122 1563 3126 3126 ...
##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 6 6 6 6 6 6 6 6 6 6 ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 826 826 826 826 826 826 826 826 826 826 ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : Factor w/ 35 levels "","  N"," NW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_LOCATI: Factor w/ 54429 levels ""," Christiansburg",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_DATE  : Factor w/ 6663 levels "","1/1/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_TIME  : Factor w/ 3647 levels ""," 0900CST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_LOCATI: Factor w/ 34506 levels ""," CANTON"," TULIA",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","+","-","0",..: 16 16 16 16 16 16 16 16 16 16 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","0","2","?",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ WFO       : Factor w/ 542 levels ""," CI","$AC",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : Factor w/ 436781 levels "","\t","\t\t",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```

```r
head(StormData)
```

```
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO         0                                               0
## 2 TORNADO         0                                               0
## 3 TORNADO         0                                               0
## 4 TORNADO         0                                               0
## 5 TORNADO         0                                               0
## 6 TORNADO         0                                               0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA         0                      14.0   100 3   0          0
## 2         NA         0                       2.0   150 2   0          0
## 3         NA         0                       0.1   123 2   0          0
## 4         NA         0                       0.0   100 2   0          0
## 5         NA         0                       0.0   150 2   0          0
## 6         NA         0                       1.5   177 2   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
## 1       15    25.0          K       0                                    
## 2        0     2.5          K       0                                    
## 3        2    25.0          K       0                                    
## 4        2     2.5          K       0                                    
## 5        2     2.5          K       0                                    
## 6        6     2.5          K       0                                    
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051       8806              1
## 2     3042      8755          0          0              2
## 3     3340      8742          0          0              3
## 4     3458      8626          0          0              4
## 5     3412      8642          0          0              5
## 6     3450      8748          0          0              6
```

The documentation refers to "Summary" EVTYPEs.  These are not "storms" as such and will be removed from the data.set.  Also only the pertinent columns, EVTYPE, CROPDMG, CROPDMGEXP, PROPDMG, PROPDMGEXP, FATALITIES and INJURIES will included.


```r
workData <- subset(StormData, !grepl("Summary", StormData$EVTYPE), select=c('EVTYPE', 'CROPDMG', 'CROPDMGEXP', 'PROPDMG', 'PROPDMGEXP', 'FATALITIES', 'INJURIES'))
```

Doing a summary of the CROPDMGEXP and of PROPMGEXP variables we get:


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

The documentation states that the exponent values of the crop and property damage (CROPDMGEXP and PROPDMGEXP) are alpha and are: "K" for thousands, "M" for millions, "B" for billions, other values such as numbers and symbols are not defined.

It may be a reasonable assumption that the sequence 0 thru 8 would be powers of ten and "H" is hundreds but they not mentioned in the documentation. Other characters such as "+", "-", "?" and lower case m, k, h are also not mentioned in the documentation.  This analyis will NOT make assumptions and will ignore all of these undocumented exponent characters.

New fields for the crop and property damage exponents, CEXP and PEXP, will be set to the proper power of ten based on the valid exponent value.  An invalid exponent value will result in a value NA.


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
dam <- melt(workData, id.vars="EVTYPE", 
                measure.vars=c("PROPDMG", "CROPDMG"))
hea <- melt(workData, id.vars="EVTYPE",
                measure.vars=c("FATALITIES", "INJURIES"))

damage <- ddply(dam, ~ EVTYPE, summarize, value=sum(value, na.rm=TRUE))
health <- ddply(hea, ~ EVTYPE, summarize, value=sum(value, na.rm=TRUE))

summary(damage)
```

```
##                    EVTYPE        value         
##     HIGH SURF ADVISORY:  1   Min.   :0.00e+00  
##   COASTAL FLOOD       :  1   1st Qu.:0.00e+00  
##   FLASH FLOOD         :  1   Median :0.00e+00  
##   LIGHTNING           :  1   Mean   :5.17e+08  
##   TSTM WIND           :  1   3rd Qu.:1.05e+05  
##   TSTM WIND (G45)     :  1   Max.   :1.50e+11  
##  (Other)              :915
```

```r
summary(health)
```

```
##                    EVTYPE        value      
##     HIGH SURF ADVISORY:  1   Min.   :    0  
##   COASTAL FLOOD       :  1   1st Qu.:    0  
##   FLASH FLOOD         :  1   Median :    0  
##   LIGHTNING           :  1   Mean   :  169  
##   TSTM WIND           :  1   3rd Qu.:    0  
##   TSTM WIND (G45)     :  1   Max.   :96979  
##  (Other)              :915
```

### Results

* Damage Impact Reports


```r
print("Top ten crop damage event types")
```

```
## [1] "Top ten crop damage event types"
```

```r
print(totals[order(-totals$CropDmgImpact),c(1,2)][1:10, ], row.names=FALSE)
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
```

```r
print("Top ten property damage event types")
```

```
## [1] "Top ten property damage event types"
```

```r
print(totals[order(-totals$PropDmgImpact), c(1,3)][1:10, ], row.names=FALSE)
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
```

```r
print("Top ten total damage (crop + property)")
```

```
## [1] "Top ten total damage (crop + property)"
```

```r
print(totals[order(-totals$TotalDmgImpact), c(1,4)][1:10, ], row.names=FALSE)
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
```

* Health Impact Reports


```r
print("Top ten fatalities event types")
```

```
## [1] "Top ten fatalities event types"
```

```r
print(totals[order(-totals$Fatalities),c(1,5)][1:10, ], row.names=FALSE)
```

```
##          EVTYPE Fatalities
##         TORNADO       5633
##  EXCESSIVE HEAT       1903
##     FLASH FLOOD        978
##            HEAT        937
##       LIGHTNING        816
##       TSTM WIND        504
##           FLOOD        470
##     RIP CURRENT        368
##       HIGH WIND        248
##       AVALANCHE        224
```

```r
print("Top ten injuries event types")
```

```
## [1] "Top ten injuries event types"
```

```r
print(totals[order(-totals$Injuries),c(1,6)][1:10, ], row.names=FALSE)
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
```

```r
print("Top ten total health impact event types")
```

```
## [1] "Top ten total health impact event types"
```

```r
print(totals[order(-totals$TotalHealth),c(1,7)][1:10, ], row.names=FALSE)
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
## [1] plyr_1.8.1   reshape2_1.4 knitr_1.6   
## 
## loaded via a namespace (and not attached):
## [1] Rcpp_0.11.2    digest_0.6.4   evaluate_0.5.5 formatR_0.10  
## [5] stringr_0.6.2  tools_3.0.2
```
