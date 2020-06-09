Mean ceramic dates for Caddo sites in the Neches River Basin
================
Robert Z. Selden, Jr.
June 8, 2020

## Including all sites and vessels

The code chunk below was developed by Matthew A. Peeples, following
Keith Kintigh, who was channelling Stanley South (1977).

``` r
# set working directory
setwd(getwd())
mydata <- read.csv(file = 'nrb_mcd_data.csv', sep = ',', header = T, row.names = 1)
types <- as.matrix(colnames(mydata))

dates <- read.csv(file = 'nrb_type_dates.csv', sep = ',', header = T)
bc <- min(dates[,2:3])
dates[,2:3] <- dates[,2:3]+(bc*-1)

midpt <- as.matrix(((dates[,3]-dates[,2])/2)+dates[,2])
dates <- cbind(dates,midpt)

mydata2 <- mydata


mcd.calc <- function(x,types,dates) {
tot <- as.matrix(rowSums(x))
for (i in 1:nrow(types)) {
for (j in 1:nrow(dates)) {
if (types[i,] == dates[j,1]) 
{x[,i] <- x[,i] * dates[j,4]}}}

mcd <- matrix(0,nrow(mydata),1)
rownames(mcd) <- rownames(mydata)

newtot <- as.matrix(rowSums(x))

for (i in 1:nrow(mcd)) {
mcd[i,1] <- newtot[i,]/tot[i,]+bc}
return(mcd)}

mcd <- mcd.calc(mydata2,types,dates)

###############################################################################

nsim <- 1000

data.rowsum <- as.matrix(rowSums(mydata))
range <- matrix(0,nrow(mydata),2)

for (i in 1:nrow(mydata)) {
data.sim <- rmultinom(nsim,data.rowsum[i,],prob=mydata[i,])
data.sim <- t(data.sim)
temp <- mcd.calc(data.sim,types,dates)
range[i,1] <- mean(temp) - (sd(temp)*1.96)
range[i,2] <- mean(temp) + (sd(temp)*1.96)}

output <- cbind(row.names(mydata),mcd,range)
colnames(output) <- c('site','mcd','lower','higher') #modified from Peeples' code
output
```

    ##         site      mcd                lower              higher            
    ## 41SM56  "41SM56"  "1116.66666666667" "1085.78871676054" "1140.74189548436"
    ## 41SM55  "41SM55"  "1250"             "1250"             "1250"            
    ## 41SM77  "41SM77"  "1548.75"          "1516.57453594678" "1580.39485180832"
    ## 41SM90  "41SM90"  "1530.625"         "1525.41378369063" "1536.0658081461" 
    ## 41SM73  "41SM73"  "1206.25"          "1025.90020966189" "1386.59979033811"
    ## 41SM93  "41SM93"  "1538.5"           "1533.14382054866" "1543.30515904317"
    ## 41SM151 "41SM151" "1540"             "1540"             "1540"            
    ## 41SM158 "41SM158" "1532.5"           "1523.23442884379" "1542.99006095213"
    ## 41SM249 "41SM249" "1535"             "1526.55789676019" "1543.0339399745" 
    ## 41SM150 "41SM150" "1346.66666666667" "1249.80189692899" "1459.31374932951"
    ## 41SM355 "41SM355" "1540"             "1540"             "1540"            
    ## 41SM2   "41SM2"   "1525"             "1525"             "1525"            
    ## 41HE114 "41HE114" "1535.26315789474" "1530.1684303265"  "1540.03565130616"
    ## 41HE4   "41HE4"   "1532.5"           "1524.20075584188" "1537.89108089281"
    ## 41HE75  "41HE75"  "1531.66666666667" "1526.52427758174" "1536.26483806451"
    ## 41HE78  "41HE78"  "1525"             "1525"             "1525"            
    ## 41HE82  "41HE82"  "1540"             "1540"             "1540"            
    ## 41HE7   "41HE7"   "1528.75"          "1522.04039867056" "1535.45960132944"
    ## 41CE4   "41CE4"   "1532.14285714286" "1528.53204930744" "1536.27844631938"
    ## 41CE12  "41CE12"  "1589.13461538462" "1561.81425788058" "1614.34743756683"
    ## 41CE14  "41CE14"  "1528.57142857143" "1527.3988132445"  "1529.80818384005"
    ## 41CE15  "41CE15"  "1601.66666666667" "1494.71008769425" "1691.27630686357"
    ## 41CE17  "41CE17"  "1530.19230769231" "1527.44517494893" "1532.84524891292"
    ## 41CE19  "41CE19"  "1076.27272727273" "1059.69348971694" "1095.25548987489"
    ## 41CE23  "41CE23"  "1535"             "1530.01071294001" "1540.90765440693"
    ## 41CE25  "41CE25"  "1547"             "1527.50643062261" "1568.90173264269"
    ## 41CE421 "41CE421" "1525"             "1525"             "1525"            
    ## 41CE6   "41CE6"   "1645.83333333333" "1564.01537641042" "1717.75333107258"
    ## 41AN1   "41AN1"   "1532.75862068966" "1530.19404331498" "1535.00651966883"
    ## 41AN13  "41AN13"  "1692.69230769231" "1643.08870778409" "1738.19857636034"
    ## 41AN14  "41AN14"  "1527.72727272727" "1524.04010714414" "1531.08048654789"
    ## 41AN16  "41AN16"  "1525"             "1525"             "1525"            
    ## 41AN18  "41AN18"  "1532.5"           "1522.87265628656" "1542.7395886114" 
    ## 41AN184 "41AN184" "1596.66666666667" "1481.42225204584" "1723.6117615596" 
    ## 41AN2   "41AN2"   "1555.625"         "1523.54055891199" "1591.10229823087"
    ## 41AN21  "41AN21"  "1525"             "1525"             "1525"            
    ## 41AN26  "41AN26"  "1716.11111111111" "1686.68449212624" "1741.96252223506"
    ## 41AN32  "41AN32"  "1538.75"          "1521.7446702128"  "1556.87777876679"
    ## 41AN34  "41AN34"  "1540.27777777778" "1515.16104983725" "1560.5192222716" 
    ## 41AN38  "41AN38"  "1528.75"          "1525.4753641722"  "1531.10626848086"
    ## 41AN39  "41AN39"  "1529.28571428571" "1524.15425121184" "1533.54254179107"
    ## 41AN44  "41AN44"  "1535.90909090909" "1531.61328284283" "1540.20489897535"
    ## 41AN48  "41AN48"  "1532.5"           "1522.54805295432" "1542.45194704568"
    ## 41AN53  "41AN53"  "1525"             "1525"             "1525"            
    ## 41AN54  "41AN54"  "1545.71428571429" "1517.97060238868" "1574.11394571627"
    ## 41AN56  "41AN56"  "1540"             "1540"             "1540"            
    ## 41AN57  "41AN57"  "1525"             "1525"             "1525"            
    ## 41AN67  "41AN67"  "1525"             "1525"             "1525"            
    ## 41AG66  "41AG66"  "1250"             "1250"             "1250"

``` r
write.table(output,file = 'mcd_out.csv', sep = ',', row.names = F)
# end of Peeples' code
```

## Plot results (mcd\_out) with error bars

I prefer graphs over tables where possible (and appropriate), and I
think it appropriate in this instance.

``` r
# plot mean ceramic dates with error bars

#install.packages("devtools")
#devtools::install_github("tidyverse/ggplot2")
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.1     v purrr   0.3.4
    ## v tibble  3.0.1     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# read mcd_out
caddo<-read.csv("mcd_out.csv",header = TRUE, as.is=TRUE)

# reorder sites by mcd
caddo$site
```

    ##  [1] "41SM56"  "41SM55"  "41SM77"  "41SM90"  "41SM73"  "41SM93"  "41SM151"
    ##  [8] "41SM158" "41SM249" "41SM150" "41SM355" "41SM2"   "41HE114" "41HE4"  
    ## [15] "41HE75"  "41HE78"  "41HE82"  "41HE7"   "41CE4"   "41CE12"  "41CE14" 
    ## [22] "41CE15"  "41CE17"  "41CE19"  "41CE23"  "41CE25"  "41CE421" "41CE6"  
    ## [29] "41AN1"   "41AN13"  "41AN14"  "41AN16"  "41AN18"  "41AN184" "41AN2"  
    ## [36] "41AN21"  "41AN26"  "41AN32"  "41AN34"  "41AN38"  "41AN39"  "41AN44" 
    ## [43] "41AN48"  "41AN53"  "41AN54"  "41AN56"  "41AN57"  "41AN67"  "41AG66"

``` r
caddo$site <- factor(caddo$site,levels = caddo$site[order(caddo$mcd)])
caddo$site
```

    ##  [1] 41SM56  41SM55  41SM77  41SM90  41SM73  41SM93  41SM151 41SM158 41SM249
    ## [10] 41SM150 41SM355 41SM2   41HE114 41HE4   41HE75  41HE78  41HE82  41HE7  
    ## [19] 41CE4   41CE12  41CE14  41CE15  41CE17  41CE19  41CE23  41CE25  41CE421
    ## [28] 41CE6   41AN1   41AN13  41AN14  41AN16  41AN18  41AN184 41AN2   41AN21 
    ## [37] 41AN26  41AN32  41AN34  41AN38  41AN39  41AN44  41AN48  41AN53  41AN54 
    ## [46] 41AN56  41AN57  41AN67  41AG66 
    ## 49 Levels: 41CE19 41SM56 41SM73 41SM55 41AG66 41SM150 41SM2 41HE78 ... 41AN26

``` r
# define variables
Site <- caddo$site
Mean_Ceramic_Date_CE <- caddo$mcd
lower<-caddo$lower
upper<-caddo$higher

# plot
cs = theme(
  axis.title.x = element_text(size = 11),
  axis.text.x = element_text(size = 8),
  axis.title.y = element_text(size = 11),
  axis.text.y = element_text(size = 6))

ggplot() +
  geom_errorbar(data = caddo, mapping = aes(x = Site, ymin = lower, ymax = upper), width = 0.2, size = 1, colour = "springgreen4") +
  geom_point(data = caddo, mapping = aes(x = Site, y = Mean_Ceramic_Date_CE), size = 2, shape = 21, fill = "white") +
  coord_flip() + # provides a representation of time similar to what archaeologists are used to seeing
  cs
```

<img src="mcd_files/figure-gfm/ggplot-1.png" width="100%" />

``` r
# end of code
```
