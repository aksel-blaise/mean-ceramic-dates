Mean ceramic dates for Caddo sites in the Neches River Basin
================
Robert Z. Selden, Jr.
June 8, 2020

## Including all sites and vessels

The code chunk below was developed by Matthew A. Peeples, following
Keith Kintigh, who was channelling Stanley South (1977).

``` r
# this code chunk was repurposed from Matthew A. Peeples excellent example (link above)
# set working directory
setwd(getwd())
mydata <- read.csv(file='nrb_mcd_data.csv',sep=',',header=T,row.names=1)
types <- as.matrix(colnames(mydata))

dates <- read.csv(file='nrb_type_dates.csv',sep=',',header=T)
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
    ## 41SM56  "41SM56"  "1116.66666666667" "1092.60572071006" "1137.32625207906"
    ## 41SM55  "41SM55"  "1250"             "1250"             "1250"            
    ## 41SM77  "41SM77"  "1548.75"          "1515.79686290459" "1581.48885138112"
    ## 41SM90  "41SM90"  "1530.625"         "1525.90057855822" "1535.50248266626"
    ## 41SM73  "41SM73"  "1206.25"          "1037.92531976049" "1417.94202717829"
    ## 41SM93  "41SM93"  "1538.5"           "1533.86899326854" "1542.5799863233" 
    ## 41SM151 "41SM151" "1540"             "1540"             "1540"            
    ## 41SM158 "41SM158" "1532.5"           "1521.32610622983" "1543.36777132119"
    ## 41SM249 "41SM249" "1535"             "1528.11722667654" "1541.67869169081"
    ## 41SM150 "41SM150" "1346.66666666667" "1234.50474273792" "1484.47484909881"
    ## 41SM355 "41SM355" "1540"             "1540"             "1540"            
    ## 41SM2   "41SM2"   "1525"             "1525"             "1525"            
    ## 41HE114 "41HE114" "1535.26315789474" "1531.68163403791" "1539.65026714362"
    ## 41HE4   "41HE4"   "1532.5"           "1523.48383332149" "1542.28147280096"
    ## 41HE75  "41HE75"  "1531.66666666667" "1527.41730808645" "1536.52827014485"
    ## 41HE78  "41HE78"  "1525"             "1525"             "1525"            
    ## 41HE82  "41HE82"  "1540"             "1540"             "1540"            
    ## 41HE7   "41HE7"   "1528.75"          "1522.03445055711" "1535.77167189187"
    ## 41CE4   "41CE4"   "1532.14285714286" "1529.19549031812" "1535.7899324224" 
    ## 41CE12  "41CE12"  "1589.13461538462" "1565.2671671028"  "1614.69751107617"
    ## 41CE14  "41CE14"  "1528.57142857143" "1527.33625511998" "1530.03400727069"
    ## 41CE15  "41CE15"  "1601.66666666667" "1489.13568691769" "1705.42213621156"
    ## 41CE17  "41CE17"  "1530.19230769231" "1527.09437044098" "1533.50217586985"
    ## 41CE19  "41CE19"  "1076.27272727273" "1054.11393205779" "1094.87957443572"
    ## 41CE23  "41CE23"  "1535"             "1530.21907929911" "1539.47479825191"
    ## 41CE25  "41CE25"  "1547"             "1526.8407403024"  "1569.63272908535"
    ## 41CE421 "41CE421" "1525"             "1525"             "1525"            
    ## 41CE6   "41CE6"   "1645.83333333333" "1570.13545244097" "1735.85094211685"
    ## 41AN1   "41AN1"   "1532.75862068966" "1530.02933429185" "1535.3190119432" 
    ## 41AN13  "41AN13"  "1692.69230769231" "1649.58611593453" "1735.50022629467"
    ## 41AN14  "41AN14"  "1527.72727272727" "1525.02280931398" "1530.09778437804"
    ## 41AN16  "41AN16"  "1525"             "1525"             "1525"            
    ## 41AN18  "41AN18"  "1532.5"           "1520.84347825133" "1544.46264419765"
    ## 41AN184 "41AN184" "1596.66666666667" "1462.47471495544" "1725.00827824184"
    ## 41AN2   "41AN2"   "1555.625"         "1520.41461382768" "1587.82518209069"
    ## 41AN21  "41AN21"  "1525"             "1525"             "1525"            
    ## 41AN26  "41AN26"  "1716.11111111111" "1690.43769269852" "1738.53434055923"
    ## 41AN32  "41AN32"  "1538.75"          "1520.29695995973" "1562.78467269333"
    ## 41AN34  "41AN34"  "1540.27777777778" "1516.4795031365"  "1562.72684606985"
    ## 41AN38  "41AN38"  "1528.75"          "1524.15548478683" "1532.73227031522"
    ## 41AN39  "41AN39"  "1529.28571428571" "1524.19804862819" "1534.02352571584"
    ## 41AN44  "41AN44"  "1535.90909090909" "1532.32408894628" "1539.88370326151"
    ## 41AN48  "41AN48"  "1532.5"           "1522.96790364588" "1540.8076065582" 
    ## 41AN53  "41AN53"  "1525"             "1525"             "1525"            
    ## 41AN54  "41AN54"  "1545.71428571429" "1520.92262750682" "1569.83538998589"
    ## 41AN56  "41AN56"  "1540"             "1540"             "1540"            
    ## 41AN57  "41AN57"  "1525"             "1525"             "1525"            
    ## 41AN67  "41AN67"  "1525"             "1525"             "1525"            
    ## 41AG66  "41AG66"  "1250"             "1250"             "1250"

``` r
write.table(output,file='mcd_out.csv',sep=',',row.names=F)
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

    ## -- Attaching packages -------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.1     v purrr   0.3.4
    ## v tibble  3.0.1     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ----------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# read mcd_out
caddo<-read.csv("mcd_out.csv",header = TRUE, as.is=TRUE)
# define variables
Site<-caddo$site
Mean_Ceramic_Date<-caddo$mcd
lower<-caddo$lower
upper<-caddo$higher
# plot
cs = theme(
  axis.title.x = element_text(size = 14),
  axis.text.x = element_text(size = 8),
  axis.title.y = element_text(size = 14),
  axis.text.y = element_text(size = 6))

ggplot() +
  geom_errorbar(data=caddo, mapping=aes(x=Site, ymin=lower, ymax=upper), width=0.5, size=0.5, colour = "springgreen4") +
  geom_point(data=caddo, mapping=aes(x=Site, y=Mean_Ceramic_Date), size=2, shape=21, fill = "white") +
  coord_flip() + # provides a representation of time similar to what archaeologists are used to seeing
  cs
```

<img src="mcd_files/figure-gfm/ggplot-1.png" width="100%" />

## Limit to those sites with five or more vessels

``` r
# this code chunk was repurposed from Matthew A. Peeples excellent example (link above)
# set working directory
setwd(getwd())
mydata <- read.csv(file='nrb2_mcd_data.csv',sep=',',header=T,row.names=1)
types <- as.matrix(colnames(mydata))

dates <- read.csv(file='nrb2_type_dates.csv',sep=',',header=T)
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
    ## 41SM77  "41SM77"  "1548.75"          "1521.25227191736" "1571.93522808264"
    ## 41SM90  "41SM90"  "1530.625"         "1523.7147604585"  "1536.4414895415" 
    ## 41SM93  "41SM93"  "1538.5"           "1533.00030435665" "1543.49969564335"
    ## 41SM150 "41SM150" "1346.66666666667" "1242.80230844153" "1458.58658044736"
    ## 41HE114 "41HE114" "1535.26315789474" "1532.44201971798" "1539.79482238729"
    ## 41HE75  "41HE75"  "1531.66666666667" "1526.62555250903" "1536.98555860208"
    ## 41CE4   "41CE4"   "1532.14285714286" "1527.97546013158" "1536.4888255827" 
    ## 41CE12  "41CE12"  "1589.13461538462" "1564.05847746356" "1617.231586639"  
    ## 41CE14  "41CE14"  "1528.57142857143" "1527.54540051797" "1530.20459948203"
    ## 41CE17  "41CE17"  "1530.19230769231" "1527.85098499247" "1532.38939962292"
    ## 41CE19  "41CE19"  "985.818181818182" "947.607389553501" "1015.63503468892"
    ## 41CE23  "41CE23"  "1535"             "1528.36431072861" "1542.88568927139"
    ## 41CE25  "41CE25"  "1547"             "1529.41153299421" "1561.83846700579"
    ## 41CE6   "41CE6"   "1645.83333333333" "1565.21279461267" "1742.14831649844"
    ## 41AN1   "41AN1"   "1532.75862068966" "1529.3229183838"  "1536.53915058172"
    ## 41AN13  "41AN13"  "1692.69230769231" "1636.69217182105" "1744.36552048665"
    ## 41AN14  "41AN14"  "1527.72727272727" "1524.13138725545" "1531.43679456273"
    ## 41AN2   "41AN2"   "1555.625"         "1527.91856361741" "1583.48768638259"
    ## 41AN26  "41AN26"  "1716.11111111111" "1689.71615051246" "1739.85175072211"
    ## 41AN32  "41AN32"  "1538.75"          "1517.06435479235" "1557.81064520765"
    ## 41AN34  "41AN34"  "1540.27777777778" "1516.66913997114" "1564.16419336219"
    ## 41AN38  "41AN38"  "1528.75"          "1525.22753709396" "1532.68912957271"
    ## 41AN44  "41AN44"  "1535.90909090909" "1532.1057382328"  "1539.48517085811"
    ## 41AN54  "41AN54"  "1545.71428571429" "1510.31928992386" "1580.57356721899"

``` r
write.table(output,file='mcd_out2.csv',sep=',',row.names=F)
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
# read mcd_out
caddo2<-read.csv("mcd_out2.csv",header = TRUE, as.is=TRUE)
# define variables
Site<-caddo2$site
Mean_Ceramic_Date<-caddo2$mcd
lower<-caddo2$lower
upper<-caddo2$higher
# plot
cs = theme(
  axis.title.x = element_text(size = 14),
  axis.text.x = element_text(size = 8),
  axis.title.y = element_text(size = 14),
  axis.text.y = element_text(size = 6))

ggplot() +
  geom_errorbar(data=caddo2, mapping=aes(x=Site, ymin=lower, ymax=upper), width=0.5, size=0.5, colour = "springgreen4") +
  geom_point(data=caddo2, mapping=aes(x=Site, y=Mean_Ceramic_Date), size=2, shape=21, fill = "white") +
  coord_flip() + # provides a representation of time similar to what archaeologists are used to seeing
  cs
```

<img src="mcd_files/figure-gfm/ggplot2-1.png" width="100%" />

``` r
# end of code
```
