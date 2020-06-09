Mean ceramic dates for Caddo sites in the Neches River Basin
================
Robert Z. Selden, Jr.
June 8, 2020

## MCD for NRB

Sites selected for this study come from the Caddo Ceramic Vessel
Database (Perttula 2017), which were plotted using coordinates from the
Texas Historic Sites Atlas, then clipped to the extent of the Neches
River Basin. Mean ceramic dates were calculated and plotted using
modified code originally developed by [Peeples
(2011)](http://www.mattpeeples.net/mcd.html), who was following South
(1972, 1977).

``` r
# set working directory
setwd(getwd())

#read data
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
colnames(output) <- c('site','mcd','lower','higher')
output
```

    ##         site      mcd                lower              higher            
    ## 41SM56  "41SM56"  "1116.66666666667" "1090.26279525655" "1149.87325916522"
    ## 41SM55  "41SM55"  "1250"             "1250"             "1250"            
    ## 41SM77  "41SM77"  "1548.75"          "1518.96551596818" "1575.93244321549"
    ## 41SM90  "41SM90"  "1530.625"         "1525.63170120378" "1535.46523757174"
    ## 41SM73  "41SM73"  "1206.25"          "989.465399333755" "1418.69786597237"
    ## 41SM93  "41SM93"  "1538.5"           "1533.44457585296" "1543.37175067765"
    ## 41SM151 "41SM151" "1540"             "1540"             "1540"            
    ## 41SM158 "41SM158" "1532.5"           "1522.82428110857" "1543.09408623837"
    ## 41SM249 "41SM249" "1535"             "1526.80446459889" "1542.7873721358" 
    ## 41SM150 "41SM150" "1346.66666666667" "1231.93793420178" "1455.47703178461"
    ## 41SM355 "41SM355" "1540"             "1540"             "1540"            
    ## 41SM2   "41SM2"   "1525"             "1525"             "1525"            
    ## 41HE114 "41HE114" "1535.26315789474" "1531.21591109523" "1539.76153251379"
    ## 41HE4   "41HE4"   "1532.5"           "1524.85150266412" "1539.8423748869" 
    ## 41HE75  "41HE75"  "1531.66666666667" "1526.90373502858" "1535.8173534068" 
    ## 41HE78  "41HE78"  "1525"             "1525"             "1525"            
    ## 41HE82  "41HE82"  "1540"             "1540"             "1540"            
    ## 41HE7   "41HE7"   "1528.75"          "1522.20760527371" "1535.90463962425"
    ## 41CE4   "41CE4"   "1532.14285714286" "1528.25338199213" "1536.14895036939"
    ## 41CE12  "41CE12"  "1589.13461538462" "1567.28454277304" "1615.98076334941"
    ## 41CE14  "41CE14"  "1528.57142857143" "1527.35818756248" "1529.64472788941"
    ## 41CE15  "41CE15"  "1601.66666666667" "1501.48685300982" "1689.94171841875"
    ## 41CE17  "41CE17"  "1530.19230769231" "1527.80910039955" "1532.38713193954"
    ## 41CE19  "41CE19"  "1076.27272727273" "1058.3746612928"  "1097.02515317845"
    ## 41CE23  "41CE23"  "1535"             "1530.45714285714" "1540.25714285714"
    ## 41CE25  "41CE25"  "1547"             "1524.79306826261" "1565.71305418637"
    ## 41CE421 "41CE421" "1525"             "1525"             "1525"            
    ## 41CE6   "41CE6"   "1645.83333333333" "1560.97053288181" "1716.13831065561"
    ## 41AN1   "41AN1"   "1532.75862068966" "1530.59274831005" "1535.22005957173"
    ## 41AN13  "41AN13"  "1692.69230769231" "1648.60434376103" "1734.89644116833"
    ## 41AN14  "41AN14"  "1527.72727272727" "1524.55108854532" "1530.95911553632"
    ## 41AN16  "41AN16"  "1525"             "1525"             "1525"            
    ## 41AN18  "41AN18"  "1532.5"           "1521.32610622983" "1543.36777132119"
    ## 41AN184 "41AN184" "1596.66666666667" "1465.96247250075" "1718.5953506285" 
    ## 41AN2   "41AN2"   "1555.625"         "1522.8696265102"  "1584.97476124491"
    ## 41AN21  "41AN21"  "1525"             "1525"             "1525"            
    ## 41AN26  "41AN26"  "1716.11111111111" "1693.53517123156" "1739.66210768"   
    ## 41AN32  "41AN32"  "1538.75"          "1518.7970709357"  "1561.59068416634"
    ## 41AN34  "41AN34"  "1540.27777777778" "1510.61094553919" "1567.79041500503"
    ## 41AN38  "41AN38"  "1528.75"          "1525.41547908687" "1532.39064336211"
    ## 41AN39  "41AN39"  "1529.28571428571" "1524.42694028901" "1533.88209761187"
    ## 41AN44  "41AN44"  "1535.90909090909" "1532.44087669438" "1538.98769473419"
    ## 41AN48  "41AN48"  "1532.5"           "1524.08261937322" "1543.06023776964"
    ## 41AN53  "41AN53"  "1525"             "1525"             "1525"            
    ## 41AN54  "41AN54"  "1545.71428571429" "1515.65524328637" "1568.60131648039"
    ## 41AN56  "41AN56"  "1540"             "1540"             "1540"            
    ## 41AN57  "41AN57"  "1525"             "1525"             "1525"            
    ## 41AN67  "41AN67"  "1525"             "1525"             "1525"            
    ## 41AG66  "41AG66"  "1250"             "1250"             "1250"

``` r
write.table(output, file = 'mcd_out.csv', sep = ',', row.names = F)
```

## Plot MCD results with error bars

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
caddo <- read.csv("mcd_out.csv", header = TRUE, as.is = TRUE)

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
caddo$site <- factor(caddo$site, levels = caddo$site[order(caddo$mcd)])
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
lower <- caddo$lower
upper <- caddo$higher

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

## References

<div id="refs" class="references">

<div id="ref-RN11605">

Perttula, Timothy K. 2017. “Caddo Ceramic Vessel Database from Sites in
Texas, Louisiana, Oklahoma, and Arkansas.” *Journal of Northeast Texas
Archaeology* 71: 147–54.

</div>

<div id="ref-RN21010">

South, Stanley A. 1972. “Evolution and Horizon as Revealed in Ceramic
Analysis in Historical Archaeology.” *Conference on Historic Site
Archaeology Papers* 6 (2): 71–116.

</div>

<div id="ref-RN21011">

———. 1977. *Method and Theory in Historical Archaeology*. New York:
Academic Press.

</div>

</div>
