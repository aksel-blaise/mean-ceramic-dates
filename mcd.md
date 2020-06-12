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

write.table(output, file = 'mcd_out.csv', sep = ',', row.names = F)
```

## Plot MCD results with error bars

``` r
# plot mean ceramic dates with error bars

#install.packages("devtools")
#devtools::install_github("tidyverse/ggplot2")
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.1     v purrr   0.3.4
    ## v tibble  3.0.1     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts -------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# read mcd_out
caddo <- read.csv("mcd_out.csv", header = TRUE, as.is = TRUE)
caddo
```

    ##       site      mcd    lower   higher
    ## 1   41SM56 1116.667 1089.929 1144.084
    ## 2   41SM55 1250.000 1250.000 1250.000
    ## 3   41SM77 1548.750 1520.574 1575.548
    ## 4   41SM90 1530.625 1525.270 1535.138
    ## 5   41SM73 1206.250 1030.429 1399.417
    ## 6   41SM93 1538.500 1534.205 1542.734
    ## 7  41SM151 1540.000 1540.000 1540.000
    ## 8  41SM158 1532.500 1522.389 1543.529
    ## 9  41SM249 1535.000 1525.224 1544.163
    ## 10 41SM150 1346.667 1240.696 1444.746
    ## 11 41SM355 1540.000 1540.000 1540.000
    ## 12   41SM2 1525.000 1525.000 1525.000
    ## 13 41HE114 1535.263 1531.236 1539.967
    ## 14   41HE4 1532.500 1525.863 1540.362
    ## 15  41HE75 1531.667 1526.907 1535.679
    ## 16  41HE78 1525.000 1525.000 1525.000
    ## 17  41HE82 1540.000 1540.000 1540.000
    ## 18   41HE7 1528.750 1522.473 1534.414
    ## 19   41CE4 1532.143 1529.095 1535.249
    ## 20  41CE12 1589.135 1567.567 1619.180
    ## 21  41CE14 1528.571 1527.355 1529.957
    ## 22  41CE15 1601.667 1484.473 1699.813
    ## 23  41CE17 1530.192 1527.423 1532.962
    ## 24  41CE19 1076.273 1056.572 1094.126
    ## 25  41CE23 1535.000 1529.614 1539.977
    ## 26  41CE25 1547.000 1525.004 1569.315
    ## 27 41CE421 1525.000 1525.000 1525.000
    ## 28   41CE6 1645.833 1573.804 1723.509
    ## 29   41AN1 1532.759 1530.276 1535.325
    ## 30  41AN13 1692.692 1648.367 1741.413
    ## 31  41AN14 1527.727 1524.185 1531.214
    ## 32  41AN16 1525.000 1525.000 1525.000
    ## 33  41AN18 1532.500 1522.837 1541.245
    ## 34 41AN184 1596.667 1466.424 1700.583
    ## 35   41AN2 1555.625 1521.518 1594.196
    ## 36  41AN21 1525.000 1525.000 1525.000
    ## 37  41AN26 1716.111 1692.359 1743.438
    ## 38  41AN32 1538.750 1516.067 1560.586
    ## 39  41AN34 1540.278 1521.801 1569.038
    ## 40  41AN38 1528.750 1525.392 1532.465
    ## 41  41AN39 1529.286 1525.231 1534.390
    ## 42  41AN44 1535.909 1531.340 1539.142
    ## 43  41AN48 1532.500 1522.611 1540.858
    ## 44  41AN53 1525.000 1525.000 1525.000
    ## 45  41AN54 1545.714 1519.420 1567.359
    ## 46  41AN56 1540.000 1540.000 1540.000
    ## 47  41AN57 1525.000 1525.000 1525.000
    ## 48  41AN67 1525.000 1525.000 1525.000
    ## 49  41AG66 1250.000 1250.000 1250.000

``` r
# reorder sites by mcd
caddo$site <- factor(caddo$site, levels = caddo$site[order(caddo$mcd)])

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
  geom_errorbar(data = caddo, mapping = aes(x = Site, ymin = lower, ymax = upper), width = 0.2, size = 1, colour = "gray") +
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
