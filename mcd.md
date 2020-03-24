Mean Ceramic Dates for Caddo Sites in the Neches River Basin
================
Robert Z. Selden, Jr.
March 23, 2020

## Including all sites and all vessels

The code chunk below was developed by Matthew A. Peeples, following
Keith Kintigh, who was channelling Stanley South (1977). Further details
regarding the method can be found on [Matthew A.
Peeples’](http://www.mattpeeples.net/mcd.html) website.

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

    ## -- Attaching packages ------------------------------------- tidyverse 1.3.0.9000 --

    ## v ggplot2 3.3.0.9000     v purrr   0.3.3     
    ## v tibble  2.1.3          v dplyr   0.8.5     
    ## v tidyr   1.0.2          v stringr 1.4.0     
    ## v readr   1.3.1          v forcats 0.5.0

    ## -- Conflicts --------------------------------------------- tidyverse_conflicts() --
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
  geom_errorbar(data=caddo, mapping=aes(x=Site, ymin=lower, ymax=upper), width=0.5, size=0.5) +
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
  geom_errorbar(data=caddo2, mapping=aes(x=Site, ymin=lower, ymax=upper), width=0.5, size=0.5) +
  geom_point(data=caddo2, mapping=aes(x=Site, y=Mean_Ceramic_Date), size=2, shape=21, fill = "white") +
  coord_flip() + # provides a representation of time similar to what archaeologists are used to seeing
  cs
```

<img src="mcd_files/figure-gfm/ggplot2-1.png" width="100%" />

``` r
# end of code
```
