Mean ceramic dates for Caddo sites in the Neches River Basin
================
Robert Z. Selden, Jr.
19 May, 2021

Before running this script, ensure that there are no duplicates and that
each site and type have at least one value assigned. If there is a
duplicate site/type or a row/column that are all 0’s, this script will
not work and line 59 will throw an error that reads:

*Error in rmultinom(nsim, data.rowsum\[i, \], prob = mydata\[i, \]) : no
positive probabilities*

There are two datasets included in this [GitHub
repository](https://github.com/aksel-blaise/mean-ceramic-dates); one
with all types/sites (prefix = nrb), and another with only those sites
that include &gt;=5 typed vessels (prefix = nrb2). I encourage you to
tinker with and explore both.

## Calculate MCD and confidence interval

Sites selected for this study come from the Caddo Ceramic Vessel
Database (Perttula 2017), which were plotted using coordinates from the
Texas Historic Sites Atlas, then clipped to the extent of the Neches
River Basin. Mean ceramic dates were calculated and plotted using
modified code originally developed by [Peeples
(2011)](http://www.mattpeeples.net/mcd.html), who was following South
(1972, 1977). Dates and probability ranges were then plotted using
`ggplot2`.

``` r
# set working directory
library(here)
```

    ## here() starts at E:/github/mean-ceramic-dates

``` r
#read data
mydata <- read.csv(file = 'nrb_mcd_data.csv', sep = ',', header = T, row.names = 1)
types <- as.matrix(colnames(mydata))


dates <- read.csv(file = 'nrb_type_dates.csv', sep = ',', header = T)
bc <- min(dates[,2:3])
dates[,2:3] <- dates[,2:3] + (bc*-1)

midpt <- as.matrix(((dates[,3] - dates[,2])/2) + dates[,2])
dates <- cbind(dates,midpt)

mydata2 <- mydata

mcd.calc <- function(x,types,dates) {
tot <- as.matrix(rowSums(x))
for (i in 1:nrow(types)) {
for (j in 1:nrow(dates)) {
if (types[i,] == dates[j,1]) 
{x[,i] <- x[,i] * dates[j,4]}}}

mcd <- matrix(0, nrow(mydata), 1)
rownames(mcd) <- rownames(mydata)

newtot <- as.matrix(rowSums(x))

for (i in 1:nrow(mcd)) {
mcd[i,1] <- newtot[i,] / tot[i,] + bc}
return(mcd)} 

mcd <- mcd.calc(mydata2, types, dates)

###############################################################################

nsim <- 1000

data.rowsum <- as.matrix(rowSums(mydata))
range <- matrix(0, nrow(mydata), 2)

for (i in 1:nrow(mydata)) {
data.sim <- rmultinom(nsim, data.rowsum[i,], prob = mydata[i,])
data.sim <- t(data.sim)
temp <- mcd.calc(data.sim, types,dates)
range[i,1] <- mean(temp) - (sd(temp)*1.96)
range[i,2] <- mean(temp) + (sd(temp)*1.96)}

output <- cbind(row.names(mydata), mcd, range)
colnames(output) <- c('site', 'mcd', 'lower', 'higher')

write.table(output, file = 'mcd_out.csv', sep = ',', row.names = F)
```

## Plot MCD results with confidence interval as error bars

``` r
# plot mean ceramic dates with error bars
# if you use the Rmd file included in the GitHub repository, it will export a publication-ready figure at 600dpi in the mcd_files folder

#devtools::install_github("tidyverse/ggplot2")
library(ggplot2) # load ggplot2

# read mcd_out
caddo <- read.csv("mcd_out.csv", header = TRUE, as.is = TRUE)
knitr::kable(caddo, "pipe", caption = "Table 1. Attributes included in table.")
```

| site    |      mcd |    lower |   higher |
|:--------|---------:|---------:|---------:|
| 41SM56  | 1116.667 | 1087.276 | 1142.656 |
| 41SM55  | 1250.000 | 1250.000 | 1250.000 |
| 41SM77  | 1548.750 | 1519.683 | 1571.960 |
| 41SM90  | 1530.625 | 1525.552 | 1536.081 |
| 41SM73  | 1206.250 | 1017.563 | 1425.294 |
| 41SM93  | 1538.500 | 1531.767 | 1543.948 |
| 41SM151 | 1540.000 | 1540.000 | 1540.000 |
| 41SM158 | 1532.500 | 1522.260 | 1542.127 |
| 41SM249 | 1535.000 | 1525.015 | 1544.372 |
| 41SM150 | 1346.667 | 1241.254 | 1457.998 |
| 41SM355 | 1540.000 | 1540.000 | 1540.000 |
| 41SM2   | 1525.000 | 1525.000 | 1525.000 |
| 41HE114 | 1535.263 | 1531.049 | 1539.059 |
| 41HE4   | 1532.500 | 1524.298 | 1539.783 |
| 41HE75  | 1531.667 | 1527.597 | 1536.620 |
| 41HE78  | 1525.000 | 1525.000 | 1525.000 |
| 41HE82  | 1540.000 | 1540.000 | 1540.000 |
| 41AN201 | 1528.750 | 1522.902 | 1535.363 |
| 41CE4   | 1532.143 | 1528.647 | 1535.434 |
| 41CE12  | 1589.135 | 1569.582 | 1609.040 |
| 41CE14  | 1528.571 | 1527.161 | 1529.673 |
| 41CE15  | 1601.667 | 1481.303 | 1681.214 |
| 41CE17  | 1530.192 | 1527.330 | 1532.749 |
| 41CE19  | 1076.273 | 1059.861 | 1099.149 |
| 41CE23  | 1535.000 | 1529.279 | 1539.802 |
| 41CE25  | 1547.000 | 1523.716 | 1573.092 |
| 41CE421 | 1525.000 | 1525.000 | 1525.000 |
| 41CE6   | 1645.833 | 1561.123 | 1717.516 |
| 41AN1   | 1532.759 | 1529.827 | 1535.522 |
| 41AN13  | 1692.692 | 1645.078 | 1732.253 |
| 41AN14  | 1527.727 | 1523.858 | 1530.706 |
| 41AN16  | 1525.000 | 1525.000 | 1525.000 |
| 41AN18  | 1532.500 | 1521.201 | 1544.412 |
| 41AN184 | 1596.667 | 1474.738 | 1727.371 |
| 41AN2   | 1555.625 | 1529.171 | 1592.985 |
| 41AN21  | 1525.000 | 1525.000 | 1525.000 |
| 41AN26  | 1716.111 | 1692.691 | 1741.481 |
| 41AN32  | 1538.750 | 1520.581 | 1557.072 |
| 41AN34  | 1540.278 | 1517.978 | 1566.115 |
| 41AN38  | 1528.750 | 1524.994 | 1532.608 |
| 41AN39  | 1529.286 | 1524.071 | 1534.501 |
| 41AN44  | 1535.909 | 1531.424 | 1540.060 |
| 41AN48  | 1532.500 | 1520.588 | 1543.799 |
| 41AN53  | 1525.000 | 1525.000 | 1525.000 |
| 41AN54  | 1545.714 | 1517.055 | 1577.727 |
| 41AN56  | 1540.000 | 1540.000 | 1540.000 |
| 41AN57  | 1525.000 | 1525.000 | 1525.000 |
| 41AN67  | 1525.000 | 1525.000 | 1525.000 |
| 41AG66  | 1250.000 | 1250.000 | 1250.000 |

Table 1. Attributes included in table.

``` r
# reorder sites by mcd
caddo$site <- factor(caddo$site, levels = caddo$site[order(caddo$mcd)])

# define variables
Site <- caddo$site
Mean_Ceramic_Date_CE <- caddo$mcd
lower <- caddo$lower
upper <- caddo$higher

# plot that includes the mean ceramic date and the 95% probability range reordered by mcd
cs = theme(
  axis.title.x = element_text(size = 11),
  axis.text.x = element_text(size = 8),
  axis.title.y = element_text(size = 11),
  axis.text.y = element_text(size = 6))

ggplot() +
  geom_errorbar(data = caddo, mapping = aes(x = Site, ymin = lower, ymax = upper), width = 0.2, size = 1, colour = "gray") + # for colour publications, I recommend changing "gray"
  geom_point(data = caddo, mapping = aes(x = Site, y = Mean_Ceramic_Date_CE), size = 2, shape = 21, fill = "white") +
  coord_flip() + # provides a representation of time similar to what archaeologists are used to seeing
  labs(x = "Archaeological Site", y = "Date Range CE") + # the axes are flipped, so x = y and y = x
  cs
```

<div class="figure">

<img src="mcd_files/figure-gfm/ggplot-1.png" alt="Figure 1. Mean ceramic dates and 95% probability ranges for Caddo sites in the Neches River basin." width="100%" />
<p class="caption">
Figure 1. Mean ceramic dates and 95% probability ranges for Caddo sites
in the Neches River basin.
</p>

</div>

``` r
# end of code
```

### Colophon

This version of the analysis was generated on 2021-05-19 11:29:52 using
the following computational environment and dependencies:

``` r
# what R packages and versions were used?
if ("devtools" %in% installed.packages()) devtools::session_info()
```

    ## - Session info ---------------------------------------------------------------
    ##  setting  value                       
    ##  version  R version 4.0.5 (2021-03-31)
    ##  os       Windows 10 x64              
    ##  system   x86_64, mingw32             
    ##  ui       RTerm                       
    ##  language (EN)                        
    ##  collate  English_United States.1252  
    ##  ctype    English_United States.1252  
    ##  tz       America/Chicago             
    ##  date     2021-05-19                  
    ## 
    ## - Packages -------------------------------------------------------------------
    ##  package     * version date       lib source        
    ##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.2)
    ##  cachem        1.0.5   2021-05-15 [1] CRAN (R 4.0.5)
    ##  callr         3.7.0   2021-04-20 [1] CRAN (R 4.0.5)
    ##  cli           2.5.0   2021-04-26 [1] CRAN (R 4.0.5)
    ##  colorspace    2.0-1   2021-05-04 [1] CRAN (R 4.0.5)
    ##  crayon        1.4.1   2021-02-08 [1] CRAN (R 4.0.3)
    ##  DBI           1.1.1   2021-01-15 [1] CRAN (R 4.0.3)
    ##  desc          1.3.0   2021-03-05 [1] CRAN (R 4.0.5)
    ##  devtools      2.4.1   2021-05-05 [1] CRAN (R 4.0.5)
    ##  digest        0.6.27  2020-10-24 [1] CRAN (R 4.0.3)
    ##  dplyr         1.0.6   2021-05-05 [1] CRAN (R 4.0.5)
    ##  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.0.5)
    ##  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.2)
    ##  fansi         0.4.2   2021-01-15 [1] CRAN (R 4.0.3)
    ##  farver        2.1.0   2021-02-28 [1] CRAN (R 4.0.5)
    ##  fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.0.3)
    ##  fs            1.5.0   2020-07-31 [1] CRAN (R 4.0.2)
    ##  generics      0.1.0   2020-10-31 [1] CRAN (R 4.0.3)
    ##  ggplot2     * 3.3.3   2020-12-30 [1] CRAN (R 4.0.3)
    ##  glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.2)
    ##  gtable        0.3.0   2019-03-25 [1] CRAN (R 4.0.2)
    ##  here        * 1.0.1   2020-12-13 [1] CRAN (R 4.0.3)
    ##  highr         0.9     2021-04-16 [1] CRAN (R 4.0.5)
    ##  htmltools     0.5.1.1 2021-01-22 [1] CRAN (R 4.0.3)
    ##  knitr         1.33    2021-04-24 [1] CRAN (R 4.0.5)
    ##  labeling      0.4.2   2020-10-20 [1] CRAN (R 4.0.3)
    ##  lifecycle     1.0.0   2021-02-15 [1] CRAN (R 4.0.4)
    ##  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.0.3)
    ##  memoise       2.0.0   2021-01-26 [1] CRAN (R 4.0.3)
    ##  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.0.2)
    ##  pillar        1.6.1   2021-05-16 [1] CRAN (R 4.0.5)
    ##  pkgbuild      1.2.0   2020-12-15 [1] CRAN (R 4.0.3)
    ##  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.0.2)
    ##  pkgload       1.2.1   2021-04-06 [1] CRAN (R 4.0.5)
    ##  prettyunits   1.1.1   2020-01-24 [1] CRAN (R 4.0.2)
    ##  processx      3.5.2   2021-04-30 [1] CRAN (R 4.0.5)
    ##  ps            1.6.0   2021-02-28 [1] CRAN (R 4.0.5)
    ##  purrr         0.3.4   2020-04-17 [1] CRAN (R 4.0.2)
    ##  R6            2.5.0   2020-10-28 [1] CRAN (R 4.0.3)
    ##  remotes       2.3.0   2021-04-01 [1] CRAN (R 4.0.5)
    ##  rlang         0.4.11  2021-04-30 [1] CRAN (R 4.0.5)
    ##  rmarkdown     2.8     2021-05-07 [1] CRAN (R 4.0.5)
    ##  rprojroot     2.0.2   2020-11-15 [1] CRAN (R 4.0.3)
    ##  scales        1.1.1   2020-05-11 [1] CRAN (R 4.0.2)
    ##  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.2)
    ##  stringi       1.6.2   2021-05-17 [1] CRAN (R 4.0.5)
    ##  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.0.2)
    ##  testthat      3.0.2   2021-02-14 [1] CRAN (R 4.0.4)
    ##  tibble        3.1.2   2021-05-16 [1] CRAN (R 4.0.5)
    ##  tidyselect    1.1.1   2021-04-30 [1] CRAN (R 4.0.5)
    ##  usethis       2.0.1   2021-02-10 [1] CRAN (R 4.0.3)
    ##  utf8          1.2.1   2021-03-12 [1] CRAN (R 4.0.5)
    ##  vctrs         0.3.8   2021-04-29 [1] CRAN (R 4.0.5)
    ##  withr         2.4.2   2021-04-18 [1] CRAN (R 4.0.5)
    ##  xfun          0.23    2021-05-15 [1] CRAN (R 4.0.5)
    ##  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.0.2)
    ## 
    ## [1] C:/Users/seldenjrz/Documents/R/win-library/4.0
    ## [2] C:/Program Files/R/R-4.0.5/library

Current Git commit details are:

``` r
# where can I find this commit? 
if ("git2r" %in% installed.packages() & git2r::in_repository(path = ".")) git2r::repository(here::here())  
```

    ## Local:    master E:/github/mean-ceramic-dates
    ## Remote:   master @ origin (https://github.com/aksel-blaise/mean-ceramic-dates)
    ## Head:     [ab0ba41] 2021-05-18: <init colophon>

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-RN11605" class="csl-entry">

Perttula, Timothy K. 2017. “Caddo Ceramic Vessel Database from Sites in
Texas, Louisiana, Oklahoma, and Arkansas.” *Journal of Northeast Texas
Archaeology* 71: 147–54.

</div>

<div id="ref-RN21010" class="csl-entry">

South, Stanley A. 1972. “Evolution and Horizon as Revealed in Ceramic
Analysis in Historical Archaeology.” *Conference on Historic Site
Archaeology Papers* 6 (2): 71–116.

</div>

<div id="ref-RN21011" class="csl-entry">

———. 1977. *Method and Theory in Historical Archaeology*. New York:
Academic Press.

</div>

</div>
