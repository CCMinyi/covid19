# covid19

## Paper :

Ting-Li Chen, Elizabeth Chou, Min-Yi Chen and Fushing Hsieh (2022). Unraveling implicit human behavioral effects on dynamic characteristics of Covid-19 daily infection rates in Taiwan.    [arxiv link](https://arxiv.org/abs/2211.10926)

## RShiny Demo :
[Website link](https://ccminyi.shinyapps.io/Covid_daily_infection_rate_in_TW/)

## Data
* Source :
	*  [Covid-19 data](https://covid-19.nchc.org.tw)
	*  [Global Administrative Areas (GADM)](https://gadm.org/download_country_v3.html)	

## Table
For Table 2, 4, 6, and 7: ues CE and SCE drop R code.

* Table 2: CE and drop.csv  (use data0805cat.csv)
  
* Table 4: CE and drop-right90.csv  (use data0805cat.csv)
  
* Table 6: ce and drop data0805-RL_NS.csv  (use data0805-leftright-cat.csv)
  
* Table 7: ce and drop data0805-RL_city.csv (use data0805-leftright-cat.csv)      

## Figure

* Fig 1 : use daily_infection_rate.R with overall_covid.csv.

* Fig 3 : use Taipei_smooth.R with taipei_district.csv.

* Fig 4 : llustrative definitions and extractions of all features.R with df_taipei.csv.

* Fig 5 : use Directed assciative network R code. (Figure 6 can be created by modifying the direction part in the R code.)

* Fig 7, 8 : use Taiwan_plot.R with peak/peak_data.csv, taiwan_data/gadm36_TWN_2.shp.

* Fig 9, 10, 11, and 12: use Heatmap R code with the treepath results csv files. The trees are created by using big-cat.csv 


## Packages
* Packages we used:
	* igraph
	* RColorBrewer
	* tidyverse
	* magrittr
	* reshape2
	* ggplot2
	* showtext
	* lubridate
	* shiny
