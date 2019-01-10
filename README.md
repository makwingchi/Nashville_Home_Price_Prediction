---
title: "Home Price Prediction in Nashville, TN"
author: "Rongzhi Mai"
date: "Nov. 11, 2018"
output:
  html_document: 
    code_folding: "hide"
---

<style type="text/css">

body{ /* Normal  */
      font-size: 15px;
  }
td {  /* Table  */
  font-size: 13px;
}
h1.title {
  font-size: 38px;
  color: DarkRed;
}
h1 { /* Header 1 */
  font-size: 28px;
  color: DarkBlue;
}
h2 { /* Header 2 */
    font-size: 22px;
  color: DarkBlue;
}
h3 { /* Header 3 */
  font-size: 18px;
  color: DarkBlue;
}
code.r{ /* Code block */
    font-size: 10px;
}
pre { /* Code block - determines code spacing between lines */
    font-size: 12px;
}
</style>


* [1. Introduction](#link1)
* [2. Data](#link2)
    + [2.1. Overview](#link3)
    + [2.2. Exploratory Analysis](#link4)
    + [2.3. Maps of Variables](#link5)
    + [2.4. Comparison between houses with offsite and onsite owners](#link6)
* [3. Method](#link7)
* [4. Results](#link8)
    + [4.1. In-Sample Prediction](#link9)
    + [4.2. Out-of-sample Prediction](#link10)
    + [4.3. Cross-Validation](#link11)
    + [4.4. Generalizability across space](#link12)
* [5. Discussion](#link13)
* [6. Conclusion](#link14)
* [APPENDIX](#link15)

***

This project aims to build an OLS predictive model of home prices for the City of Nashville, TN. The primary dataset is called "train.and.test_student.csv", which includes several fields of home characteristics, such as acres of land, the number of rooms, and whether it is supported by air conditioning. This markdown file will explain how I undertake this analysis. Let's begin by loading the packages and disabling scientific notation.


```r
library(corrplot)
library(caret) 
library(AppliedPredictiveModeling)
library(stargazer)
library(tidyverse)
library(sf)
library(FNN)
library(ggmap)
library(spdep)
library(osmdata)
library(knitr)
library(kableExtra)

options(scipen=999)

vars_train <- 
  vars %>% 
  filter(test==0) %>%
  filter(LandUseFul != 'VACANT RESIDENTIAL LAND')
```

***

# 1. Introduction{#link1}
Zillow, one of the nation¡¯s largest and most powerful real estate online databases, has always been endeavoring to provide and predict estimates of home values for its users. However, it has been realized that its housing market predictions aren¡¯t as accurate as they could be. Housing price prediction is indeed not an easy task since so many factors are at play. It is very difficult to find effective determinants and reduce the bias of the prediction. In this project, we are making every effort to dig deeper and build a more reliable predictive model which would be more useful in Nashville¡¯s real estate market.

The primary methodology adopted here is Ordinary Least Squares (OLS) regression, where the home price is a linear function of a set of predicting variables. We introduced 20 independent variables in our final model, including internal characteristics of the houses, demographic predictors at block group level, amenities and public services, as well as spatial lag effects of nearby home sales. 

Overall, our final model is able to explain 65.2% of the variation in home sale prices. The predicted prices our model generates will not differ from the true values by much. The cross-validation technique suggests that the predicted home prices (in log format) are approximately 0.29 different from the true values. Other regression statistics such as the root mean square error (RMSE) and R-squared are also ideal, indicating our model is able to predict unseen data equally well across datasets. When it comes to spatial generalizability, on one hand, Moran¡¯s I test informed us that there is no significant spatial auto-correlation in our model; on the other, even when we apply zip code as our unit of analysis, it seems that the performance of the model is quite stable across zip codes with different economic level. 


***

# 2. Data{#link2}
## 2.1. Overview{#link3}
First of all, 9,001 samples are included in the dataset we use. We notice there are 570 homes sitting on vacant residential land, and hence consider them as outliers. When building our prediction model, we will get rid of these observations.

The dependent variable we use in this home price prediction is, no doubt, the sale price of each house. This information is available in the Nashville homesales dataset. Later we will discuss why we take the log of the dependent variable.

As for independent variables, we have collected four kinds of independent variables in total, i.e. internal characteristics, demographic predictors, amenities/public services, and the underlying spatial structure of prices (spatial lag). The first category is also available in the above-mentioned dataset. Regarding demographic predictors, the US census provides a number of useful indicators in census block group level for us. Public services are assessed mainly by the distance from each house to the nearest N amenities. Last but not least, we calculate several spatial lag variables, basically taking advantage of the homesales dataset as well.

A detailed variable list is provided below.


```r
kable(table1, caption = 'Summary of Variables in the Prediction Model') %>%
  kable_styling(full_width = F,
                font_size = 15) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1:2, valign = "middle")
```

<table class="table" style="font-size: 15px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Summary of Variables in the Prediction Model</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> Category </th>
   <th style="text-align:left;"> Symbol </th>
   <th style="text-align:left;"> Meaning </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Dependent Variable </td>
   <td style="text-align:left;"> ----- </td>
   <td style="text-align:left;"> log_SalePrice </td>
   <td style="text-align:left;"> Sale Price of the property (log-transformed) </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="20"> Independent Variable </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="7"> Internal Characteristics </td>
   <td style="text-align:left;"> age_factor </td>
   <td style="text-align:left;"> 2018 minus the effective year the property was built; 0 for age &lt; 5; 1 for age 5-23; 2 for age 24-38; 3 for the others </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> bedroom_factor </td>
   <td style="text-align:left;"> 0 for properties whose number of bedrooms less than 3; 1 for those equal 3; 2 for the others </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> baths_factor </td>
   <td style="text-align:left;"> 0 for properties whose number of bathrooms equal 1; 1 for those equal 2; 2 for the others </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> building_factor </td>
   <td style="text-align:left;"> 0 for single family; 1 for the others </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> log_sf_finis_1 </td>
   <td style="text-align:left;"> Total Finished area less an adjustment (log-transformed) </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> log_Acrage </td>
   <td style="text-align:left;"> Acres of land (log-transformed) </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> offsite </td>
   <td style="text-align:left;"> Whether the owner of the properties is offsite or not. 0 for onsite; 1 for offsite </td>
  </tr>
  <tr>
   
   <td style="text-align:left;vertical-align: middle !important;" rowspan="3"> Demographic Predictors </td>
   <td style="text-align:left;"> log_MdValue </td>
   <td style="text-align:left;"> Median value of owner-occupied housing units in census block group (log-transformed) </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> Pctwhite </td>
   <td style="text-align:left;"> Percentage of white population in census block group </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> log_Density </td>
   <td style="text-align:left;"> Population density in census block group (log-transformed) </td>
  </tr>
  <tr>
   
   <td style="text-align:left;vertical-align: middle !important;" rowspan="7"> Amenities/Public Services </td>
   <td style="text-align:left;"> log_Public_Art_Collection_20 </td>
   <td style="text-align:left;"> Distance to nearest 20 public art collections (log-transformed) </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> log_garden_10 </td>
   <td style="text-align:left;"> Distance to nearest 10 gardens (log-transformed) </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> log_retail2_10 </td>
   <td style="text-align:left;"> Distance to nearest 10 retail land use (log-transformed) </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> BID </td>
   <td style="text-align:left;"> Distance to nearest Business Improvement District </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> log_home_20 </td>
   <td style="text-align:left;"> Distance to nearest 20 properties (log-transformed) </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> school_white_10 </td>
   <td style="text-align:left;"> Proportion of white students in nearest 10 schools </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> school_econ_15 </td>
   <td style="text-align:left;"> Proportion of economically disabled students in nearest 15 public schools </td>
  </tr>
  <tr>
   
   <td style="text-align:left;vertical-align: middle !important;" rowspan="3"> Spatial Lag </td>
   <td style="text-align:left;"> log_price_5 </td>
   <td style="text-align:left;"> Average sale price of nearby 5 properties (log-transformed) </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> log_price_10 </td>
   <td style="text-align:left;"> Average sale price of nearby 10 properties (log-transformed) </td>
  </tr>
  <tr>
   
   
   <td style="text-align:left;"> log_avg_price_10 </td>
   <td style="text-align:left;"> Average price per square foot of nearby 10 properties (log-transformed) </td>
  </tr>
</tbody>
</table>

## 2.2. Exploratory Analysis{#link4}
There are some variables whose distribution are not normal. In these cases, we apply log transformation towards those variables. A typical example would be our dependent variable - home sale price, which is shown below.


```r
ggplot(vars_train) +
  geom_histogram(aes(x = SalePrice), fill = '#b8d1e3') +
  labs(title = 'Distribution of Home Sale Price',
       subtitle = 'Nashville, TN',
       x = 'Home Sale Price',
       y = 'Count') +
  theme_minimal() +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=10, face="italic", color="grey"))

ggplot(vars_train) +
  geom_histogram(aes(x = log_SalePrice), fill = '#b8d1e3') +
  labs(title = 'Distribution of log Home Sale Price',
       subtitle = 'Nashville, TN',
       x = 'Log Home Sale Price',
       y = 'Count') +
  theme_minimal() +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=10, face="italic", color="grey"))
```

<img src="markdown_files/figure-html/distribution of saleprice-1.png" width="50%" /><img src="markdown_files/figure-html/distribution of saleprice-2.png" width="50%" />


Below are the summary statistics of the variables we use in the model.



```r
kable(table2, align = "l", col.names = c('Variable', 'N', "Mean", "St. Dev", 'Min', 'Pctl(25)', 'Pctl(75)', 'Max'),
      caption = 'Summary Statistics of Continuous Variables') %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                font_size = 15,
                full_width = F) %>%
  row_spec(seq(1, nrow(table2), 2), background = "#ecf6fe")
```

<table class="table table-striped table-hover" style="font-size: 15px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Summary Statistics of Continuous Variables</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:left;"> Mean </th>
   <th style="text-align:left;"> St. Dev </th>
   <th style="text-align:left;"> Min </th>
   <th style="text-align:left;"> Pctl(25) </th>
   <th style="text-align:left;"> Pctl(75) </th>
   <th style="text-align:left;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> SalePrice </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 8431 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 312,163.300 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 308,152.200 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 2,000 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 150,000 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 376,340 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 6,894,305 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sf_finis_1 </td>
   <td style="text-align:left;"> 8431 </td>
   <td style="text-align:left;"> 1,848.817 </td>
   <td style="text-align:left;"> 878.403 </td>
   <td style="text-align:left;"> 348 </td>
   <td style="text-align:left;"> 1,247 </td>
   <td style="text-align:left;"> 2,189 </td>
   <td style="text-align:left;"> 10,608 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> Acrage </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 8431 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.355 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.275 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.030 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.210 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.378 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 8.160 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MdValue </td>
   <td style="text-align:left;"> 8431 </td>
   <td style="text-align:left;"> 227,860.600 </td>
   <td style="text-align:left;"> 141,485.300 </td>
   <td style="text-align:left;"> 58,600 </td>
   <td style="text-align:left;"> 136,200 </td>
   <td style="text-align:left;"> 295,700 </td>
   <td style="text-align:left;"> 1,840,300 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> Pctwhite </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 8431 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.648 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.248 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.000 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.480 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.839 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 1.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Density </td>
   <td style="text-align:left;"> 8431 </td>
   <td style="text-align:left;"> 3,466.940 </td>
   <td style="text-align:left;"> 2,052.780 </td>
   <td style="text-align:left;"> 160.102 </td>
   <td style="text-align:left;"> 1,967.141 </td>
   <td style="text-align:left;"> 4,612.103 </td>
   <td style="text-align:left;"> 23,124.220 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> Public_Art_Collection_20 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 8431 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 25,863.690 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 15,428.200 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 4,263.141 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 13,570.810 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 37,949.010 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 71,686.350 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> garden_10 </td>
   <td style="text-align:left;"> 8431 </td>
   <td style="text-align:left;"> 9.288 </td>
   <td style="text-align:left;"> 0.995 </td>
   <td style="text-align:left;"> 3.581 </td>
   <td style="text-align:left;"> 8.673 </td>
   <td style="text-align:left;"> 9.995 </td>
   <td style="text-align:left;"> 10.983 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> retail2_10 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 8431 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 8.712 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.684 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 5.399 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 8.349 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 9.194 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 10.308 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BID </td>
   <td style="text-align:left;"> 8431 </td>
   <td style="text-align:left;"> 27,776.980 </td>
   <td style="text-align:left;"> 16,299.560 </td>
   <td style="text-align:left;"> 1,361.014 </td>
   <td style="text-align:left;"> 14,719.020 </td>
   <td style="text-align:left;"> 40,207.290 </td>
   <td style="text-align:left;"> 75,448.300 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> home_20 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 8431 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 891.221 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 494.818 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 47.113 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 547.953 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 1,108.266 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 5,337.407 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> school_white_10 </td>
   <td style="text-align:left;"> 8431 </td>
   <td style="text-align:left;"> 0.276 </td>
   <td style="text-align:left;"> 0.125 </td>
   <td style="text-align:left;"> 0.030 </td>
   <td style="text-align:left;"> 0.199 </td>
   <td style="text-align:left;"> 0.338 </td>
   <td style="text-align:left;"> 0.638 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> school_econ_15 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 8431 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.467 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.092 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.199 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.415 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.528 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.677 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> price_5 </td>
   <td style="text-align:left;"> 8431 </td>
   <td style="text-align:left;"> 288,578.000 </td>
   <td style="text-align:left;"> 239,454.800 </td>
   <td style="text-align:left;"> 10,500 </td>
   <td style="text-align:left;"> 146,841.7 </td>
   <td style="text-align:left;"> 352,940 </td>
   <td style="text-align:left;"> 4,839,271 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> price_10 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 8431 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 316,894.300 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 246,583.800 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 38,357.800 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 163,847.400 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 390,316.100 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 3,659,410.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> avg_price_10 </td>
   <td style="text-align:left;"> 8431 </td>
   <td style="text-align:left;"> 175.066 </td>
   <td style="text-align:left;"> 118.146 </td>
   <td style="text-align:left;"> 16.221 </td>
   <td style="text-align:left;"> 108.503 </td>
   <td style="text-align:left;"> 208.921 </td>
   <td style="text-align:left;"> 1,842.748 </td>
  </tr>
</tbody>
</table>



```r
kable(table3, col.names = c('Variable', 'Level 0', 'Level 1', 'Level 2', 'Level 3'),
      caption = 'Summary Statistics of Categorical Variables') %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                font_size = 16) %>%
  row_spec(seq(1, nrow(table3), 2), background = "#ecf6fe")
```

<table class="table table-striped table-hover" style="font-size: 16px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Summary Statistics of Categorical Variables</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> Level 0 </th>
   <th style="text-align:left;"> Level 1 </th>
   <th style="text-align:left;"> Level 2 </th>
   <th style="text-align:left;"> Level 3 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> age_factor </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 2355 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 2436 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 1409 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 2231 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bedroom_factor </td>
   <td style="text-align:left;"> 2465 </td>
   <td style="text-align:left;"> 4382 </td>
   <td style="text-align:left;"> 1584 </td>
   <td style="text-align:left;"> --- </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> baths_factor </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 2404 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 4141 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 1886 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> --- </td>
  </tr>
  <tr>
   <td style="text-align:left;"> building_factor </td>
   <td style="text-align:left;"> 5972 </td>
   <td style="text-align:left;"> 2459 </td>
   <td style="text-align:left;"> --- </td>
   <td style="text-align:left;"> --- </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> offsite </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 4810 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 3621 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> --- </td>
   <td style="text-align:left;background-color: #ecf6fe;"> --- </td>
  </tr>
</tbody>
</table>


We also create a correlation matrix to evaluate the relationships between our variables. We can see the log of sale price is highly associated with the prices of nearby houses. What can also be identified is the multicollinearity issue of the model, however, we still decide to keep those indicators for the sake of model performance.


```r
corrplot(M, method = "shade")
```

<img src="markdown_files/figure-html/correlation matrix-1.png" width="576" />

## 2.3. Maps of Variables{#link5}
We first take a look at the distribution of home sale price in the city. Though we use the log transformation in the model, here we think the original values are much more straightforward.


```r
ggmap(map3) +
  geom_sf(data = baseMap, inherit.aes = FALSE, color='grey21', size=0.8) +
  geom_point(data=vars_train, 
             aes(x=WGS1984X, y=WGS1984Y, colour=factor(ntile(SalePrice,5))), 
             size = 0.7) + 
  scale_colour_manual(values = c("#d6d6ff","#8f97e3","#556cc9","#244ead","#003994"),
                      labels=as.character(round(quantile(vars_train$SalePrice,
                                                         c(.1,.2,.4,.6,.8),na.rm=T))),
                      name="Sales Price \n(Quintile Breaks)") +
  labs(title="Home Sale Price",
       subtitle="Nashville, TN") +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=11, face="italic", color="grey")) +
  theme(legend.text = element_text(face = "italic"))
```

<img src="markdown_files/figure-html/homesale price map-1.png" width="672" />

As for explanatory variables, we think the distance to nearest 20 public art collections, total finished area less an adjustment, and the proportion of economically disabled students in nearest 15 public schools are of interest. What would be displayed are their initial values as well.


```r
ggmap(map3) +
  geom_sf(data = baseMap, inherit.aes = FALSE, color='grey21', size=0.8) +
  geom_point(data=vars_train, 
             aes(x=WGS1984X, y=WGS1984Y, colour=factor(ntile(Public_Art_Collection_20,5))), 
             size = 0.7) + 
  scale_colour_manual(values = c("#d6d6ff","#8f97e3","#556cc9","#244ead","#003994"),
                      labels=as.character(round(quantile(vars_train$Public_Art_Collection_20,
                                                         c(.1,.2,.4,.6,.8),na.rm=T),2)),
                      name="Distance \n(Quintile Breaks)") +
  labs(title="Distance to nearest 20 Public Art Collections",
       subtitle='Nashville, TN') +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=11, face="italic", color="grey")) +
  theme(legend.text = element_text(face = "italic"))
```

<img src="markdown_files/figure-html/public art collections-1.png" width="672" />


```r
ggmap(map3) +
  geom_sf(data = baseMap, inherit.aes = FALSE, color='grey21', size=0.8) +
  geom_point(data=vars_train, 
             aes(x=WGS1984X, y=WGS1984Y, colour=factor(ntile(sf_finis_1,5))), 
             size = 0.7) + 
  scale_colour_manual(values = c("#d6d6ff","#8f97e3","#556cc9","#244ead","#003994"),
                      labels=as.character(round(quantile(vars_train$sf_finis_1,
                                                         c(.1,.2,.4,.6,.8),na.rm=T),2)),
                      name="Area \n(Quintile Breaks)") +
  labs(title="Total Finished area less an adjustment",
       subtitle="Nashville, TN") +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=11, face="italic", color="grey")) +
  theme(legend.text = element_text(face = "italic"))
```

<img src="markdown_files/figure-html/total finished area less an adjustment-1.png" width="672" />


```r
ggmap(map3) +
  geom_sf(data = baseMap, inherit.aes = FALSE, color='grey21', size=0.8) +
  geom_point(data=vars_train, 
             aes(x=WGS1984X, y=WGS1984Y, colour=factor(ntile(school_econ_15,5))), 
             size = 0.7) + 
  scale_colour_manual(values = c("#d6d6ff","#8f97e3","#556cc9","#244ead","#003994"),
                      labels=as.character(round(quantile(vars_train$school_econ_15,
                                                         c(.1,.2,.4,.6,.8),na.rm=T),2)),
                      name="Distance \n(Quintile Breaks)") +
  labs(title="The Proportion of Economically Disabled Students \nin nearest 15 Public Schools",
       subtitle='Nashville, TN') +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=11, face="italic", color="grey")) +
  theme(legend.text = element_text(face = "italic"))
```

<img src="markdown_files/figure-html/proportion of economically disabled students-1.png" width="672" />

## 2.4. Comparison between houses with offsite and onsite owners{#link6}
Below shows the variable distribution for houses where the owners are onsite or offsite. Illustrated by the box plots, generally the median of log saleprice for onsite houses are higher, and its distribution is a bit more condensed.


```r
ggplot(data = OffOrOnsite, aes(offsite, value)) +
  geom_boxplot(aes(fill=offsite)) +  
  facet_wrap(~variable,scales="free",ncol=4) +
  scale_fill_manual(values = c("#fa725a","#b8d1e3"),
                    name = "Offsite?",
                    labels=c("No","Yes")) +
  labs(title="Variable distributions across Houses with Offsite \nand Onsite Owners",
       subtitle='Nashville, TN', 
       x="Offsite or not",
       y="Value") +
  theme_minimal() +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=10, face="italic", color="grey")) +
  theme(legend.text = element_text(face = "italic"))
```

<img src="markdown_files/figure-html/variable distribution boxplots-1.png" width="768" />

***

# 3. Methods{#link7}
The method we use is called Ordinary Least Squares regression, and we will go through four steps in the following sections. 

The first step is in-sample prediction. Initially the team has more than 300 independent variables, but we narrow down to only 20 without worsening the model's performance. This step is essential for us to get a sense of how the model behaves using the whole dataset. The second one is out-of-sample prediction, in which we randomly select 75% of the data to train the model, and use the remaining 25% to test it. One of the goals of predictive modeling is generalizability, consequently, we would want to know if the model predicts well for the data it has not seen yet. However, one can barely make sure whether a randomly selected data sample is representative enough. This makes sense of our third step - cross validation. Basically it is similar to the previous one, except that we will evaluate how generalizable our model is to a number of random samples. The last step would be to assess model generalizability across space. We use zipcode districts as the unit of analysis to see whether the model is able to perform equally well across different zipcodes.

***

# 4. Results{#link8}
Here is the regression formula we utilize in the prediction.


```r
reg1 <- lm(log_SalePrice ~ age_factor + bedroom_factor + baths_factor +
             building_factor + log_sf_finis_1 + log_Acrage + offsite + 
             log_MdValue + Pctwhite + log_Density + log_Public_Art_Collection_20 +  
             log_garden_10 + log_retail2_10 + BID + log_home_20 +
             school_white_10 +school_econ_15 + 
             log_price_5 + log_price_10 + log_avg_price_10, 
           data = vars_train)
```

## 4.1. In-Sample Prediction{#link9}
The results of in-sample prediction are shown as follows. The first table illustrates the overall model performance, and it indicates that 65.2% variation of homesale price can be explained by our independent variables. In addition, the second table details how each indicator performs in the model. The number of stars means how significant a particular variable is.



```r
kable(table4, align = "l", col.names = c('Statistics', 'R-squared', 
                                         'Adjusted R-squared', 'F statistic', 
                                         'Degree of Freedom', 'P-value'),
      caption = 'In-sample Model Statistics') %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                font_size = 15) %>%
  row_spec(seq(1, nrow(table4), 2), background = "#ecf6fe")
```

<table class="table table-striped table-hover" style="font-size: 15px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">In-sample Model Statistics</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Statistics </th>
   <th style="text-align:left;"> R-squared </th>
   <th style="text-align:left;"> Adjusted R-squared </th>
   <th style="text-align:left;"> F statistic </th>
   <th style="text-align:left;"> Degree of Freedom </th>
   <th style="text-align:left;"> P-value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> In-Sample Prediction </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.6533 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.6523 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 659.9 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 8406 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> &lt;0.00000000000000022 </td>
  </tr>
</tbody>
</table>



```r
kable(table5, col.names = c('Variable', 'Estimate', 'Std. Error', 't value', 'P value'), 
      caption = 'In-sample Model Results') %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                font_size = 15) %>%
  footnote(general = 'Three asterisks means significant at 99.9% confidence level; two asterisks 99% confidence level; one asterisk 95% confidence level; and a tiny dot 90% confidence level.',
           general_title = "Significance Codes: ") %>%
  row_spec(seq(1, nrow(table5), 2), background = "#ecf6fe")
```

<table class="table table-striped table-hover" style="font-size: 15px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">In-sample Model Results</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> Estimate </th>
   <th style="text-align:left;"> Std. Error </th>
   <th style="text-align:left;"> t value </th>
   <th style="text-align:left;"> P value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> (Intercept) </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 5.520039904 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.465047023 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 11.870 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> &lt; 0.0000000000000002 *** </td>
  </tr>
  <tr>
   <td style="text-align:left;"> age_factor1 </td>
   <td style="text-align:left;"> 0.029189132 </td>
   <td style="text-align:left;"> 0.015071681 </td>
   <td style="text-align:left;"> 1.937 </td>
   <td style="text-align:left;"> 0.052817 . </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> age_factor2 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> -0.029688169 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.018075049 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> -1.642 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.100525 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> age_factor3 </td>
   <td style="text-align:left;"> -0.062303025 </td>
   <td style="text-align:left;"> 0.017940115 </td>
   <td style="text-align:left;"> -3.473 </td>
   <td style="text-align:left;"> 0.000518 *** </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> bedroom_factor1 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.037865956 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.014410136 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 2.628 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.008611 ** </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bedroom_factor2 </td>
   <td style="text-align:left;"> 0.058217800 </td>
   <td style="text-align:left;"> 0.020665511 </td>
   <td style="text-align:left;"> 2.817 </td>
   <td style="text-align:left;"> 0.004857 ** </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> baths_factor1 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.039071297 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.015001192 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 2.605 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.009216 ** </td>
  </tr>
  <tr>
   <td style="text-align:left;"> baths_factor2 </td>
   <td style="text-align:left;"> 0.056884030 </td>
   <td style="text-align:left;"> 0.020928092 </td>
   <td style="text-align:left;"> 2.718 </td>
   <td style="text-align:left;"> 0.006580 ** </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> building_factor1 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> -0.085276317 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.014377086 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> -5.931 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.000000003122282 *** </td>
  </tr>
  <tr>
   <td style="text-align:left;"> log_sf_finis_1 </td>
   <td style="text-align:left;"> 0.452316073 </td>
   <td style="text-align:left;"> 0.021971620 </td>
   <td style="text-align:left;"> 20.586 </td>
   <td style="text-align:left;"> &lt; 0.0000000000000002 *** </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> log_Acrage </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.104325338 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.010523314 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 9.914 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> &lt; 0.0000000000000002 *** </td>
  </tr>
  <tr>
   <td style="text-align:left;"> offsite1 </td>
   <td style="text-align:left;"> -0.245366643 </td>
   <td style="text-align:left;"> 0.010027412 </td>
   <td style="text-align:left;"> -24.470 </td>
   <td style="text-align:left;"> &lt; 0.0000000000000002 *** </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> log_MdValue </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.134560988 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.017406555 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 7.730 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.000000000000012 *** </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pctwhite </td>
   <td style="text-align:left;"> 0.101718482 </td>
   <td style="text-align:left;"> 0.029350251 </td>
   <td style="text-align:left;"> 3.466 </td>
   <td style="text-align:left;"> 0.000532 *** </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> log_Density </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.016311631 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.008005077 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 2.038 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.041615 * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> log_Public_Art_Collection_20 </td>
   <td style="text-align:left;"> -0.163120732 </td>
   <td style="text-align:left;"> 0.034379077 </td>
   <td style="text-align:left;"> -4.745 </td>
   <td style="text-align:left;"> 0.000002121894355 *** </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> log_garden_10 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> -0.288138335 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.070734516 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> -4.074 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.000046733645839 *** </td>
  </tr>
  <tr>
   <td style="text-align:left;"> log_retail2_10 </td>
   <td style="text-align:left;"> -0.144331131 </td>
   <td style="text-align:left;"> 0.064192224 </td>
   <td style="text-align:left;"> -2.248 </td>
   <td style="text-align:left;"> 0.024575 * </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> BID </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.000002784 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.000001272 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 2.189 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.028651 * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> log_home_20 </td>
   <td style="text-align:left;"> -0.080611627 </td>
   <td style="text-align:left;"> 0.011951937 </td>
   <td style="text-align:left;"> -6.745 </td>
   <td style="text-align:left;"> 0.000000000016353 *** </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> school_white_10 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.222161042 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.097758249 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 2.273 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.023078 * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> school_econ_15 </td>
   <td style="text-align:left;"> -0.280006557 </td>
   <td style="text-align:left;"> 0.122084984 </td>
   <td style="text-align:left;"> -2.294 </td>
   <td style="text-align:left;"> 0.021842 * </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> log_price_5 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.163514182 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.017292883 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 9.456 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> &lt; 0.0000000000000002 *** </td>
  </tr>
  <tr>
   <td style="text-align:left;"> log_price_10 </td>
   <td style="text-align:left;"> 0.118961604 </td>
   <td style="text-align:left;"> 0.029291103 </td>
   <td style="text-align:left;"> 4.061 </td>
   <td style="text-align:left;"> 0.000049231575889 *** </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> log_avg_price_10 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.291349995 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.027442965 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 10.617 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> &lt; 0.0000000000000002 *** </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; border: 0;" colspan="100%"><span style="font-style: italic;">Significance Codes: </span></td></tr>
<tr><td style="padding: 0; border: 0;" colspan="100%">
<sup></sup> Three asterisks means significant at 99.9% confidence level; two asterisks 99% confidence level; one asterisk 95% confidence level; and a tiny dot 90% confidence level.</td></tr>
</tfoot>
</table>



The below graph plots observed as a function of predicted values, and also the perfect regression line. In general, our model tends to under-predict the homesale price. 


```r
ggplot() + 
  geom_point(data=reg1Attributes, aes(SalePrice, PredictedPrice)) +
  stat_smooth(data=reg1Attributes, aes(SalePrice, SalePrice), method = "lm", se = FALSE, size = 1, colour="#fa725a") + 
  stat_smooth(data=reg1Attributes, aes(SalePrice, PredictedPrice), method = "lm", se = FALSE, size = 1, colour="#b8d1e3") + 
  labs(title="Predicted Sales Price as a function of \nObserved Sales Price",
       subtitle="Perfect prediction in red, Actual prediction in blue; Entire dataset",
       x = 'Home Sale Price',
       y = 'Predicted Price') +
  theme_minimal() +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=10, face="italic", color="grey")) +
  theme(legend.text = element_text(face = "italic"))
```

<img src="markdown_files/figure-html/predicted prices as a function of observed prices-1.png" width="672" />

Nevertheless, the histogram of absolute residuals tells us that the predicted prices our model produces will not differ from the true values by much.


```r
ggplot() +
  geom_histogram(data=reg1Attributes, aes(abs(reg1Attributes$SalePrice - reg1Attributes$PredictedPrice)), fill = '#b8d1e3') +
  labs(title = 'Histogram of residuals (absolute values)',
       subtitle = 'Nashville Home Price Prediction; Entire Dataset',
       x = 'Residuals in absolute values',
       y = 'Count') +
  theme_minimal() +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=10, face="italic", color="grey"))
```

<img src="markdown_files/figure-html/histogram of residuals (absolute values)-1.png" width="672" />

We also map the predicted prices for home sales for the entire dataset. The predicted prices are categorized using quintile breaks, and the distribution across space looks much like the observed ones shown in Section 2.3.


```r
ggmap(map3) +
  geom_sf(data = baseMap, inherit.aes = FALSE, color='grey21', size=0.8) +
  geom_point(data=reg1Attributes, 
             aes(x=WGS1984X, y=WGS1984Y, colour=factor(ntile(PredictedPrice,5))), 
             size = 0.7) + 
  scale_colour_manual(values = c("#d6d6ff","#8f97e3","#556cc9","#244ead","#003994"),
                      labels=as.character(round(quantile(reg1Attributes$PredictedPrice,
                                                         c(.1,.2,.4,.6,.8),na.rm=T))),
                      name="Predicted price \n(Quintile Breaks)") +
  labs(title="Predicted Prices for Home Sales",
       subtitle='Nashville Home Price Prediction; Entire Dataset') +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=11, face="italic", color="grey")) +
  theme(legend.text = element_text(face = "italic"))
```

<img src="markdown_files/figure-html/predicted prices for home sales-1.png" width="672" />

## 4.2. Out-of-sample Prediction{#link10}
As previously mentioned, one of the goals of predictive modeling is generalizability, so we separate the data into two parts: 75% randomly selected training set and 25% remaining test set. The performance of our model is evaluated mainly based on Mean Absolute Error (MAE), and Mean Absolute Percentage Error (MAPE). The former informs us the average difference between predicted prices and observed prices, while the latter the average difference **in percentage** between predicted and observed home sale prices.

For this specific test set, the MAE is slightly less than 90K dollars, and the MAPE is 36%.


```r
kable(table6, align = "l", col.names = c('Statistics', 'R-squared', 'MAE', 'MAPE'),
      caption = 'Out-of-sample Prediction Statistics') %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                font_size = 15,
                full_width = F) %>%
  row_spec(seq(1, nrow(table6), 2), background = "#ecf6fe")
```

<table class="table table-striped table-hover" style="font-size: 15px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Out-of-sample Prediction Statistics</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Statistics </th>
   <th style="text-align:left;"> R-squared </th>
   <th style="text-align:left;"> MAE </th>
   <th style="text-align:left;"> MAPE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> Out-of-Sample Prediction </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.646775 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 89660.27 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.3597092 </td>
  </tr>
</tbody>
</table>


The following graph plots observed as a function of predicted values, and also the perfect regression line. In general, our model tends to under-predict the homesale price.


```r
ggplot() + 
  geom_point(data=reg2Attributes, aes(SalePrice, PredictedPrice)) +
  stat_smooth(data=reg2Attributes, aes(SalePrice, SalePrice), method = "lm", se = FALSE, size = 1, colour="#fa725a") + 
  stat_smooth(data=reg2Attributes, aes(SalePrice, PredictedPrice), method = "lm", se = FALSE, size = 1, colour="#b8d1e3") + 
  labs(title="Predicted Sales Price as a function of \nObserved Sales Price",
       subtitle="Perfect prediction in red, Actual prediction in blue; 25% Test set",
       x = 'Home Sale Price',
       y = 'Predicted Price') +
  theme_minimal() +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=10, face="italic", color="grey")) +
  theme(legend.text = element_text(face = "italic"))
```

<img src="markdown_files/figure-html/predicted prices as a function of observed prices (25 pct)-1.png" width="672" />

Below shows a map of residuals for the 25% randomly selected test set. The purpose of the map is to see whether there are some patterns in space of the residuals. Ideally, the residuals should distribute randomly across space.


```r
ggmap(map3) +
  geom_sf(data = baseMap, inherit.aes = FALSE, color='grey21', size=0.8) +
  geom_point(data=reg2Attributes, 
             aes(x=WGS1984X, y=WGS1984Y, colour=factor(ntile(residuals,5))), 
             size = 0.9) + 
  scale_colour_manual(values = c("#d6d6ff","#8f97e3","#556cc9","#244ead","#003994"),
                      labels=as.character(round(quantile(reg2Attributes$residuals,
                                                         c(.1,.2,.4,.6,.8),na.rm=T))),
                      name="Residuals \n(Quintile Breaks)") +
  labs(title="Residuals in Home Sale Price Prediction",
       subtitle='Nashville Home Price Prediction; 25% Test set') +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=11, face="italic", color="grey")) +
  theme(legend.text = element_text(face = "italic"))
```

<img src="markdown_files/figure-html/residuals for 25 pct randomly selected test set-1.png" width="672" />

However, we think it would be more straightforward if we can use some kind of metrics to evaluate spatial auto-correlation, namely dispersed, random, or clustered. As a result, we compute Moran¡¯s I index of the residuals of the test set. Moran¡¯s I ranges from +1 (clustered) to -1 (dispersed). For our test set here, the p-value is way above 0.05, meaning that spatial auto-correlation of the residuals is not significant.


```r
kable(table7, align = "l", col.names = c('Moran I Statistic', 'Expectation', 'Variance', 'P value'),
      caption = "Moran's I test") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = F,
                font_size = 15) %>%
  row_spec(seq(1, nrow(table7), 2), background = "#ecf6fe")
```

<table class="table table-striped table-hover" style="font-size: 15px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Moran's I test</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Moran I Statistic </th>
   <th style="text-align:left;"> Expectation </th>
   <th style="text-align:left;"> Variance </th>
   <th style="text-align:left;"> P value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.0023728 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> -0.0004751 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.0001981 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.4198 </td>
  </tr>
</tbody>
</table>

## 4.3. Cross-Validation{#link11}
Cross-validation allows us to see how generalizable our model is to a number of random samples, instead of just one sample in the previous section. Here, we used an algorithm called '100-fold cross-validation'. This methodology allows us to first partition the entire data frame into 100 equally sized subsets, hold out one of those subsets as the test set, train the model using the remaining 99 subsets, predict for the hold out subset, and record a goodness of fit metric. The average of the goodness of fit metrics across all 100 folds will also be generated.

The mean of the R^2 here is 0.66, indicating generally 66% of variation of homesale price can be explained by our independent variables. The MAE of 0.29 indicates that the average difference between predicted and observed prices is 0.29 after log transformation.


```r
kable(table8, align = "l", col.names = c('RMSE', 'Mean of R-squared', 'SD of R-squared', 'Mean of MAE', 'SD of MAE'),
      caption = 'Results of Cross-Validation Tests') %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                font_size = 16,
                full_width = F) %>%
  row_spec(seq(1, nrow(table8), 2), background = "#ecf6fe")
```

<table class="table table-striped table-hover" style="font-size: 16px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Results of Cross-Validation Tests</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> RMSE </th>
   <th style="text-align:left;"> Mean of R-squared </th>
   <th style="text-align:left;"> SD of R-squared </th>
   <th style="text-align:left;"> Mean of MAE </th>
   <th style="text-align:left;"> SD of MAE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.4324231 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.6630223 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.0855336 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.2924221 </td>
   <td style="text-align:left;background-color: #ecf6fe;"> 0.0321642 </td>
  </tr>
</tbody>
</table>

The following histogram displays the distribution of R^2 and MAE across all 100 folds. What might worth noticing is that there are two samples in which the R^2 equals approximately to 0.4. Our model does not perform very well in these subsets. Beyond that, the 100 R-squared values are distributed mainly from 0.65 to 0.75. As for the distribution of MAE, it is very close to normal, but we think it would be better if the distribution can be more condensed.


```r
ggplot(as.data.frame(lmFit$resample), aes(Rsquared)) + 
  geom_histogram(bins=20, fill='#b8d1e3') +
  labs(title = '100-Fold Cross-Validation R-squared',
       subtitle = 'Nashville Home Price Prediction',
       x="R-squared (coefficient of determination)",
       y="Count") +
  theme_minimal() +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=10, face="italic", color="grey"))

ggplot(as.data.frame(lmFit$resample), aes(MAE)) + 
  geom_histogram(bins=20, fill='#b8d1e3') +
  labs(title = '100-Fold Cross-Validation MAE',
       subtitle = 'Nashville Home Price Prediction',
       x="Mean Absolute Error",
       y="Count") +
  theme_minimal() +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=10, face="italic", color="grey"))
```

<img src="markdown_files/figure-html/histogram of 100-fold cv R^2-1.png" width="50%" /><img src="markdown_files/figure-html/histogram of 100-fold cv R^2-2.png" width="50%" />


## 4.4. Generalizability across space{#link12}
Other than generalizability across data samples, as spatial analysts, we also care about generalizability across space. Here, we choose zipcode districts as our unit of analysis, and analyze whether the model is generalizable across different zipcodes.

The map below illustrates the mean absolute percentage error by zip code using the above 25% test set. The lighter the color is, the better our model performs. Those 'empty' zipcodes are the ones without any home sales. Admittedly, the MAPE for some zipcodes are relatively high, and there is one whose MAPE is over 1, but we think this might result from the small sample size. In a later section we will see our model is able to perform similarly well in three zipcodes with different income level.


```r
ggmap(map3) + 
  geom_sf(data=Nashville_Zipcodes, inherit.aes = FALSE, fill=NA, color='grey21', size=0.8) +
  geom_sf(data=reg2Residuals_Summary, inherit.aes = FALSE, aes(fill=factor(ntile(MAPE,5))), alpha=0.95, colour='grey21') +
  labs(title='Mean Absolute Percentage Error (MAPE) \nby Zipcode',
       subtitle='Nashville Home Price Prediction; 25% Test set',
       x = 'lon', 
       y = 'lat') +
  scale_fill_manual(values = c("#d6d6ff","#8f97e3","#556cc9","#244ead","#003994"),
                      labels=as.character(round(quantile(reg2Residuals_Summary$MAPE,
                                                         c(.1,.2,.4,.6,.8),na.rm=T),3)),
                      name="MAPE \n(Quintile Breaks)") +
  theme_minimal() +
  theme(plot.title=element_text(size=14, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=10, face="italic", color="grey")) +
  theme(legend.text = element_text(face = "italic"))
```

<img src="markdown_files/figure-html/MAPE by zipcode map-1.png" width="672" style="display: block; margin: auto;" />




```r
kable(table9, align = "r", col.names = c('Zipcode', 'MAPE', 'Mean Price', 'Count'),
      caption = 'MAPE and Mean Price by Zipcode') %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                font_size = 15,
                full_width = F) %>%
  row_spec(seq(1, nrow(table9), 2), background = "#ecf6fe")
```

<table class="table table-striped table-hover" style="font-size: 15px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">MAPE and Mean Price by Zipcode</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> Zipcode </th>
   <th style="text-align:right;"> MAPE </th>
   <th style="text-align:right;"> Mean Price </th>
   <th style="text-align:right;"> Count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;background-color: #ecf6fe;"> 37013 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 0.4112279 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 185720.4 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 279 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 37027 </td>
   <td style="text-align:right;"> 0.1826943 </td>
   <td style="text-align:right;"> 356831.3 </td>
   <td style="text-align:right;"> 32 </td>
  </tr>
  <tr>
   <td style="text-align:right;background-color: #ecf6fe;"> 37115 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 0.3055312 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 140237.9 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 86 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 37189 </td>
   <td style="text-align:right;"> 0.1155155 </td>
   <td style="text-align:right;"> 76000.0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;background-color: #ecf6fe;"> 37201 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 0.1282969 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 270200.0 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 37203 </td>
   <td style="text-align:right;"> 0.5823492 </td>
   <td style="text-align:right;"> 647729.6 </td>
   <td style="text-align:right;"> 79 </td>
  </tr>
  <tr>
   <td style="text-align:right;background-color: #ecf6fe;"> 37204 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 0.1969529 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 477326.2 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 71 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 37205 </td>
   <td style="text-align:right;"> 0.2502283 </td>
   <td style="text-align:right;"> 513683.4 </td>
   <td style="text-align:right;"> 101 </td>
  </tr>
  <tr>
   <td style="text-align:right;background-color: #ecf6fe;"> 37206 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 0.3292464 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 355953.1 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 174 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 37207 </td>
   <td style="text-align:right;"> 0.4982391 </td>
   <td style="text-align:right;"> 192123.9 </td>
   <td style="text-align:right;"> 110 </td>
  </tr>
  <tr>
   <td style="text-align:right;background-color: #ecf6fe;"> 37208 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 0.4096702 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 345009.3 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 106 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 37209 </td>
   <td style="text-align:right;"> 0.3796108 </td>
   <td style="text-align:right;"> 374519.8 </td>
   <td style="text-align:right;"> 183 </td>
  </tr>
  <tr>
   <td style="text-align:right;background-color: #ecf6fe;"> 37210 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 1.0885374 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 213149.3 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 37211 </td>
   <td style="text-align:right;"> 0.2650309 </td>
   <td style="text-align:right;"> 196269.3 </td>
   <td style="text-align:right;"> 202 </td>
  </tr>
  <tr>
   <td style="text-align:right;background-color: #ecf6fe;"> 37212 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 0.2182612 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 515824.5 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 78 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 37214 </td>
   <td style="text-align:right;"> 0.3965312 </td>
   <td style="text-align:right;"> 211690.2 </td>
   <td style="text-align:right;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:right;background-color: #ecf6fe;"> 37215 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 0.2150284 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 563155.0 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 107 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 37216 </td>
   <td style="text-align:right;"> 0.3180348 </td>
   <td style="text-align:right;"> 258569.7 </td>
   <td style="text-align:right;"> 168 </td>
  </tr>
  <tr>
   <td style="text-align:right;background-color: #ecf6fe;"> 37217 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 0.3430785 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 143684.4 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 109 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 37218 </td>
   <td style="text-align:right;"> 0.7846816 </td>
   <td style="text-align:right;"> 144957.0 </td>
   <td style="text-align:right;"> 28 </td>
  </tr>
  <tr>
   <td style="text-align:right;background-color: #ecf6fe;"> 37219 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 0.2673158 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 462900.0 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 37220 </td>
   <td style="text-align:right;"> 0.1690711 </td>
   <td style="text-align:right;"> 414377.5 </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
  <tr>
   <td style="text-align:right;background-color: #ecf6fe;"> 37221 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 0.1373411 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 244347.4 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 24 </td>
  </tr>
</tbody>
</table>


We also plot the MAPE as a function of mean price by zipcode. The regression line is not so steep, indicating the performance of our model would not vary much across zipcodes. But it is true that for districts with a higher average home price, the model is likely to perform better.


```r
ggplot(reg2Attributes %>%
         group_by(LocationZi) %>%
         summarize(MAPE = mean(percentAbsError),
                   Mean_Price = mean(SalePrice))) +
  geom_point(aes(Mean_Price, MAPE)) +
  stat_smooth(aes(Mean_Price, MAPE), method = "lm", se = FALSE, size = 0.7, colour="#003994") +
  labs(title='Mean Absolute Percentage Error (MAPE) \nas function of Mean Price',
       subtitle='Nashville Home Price Prediction; 25% Test set',
       x = 'Mean Price', 
       y = 'Mean Absolute Percentage Error (MAPE)') +
  theme_minimal() +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=10, face="italic", color="grey")) +
  theme(legend.text = element_text(face = "italic"))
```

<img src="markdown_files/figure-html/MAPE as a function of mean price by zipcode-1.png" width="672" />

Lastly, we perform a 'spatial cross-validation', in which we remove the homesales in zipcodes with three different income level once at a time. Each time we train the model using the remaining zipcodes, and predict for the holdout one. The analysis will help identify whether the model is generalizable across different districts.

For the relatively rich zipcode, we select TN 37205, and we consider 37206 a middle-income district. When it comes to the poor zipcode, we pick 37211.

Specifically, we first filter the according training and test sets from the original homesale dataset.

```r
## Rich Zipcode
vars_worich_train <-
  vars_train %>%
  filter(LocationZi != '37205')

vars_worich_test <-
  vars_train %>%
  filter(LocationZi == '37205')

## Middle-income Zipcode
vars_womid_train <-
  vars_train %>%
  filter(LocationZi != '37206')

vars_womid_test <-
  vars_train %>%
  filter(LocationZi == '37206')

## Low-income Zipcode
vars_wopoor_train <-
  vars_train %>%
  filter(LocationZi != '37211')

vars_wopoor_test <-
  vars_train %>%
  filter(LocationZi == '37211')
```

And then we train the model using each training set.

```r
## Rich Zipcode
reg3 <- lm(log_SalePrice ~ age_factor + bedroom_factor + baths_factor +
             building_factor + log_sf_finis_1 + log_Acrage + offsite + 
             log_MdValue + Pctwhite + log_Density + log_Public_Art_Collection_20 +  
             log_garden_10 + log_retail2_10 + BID + log_home_20 +
             school_white_10 +school_econ_15 + 
             log_price_5 + log_price_10 + log_avg_price_10, 
           data = vars_worich_train)

## Middle-income Zipcode
reg4 <- lm(log_SalePrice ~ age_factor + bedroom_factor + baths_factor +
             building_factor + log_sf_finis_1 + log_Acrage + offsite + 
             log_MdValue + Pctwhite + log_Density + log_Public_Art_Collection_20 +  
             log_garden_10 + log_retail2_10 + BID + log_home_20 +
             school_white_10 +school_econ_15 + 
             log_price_5 + log_price_10 + log_avg_price_10, 
           data = vars_womid_train)

## Low-income Zipcode
reg5 <- lm(log_SalePrice ~ age_factor + bedroom_factor + baths_factor +
             building_factor + log_sf_finis_1 + log_Acrage + offsite + 
             log_MdValue + Pctwhite + log_Density + log_Public_Art_Collection_20 +  
             log_garden_10 + log_retail2_10 + BID + log_home_20 +
             school_white_10 +school_econ_15 + 
             log_price_5 + log_price_10 + log_avg_price_10,
           data = vars_wopoor_train)
```

Based on the regression results, we next construct three new dataframes. These dataframes will be used to calculate the MAE and MAPE statistics.

```r
## Rich Zipcode
reg3Pred <- exp(predict(reg3, vars_worich_test))

reg3PredValues <- 
  data.frame(observedSales = vars_worich_test$SalePrice,
             predictedSales = reg3Pred)

reg3PredValues <-
  reg3PredValues %>%
  mutate(error = predictedSales - observedSales) %>%
  mutate(absError = abs(predictedSales - observedSales)) %>%
  mutate(percentAbsError = abs(predictedSales - observedSales) / observedSales) %>%
  mutate(Legend = 'High-Income')

## Middle-income Zipcode
reg4Pred <- exp(predict(reg4, vars_womid_test))

reg4PredValues <- 
  data.frame(observedSales = vars_womid_test$SalePrice,
             predictedSales = reg4Pred)

reg4PredValues <-
  reg4PredValues %>%
  mutate(error = predictedSales - observedSales) %>%
  mutate(absError = abs(predictedSales - observedSales)) %>%
  mutate(percentAbsError = abs(predictedSales - observedSales) / observedSales) %>%
  mutate(Legend = 'Middle-Income')

## Low-income Zipcode
reg5Pred <- exp(predict(reg5, vars_wopoor_test))

reg5PredValues <- 
  data.frame(observedSales = vars_wopoor_test$SalePrice,
             predictedSales = reg5Pred)

reg5PredValues <-
  reg5PredValues %>%
  mutate(error = predictedSales - observedSales) %>%
  mutate(absError = abs(predictedSales - observedSales)) %>%
  mutate(percentAbsError = abs(predictedSales - observedSales) / observedSales) %>%
  mutate(Legend = 'Low-Income')
```


```r
## Rich Zipcode
mean(reg3PredValues$absError) # MAE
mean(reg3PredValues$percentAbsError) # MAPE

## Middle-income Zipcode
mean(reg4PredValues$absError) # MAE
mean(reg4PredValues$percentAbsError) # MAPE

## Low-income Zipcode
mean(reg5PredValues$absError) # MAE
mean(reg5PredValues$percentAbsError) # MAPE
```


Below shows a table of MAPE and MAE for each of the three holdout zipcodes. Generally speaking, the model is quite generalizable across different zipcodes. The MAPE ranges from 0.269 in rich zipcodes, to 0.327 in poor zipcodes, which is not a big gap.


```r
kable(table10, col.names = c('Holdout Zipcode', 'MAE', 'MAPE'),
      caption = 'MAPE and MAE for each holdout zipcodes') %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = F,
                font_size = 16) %>%
  row_spec(seq(1, nrow(table10), 2), background = "#ecf6fe")
```

<table class="table table-striped table-hover" style="font-size: 16px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">MAPE and MAE for each holdout zipcodes</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Holdout Zipcode </th>
   <th style="text-align:right;"> MAE </th>
   <th style="text-align:right;"> MAPE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> Rich </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 146376.59 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 0.2690282 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Middle-income </td>
   <td style="text-align:right;"> 84583.70 </td>
   <td style="text-align:right;"> 0.3198609 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #ecf6fe;"> Poor </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 40985.82 </td>
   <td style="text-align:right;background-color: #ecf6fe;"> 0.3273834 </td>
  </tr>
</tbody>
</table>


As for the faceted scatterplot, please notice that when we build the above three new datasets, we include a column called 'Legend'. This makes it convenient for us to plot the faceted scatterplot.


```r
Compare_Neighborhoods <- rbind(reg3PredValues[, c('Legend', 'observedSales', 'predictedSales')],
                               reg4PredValues[, c('Legend', 'observedSales', 'predictedSales')],
                               reg5PredValues[, c('Legend', 'observedSales', 'predictedSales')])

ggplot(Compare_Neighborhoods) +
  geom_point(aes(observedSales, predictedSales)) + 
  stat_smooth(aes(observedSales, predictedSales), method = 'lm', se = F, color = '#b8d1e3') +
  stat_smooth(aes(observedSales, observedSales), method = 'lm', se = F, color = '#fa725a') +
  facet_wrap(~Legend, ncol=2) +
  labs(title='Scatterplot of Predicted vs. Observed Home Sale Prices \nin Neighborhoods with Different Income Level',
       subtitle='Perfect prediction in red, Actual prediction in blue',
       x = 'Observed Prices', 
       y = 'Predicted Prices') +
  theme_minimal() +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=10, face="italic", color="grey")) +
  theme(legend.text = element_text(face = "italic"))
```



```r
ggplot(Compare_Neighborhoods) +
  geom_point(aes(observedSales, predictedSales)) + 
  stat_smooth(aes(observedSales, predictedSales), method = 'lm', se = F, color = '#b8d1e3') +
  stat_smooth(aes(observedSales, observedSales), method = 'lm', se = F, color = '#fa725a') +
  facet_wrap(~Legend, ncol=2) +
  labs(title='Scatterplot of Predicted vs. Observed Home Sale Prices \nin Neighborhoods with Different Income Level',
       subtitle='Perfect prediction in red, Actual prediction in blue',
       x = 'Observed Prices', 
       y = 'Predicted Prices') +
  theme_minimal() +
  theme(plot.title=element_text(size=16, face="bold", vjust=-1)) +
  theme(plot.subtitle=element_text(size=10, face="italic", color="grey")) +
  theme(legend.text = element_text(face = "italic"))
```

<img src="markdown_files/figure-html/scatterplot of predicted vs. observed for the three neighborhoods-1.png" width="672" />

***

# 5. Discussion{#link13}

Generally speaking, we believe our model is an effective one, because it¡¯s both relatively accurate and generalizable. First of all, the model could account for around 65.2% of variations for home sale prices. In addition, almost every single independent variable in our model is statistically significant at least at 95% confidence level, suggesting the reliable predicting power of the model. Regarding generalizability, the cross-validation technique suggests that the home prices (in log format) our model produces are approximately 0.29 different from the true values. Other indicators such as the low standard deviation of R-squared (0.085) in the 100-fold cross-validation test also indicates our model is able to perform well in most subsets. As spatial analysts, we also care about generalizability across space. According to Moran¡¯s I test, there is no significant spatial auto-correlation in the model. The model performs similarly well across zip codes with different levels of wealth as well. Therefore, it¡¯s reasonable to consider our final model useful to predict home prices in Nashville, Tennessee.

During data wrangling, we did find some interesting variables. First, we surprisingly found that the distance to nearest 20 public art collections is a significant predictor of home prices. A culture of art appreciation might have formed in Nashville. Thus, proximity to art collections adds value to homes. Public art locations cluster in the core of the city, helping explain the relatively high sales prices there. Second, the total finished area with a negative adjustment amount in case the computer sketch routine is not precise enough is a powerful predicting factor. This indicates that a more accurate home area is helpful when considering the impact on home prices. Third, the proportion of economically disabled students in nearest 15 public schools has negative influence on home prices. The reason might be that parents believe the better the economic conditions of students¡¯ families, the better the teaching quality of the schools and academic performance of students.

Some independent variables in our model are more effective in terms of significant level. In internal characteristics, whether the home is single-family home, total finished area with an adjustment, lot size, and whether the owner is offsite are more important than others. Generally, the larger the home and land, the higher the sales price. Being a single-family home adds value to homes, which is probably because of home buyers' preference for privacy. Having an offsite owner, on the other hand, leads to the decrease of home sales price perhaps due to some inconvenience brought by it. As for demographic predictors, median home values in block groups and proportion of white population are more important in predicting home prices. When it comes to amenities and public services, as discussed above, distance to public art collections plays a very important role. All spatial lag variables are essential and significant at 99.9% level in predicting home sales prices. These predictors also have larger coefficients. This is easy to understand because almost every buyer would take nearby prices as a reference for surrounding environment.

To examine the effectiveness and generalizability of our model, we conducted a 25% randomly selected out-of-sample test and a 100-fold cross-validation test. In the 25% test set, the average difference between predicted prices and observed prices (MAE) is slightly less than $90k, and the average difference in percentage between these two (MAPE) is 36%. Shown by the cross-validation test, the average difference between predicted and observed home prices is 0.29 after log transformation. The low standard deviations of both MAE and R-squared are another proof of the model¡¯s ability to perform well in most cases. Consequently, we are confident on our model¡¯s generalizability across different datasets. 

When it comes to spatial generalizability, the Moran¡¯s I test for the residuals shows that there is no significant spatial auto-correlation here. Admittedly, during out-of-sample prediction, the prediction errors in a few zip code districts are indeed relatively high, but it may just result from the small housing sample size. The later ¡®spatial cross-validation¡¯ proves the model¡¯s generalizability across space. Specifically, the average difference between predicted and observed home prices (in log format) ranges from 0.269 in wealthy zip codes, to 0.327 in poor zip codes, which is not a big gap. That said, we still cannot ignore the fact that our model performs better in where the mean price of houses or individuals¡¯ income level is higher. This suggests the model is likely still missing some factors associated with the prices of lower-income neighborhood houses.


***

# 6. Conclusion{#link14}

We would recommend our model to Zillow in their future home price analysis and prediction, not only because our model is able to explain the majority of variation of home sale prices, but also its generalizability across both different datasets and space. However, it does not mean there is no room for improvement. As previously mentioned, we should try to find and introduce more effective factors correlated with prices of homes in poorer districts to better the model¡¯s performance and make the model more generalizable across the entire city. In addition,  it would be beneficial for us to get the exact year when the houses were sold. The reason is that almost every economic phenomenon has something to do with time.

***

# APPENDIX{#link15}
This section includes how the data used in the above analyses are collected and processed. The original dataset called "train.and.test_student.csv" contains several fields of homesales characteristics.

We first wrangle the internal characteristics as follows.


```r
dat <- read.csv('train.and.test_student.csv')

## Internal Characteristics
dat <-
  dat %>%
  mutate(effyearbuilt_building = ifelse(effyearbuilt_building==0, NA, effyearbuilt_building),
         sf_finished_less_ifla = ifelse(sf_finished_less_ifla==0, NA, sf_finished_less_ifla),
         Acrage = ifelse(Acrage == 0, NA, Acrage)) %>%
  mutate(effyearbuilt_building = replace_na(effyearbuilt_building, mean(effyearbuilt_building, na.rm = T)),
         sf_finished_less_ifla = replace_na(sf_finished_less_ifla, mean(sf_finished_less_ifla, na.rm = T)),
         Acrage = replace_na(Acrage, mean(Acrage, na.rm = T))) %>%
  mutate(age = 2018 - effyearbuilt_building) %>%
  mutate(age_factor = ifelse(age < 5, 0, 
                             ifelse(age <= 23, 1, 
                                    ifelse(age <= 38, 2, 3))),
         bedroom_factor = ifelse(bedroomsunits_building < 3, 0, 
                                 ifelse(bedroomsunits_building == 3, 1, 2)),
         baths_factor = ifelse(baths == 0, 0,
                               ifelse(baths == 1, 1, 2)),
         building_factor = ifelse(LandUseFullDescription == 'SINGLE FAMILY', 0, 1),
         log_sf_finis_1 = log(sf_finished_less_ifla),
         log_Acrage = log(Acrage),
         offsite = ifelse(as.character(OwnerAddress1) == as.character(LocationAddress), 0, 1)) %>%
  st_as_sf(coords = c("WGS1984X", "WGS1984Y"), crs = 4326, agr = "constant") %>%
  st_transform(crs = 2915) %>%
  select(kenID, SalePrice, age_factor, bedroom_factor, baths_factor, 
         building_factor, log_sf_finis_1, log_Acrage, offsite, test)
```

Demographic data are acquired from [American FactFinder](https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml). To let each house know the information of the census block group it is located in, we first intersect the homesales and the block group layers, and then join the census dataframe to the product.


```r
## Demographic
census <- 
  read.csv('census.csv', stringsAsFactors = F) %>%
  mutate(GEOID = as.character(FIPS)) %>%
  select(GEOID, Md_Value, Pctwhite, Density)

block_group <- 
  read_sf('shp/block_group.shp') %>%
  select(GEOID) %>%
  st_transform(crs = 2915)

dat <-
  st_join(dat, block_group, join = st_intersects) %>%
  left_join(., census, by = c('GEOID' = 'GEOID'))
```

Regarding accessibility to public services, we calculate the average distance to a certain number of amenities. The below code block shows how the average distance to the nearest 20 public art collections (log-transformed), average distance to the nearest 10 gardens (log-transformed), average distance to the nearest 10 retail land use (log-transformed), average distance to the nearest Business Improvement District, and average distance to the nearest 20 properties (log-transformed) are measured.


```r
nn_function <- function(measureFrom,measureTo,k) {
  
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint)
  
  return(output)  
}

## Public Services
public_art <- 
  read_sf('shp/Public_Art_Collection.shp') %>%
  st_transform(crs = 2915)

art.xy <- 
  public_art %>%
  cbind(.,st_coordinates(st_centroid(public_art)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

retail2 <-
  add_osm_feature(opq = opq(bbox = c(-87.054764,35.967785,-86.515587,36.405495)), 
                  key = 'building', 
                  value = 'retail') %>%
  osmdata_sf()

retail2 <- 
  st_geometry(retail2$osm_points) %>%
  st_transform(crs = 2915) %>%
  st_sf()

retail.xy <- 
  retail2 %>%
  cbind(.,st_coordinates(st_centroid(retail2)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

garden <-
  add_osm_feature(opq = opq(bbox = c(-87.054764,35.967785,-86.515587,36.405495)), 
                  key = 'leisure', 
                  value = 'garden') %>%
  osmdata_sf()

garden <- 
  st_geometry(garden$osm_points) %>%
  st_transform(crs = 2915) %>%
  st_sf()

garden.xy <- 
  garden %>%
  cbind(.,st_coordinates(st_centroid(garden)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

BID <- 
  read_sf('shp/BID_point.shp') %>%
  st_transform(crs = 2915)

BID.xy <- 
  BID %>%
  cbind(.,st_coordinates(st_centroid(BID)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

home.xy <-
  dat %>%
  cbind(.,st_coordinates(st_centroid(dat)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()


distArt_20 <- 
  as.data.frame(nn_function(home.xy, art.xy, 20)) %>%
  mutate(log_Public_Art_Collection_20 = log(pointDistance)) %>%
  select(log_Public_Art_Collection_20)

distGarden_10 <- 
  as.data.frame(nn_function(home.xy, garden.xy, 10)) %>%
  mutate(log_garden_10 = log(pointDistance)) %>%
  select(log_garden_10)

distRetail_10 <- 
  as.data.frame(nn_function(home.xy, retail.xy, 10)) %>%
  mutate(log_retail2_10 = log(pointDistance)) %>%
  select(log_retail2_10)

distBID_1 <- 
  as.data.frame(nn_function(home.xy, BID.xy, 1)) %>%
  mutate(BID = log(pointDistance)) %>%
  select(BID)

distHome_20 <-
  as.data.frame(nn_function(home.xy, home.xy, 21)) %>%
  mutate(log_home_20 = log(pointDistance * 21 / 20)) %>%
  select(log_home_20)

vars <-
  dat %>%
  st_set_geometry(NULL) %>%
  cbind(., distArt_20, distGarden_10, distRetail_10, distBID_1, distHome_20) %>%
  select(-GEOID)
```

Lastly, there are a few variables that can be more conveniently calculated in ArcGIS. For them, we import the dataframe to R, and the final dataset used to train our model is generated as well.

```r
## Other Variables
GIS <- read.csv('GIS_output.csv')

vars <-
  left_join(vars, GIS) %>%
  mutate(log_MdValue = log(Md_Value),
         log_Density = log(Density)) %>%
  select(-Md_Value, -Density)
```

Below shows the codes for generating various tables in the report.

```r
table1 <- data.frame('Variable' = c('Dependent Variable', rep('Independent Variable', 20)),
                    'Category' = c('-----', 
                                   rep('Internal Characteristics', 7), 
                                   rep('Demographic Predictors', 3),
                                   rep('Amenities/Public Services', 7),
                                   rep('Spatial Lag', 3)),
                    'Symbol' = c('log_SalePrice', 'age_factor', 'bedroom_factor',
                                 'baths_factor', 'building_factor', 'log_sf_finis_1',
                                 'log_Acrage', 'offsite', 'log_MdValue', 'Pctwhite',
                                 'log_Density', 'log_Public_Art_Collection_20', 'log_garden_10',
                                 'log_retail2_10', 'BID','log_home_20', 'school_white_10',
                                 'school_econ_15', 'log_price_5', 'log_price_10',
                                 'log_avg_price_10'),
                    'Meaning' = c('Sale Price of the property (log-transformed)',
                                  '2018 minus the effective year the property was built; 0 for age < 5; 1 for age 5-23; 2 for age 24-38; 3 for the others',
                                  '0 for properties whose number of bedrooms less than 3; 1 for those equal 3; 2 for the others',
                                  '0 for properties whose number of bathrooms equal 1; 1 for those equal 2; 2 for the others',
                                  '0 for single family; 1 for the others',
                                  'Total Finished area less an adjustment (log-transformed)',
                                  'Acres of land (log-transformed)',
                                  'Whether the owner of the properties is offsite or not. 0 for onsite; 1 for offsite',
                                  'Median value of owner-occupied housing units in census block group (log-transformed)',
                                  'Percentage of white population in census block group',
                                  'Population density in census block group (log-transformed)',
                                  'Distance to nearest 20 public art collections (log-transformed)',
                                  'Distance to nearest 10 gardens (log-transformed)',
                                  'Distance to nearest 10 retail land use (log-transformed)',
                                  'Distance to nearest Business Improvement District',
                                  'Distance to nearest 20 properties (log-transformed)',
                                  'Proportion of white students in nearest 10 schools',
                                  'Proportion of economically disabled students in nearest 15 public schools',
                                  'Average sale price of nearby 5 properties (log-transformed)',
                                  'Average sale price of nearby 10 properties (log-transformed)',
                                  'Average price per square foot of nearby 10 properties (log-transformed)'))

table2 <- data.frame('Variable' = c('SalePrice', 'sf_finis_1', 'Acrage', 'MdValue', 'Pctwhite', 'Density',
                                     'Public_Art_Collection_20', 'garden_10', 'retail2_10', 'BID', 
                                    'home_20', 'school_white_10', 'school_econ_15', 'price_5',
                                    'price_10', 'avg_price_10'),
                     'N' = rep(8431, 16),
                     'Mean' = c('312,163.300', '1,848.817', '0.355', '227,860.600', '0.648', '3,466.940',
                                '25,863.690', '9.288', '8.712', '27,776.980', '891.221', '0.276', '0.467',
                                '288,578.000', '316,894.300', '175.066'),
                     'SD' = c('308,152.200', '878.403', '0.275', '141,485.300', '0.248', '2,052.780',
                              '15,428.200', '0.995', '0.684', '16,299.560', '494.818', '0.125', '0.092',
                              '239,454.800', '246,583.800', '118.146'),
                     'Min' = c('2,000', '348', '0.030', '58,600', '0.000', '160.102', '4,263.141', '3.581',
                               '5.399', '1,361.014', '47.113', '0.030', '0.199', '10,500', '38,357.800',
                               '16.221'),
                     'Pctl(25)' = c('150,000', '1,247', '0.210', '136,200', '0.480', '1,967.141', '13,570.810',
                                    '8.673', '8.349', '14,719.020', '547.953', '0.199', '0.415', '146,841.7',
                                    '163,847.400', '108.503'),
                     'Pctl(75)' = c('376,340', '2,189', '0.378', '295,700', '0.839', '4,612.103', '37,949.010', 
                                    '9.995', '9.194', '40,207.290', '1,108.266', '0.338', '0.528', '352,940',
                                    '390,316.100', '208.921'),
                     'Max' = c('6,894,305', '10,608', '8.160', '1,840,300', '1.000', '23,124.220', '71,686.350',
                               '10.983', '10.308', '75,448.300', '5,337.407', '0.638', '0.677', '4,839,271', 
                               '3,659,410.000', '1,842.748'))

table3 <- data.frame('Variable' = c('age_factor', 'bedroom_factor', 'baths_factor', 'building_factor', 'offsite'),
                     'Level 0' = c('2355', '2465', '2404', '5972', '4810'),
                     'Level 1' = c('2436', '4382', '4141', '2459', '3621'),
                     'Level 2' = c('1409', '1584', '1886', '---', '---'),
                     'Level 3' = c('2231', '---', '---', '---', '---'))

table4 <- data.frame('Statistics' = c('In-Sample Prediction'),
                     'R-Squared' = 0.6533,
                     'Adjusted R-squared' = 0.6523,
                     'F-statistic' = 659.9,
                     'DF' = 8406,
                     'P-value' = '<0.00000000000000022')

table5 <- data.frame('Variable' = c('(Intercept)', 'age_factor1', 'age_factor2', 'age_factor3', 'bedroom_factor1',
                                    'bedroom_factor2', 'baths_factor1', 'baths_factor2', 'building_factor1',
                                    'log_sf_finis_1', 'log_Acrage', 'offsite1', 'log_MdValue', 'Pctwhite',
                                    'log_Density', 'log_Public_Art_Collection_20', 'log_garden_10','log_retail2_10',
                                    'BID', 'log_home_20', 'school_white_10', 'school_econ_15',
                                    'log_price_5', 'log_price_10', 'log_avg_price_10'),
                     'Estimate' = c('5.520039904', '0.029189132', '-0.029688169', '-0.062303025', '0.037865956',
                                    '0.058217800', '0.039071297', '0.056884030', '-0.085276317', '0.452316073',
                                    '0.104325338', '-0.245366643', '0.134560988', '0.101718482', '0.016311631',
                                    '-0.163120732', '-0.288138335', '-0.144331131', '0.000002784', '-0.080611627',
                                    '0.222161042', '-0.280006557', '0.163514182', '0.118961604', '0.291349995'),
                     'Std.Error' = c('0.465047023', '0.015071681', '0.018075049', '0.017940115', '0.014410136',
                                     '0.020665511', '0.015001192', '0.020928092', '0.014377086', '0.021971620',
                                     '0.010523314', '0.010027412', '0.017406555', '0.029350251', '0.008005077',
                                     '0.034379077', '0.070734516', '0.064192224', '0.000001272', '0.011951937',
                                     '0.097758249', '0.122084984', '0.017292883', '0.029291103', '0.027442965'),
                     't value' = c('11.870', '1.937', '-1.642', '-3.473', '2.628', '2.817', '2.605', '2.718',
                                   '-5.931', '20.586', '9.914', '-24.470', '7.730', '3.466', '2.038', '-4.745',
                                   '-4.074', '-2.248', '2.189', '-6.745', '2.273', '-2.294', '9.456', '4.061',
                                   '10.617'),
                     'P-Value' = c('< 0.0000000000000002 ***', ' 0.052817 .', '0.100525', '0.000518 ***', '0.008611 **', 
                                   '0.004857 **', '0.009216 **', '0.006580 **', '0.000000003122282 ***', '< 0.0000000000000002 ***',
                                    '< 0.0000000000000002 ***', '< 0.0000000000000002 ***', '0.000000000000012 ***', '0.000532 ***',
                                    '0.041615 *', '0.000002121894355 ***', '0.000046733645839 ***', '0.024575 *',
                                    '0.028651 *', '0.000000000016353 ***', '0.023078 *', '0.021842 *', 
                                   '< 0.0000000000000002 ***', '0.000049231575889 ***', '< 0.0000000000000002 ***'))

table6 <- data.frame('Statistics' = c('Out-of-Sample Prediction'),
                     'R-Squared' = 0.646775,
                     'MAE' = mean(reg2PredValues$absError),
                     'MAPE' = mean(reg2PredValues$percentAbsError))

table7 <- data.frame('Moran I Statistic' = 0.0023728215,
                     'Expectation' = -0.0004750594,
                     'Variance' = 0.0001981323,
                     'P-value' = 0.4198)

table8 <- data.frame('RMSE' = 0.4324231,
                     'Mean of R-squared' = 0.6630223,
                     'SD of R-squared' = 0.08553356,
                     'Mean of MAE' = 0.2924221,
                     'SD of MAE' = 0.03216418)

table9 <- data.frame('Zipcode' = reg2Residuals_Summary$LocationZi,
                     "MAPE" = reg2Residuals_Summary$MAPE,
                     'Mean Price' = reg2Residuals_Summary$Mean_Price,
                     'Count' = reg2Residuals_Summary$Count)

table10 <- data.frame('Holdout Neighborhood' = c('Rich', 'Middle-income', 'Poor'),
                      'MAE' = c(mean(reg3PredValues$absError), 
                                mean(reg4PredValues$absError), 
                                mean(reg5PredValues$absError)),
                      'MAPE' = c(mean(reg3PredValues$percentAbsError), 
                                 mean(reg4PredValues$percentAbsError), 
                                 mean(reg5PredValues$percentAbsError)))
```










