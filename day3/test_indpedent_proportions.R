library(tidyverse)
library(freqtables)
## read in the data
bw_data <- read.csv("birthweight2.csv")

## compare proportion lbw and sex
##lbw=0 , normal birthweight and lbw=1 normals
tbl_sex <- table( bw_data$sex,bw_data$lbw2 )
tbl_sex

prop.table(tbl_sex , margin = 1)
prop.table(tbl_sex , margin = 2)

## test of proportion
## we observer no significant difference (pvalue)
prop.test(tbl_sex)

## the confidence intervals overlap
bw_data %>% 
  ## create the frequency table
  freq_table(sex, lbw2) %>%
  ## filter only low birthweights
  filter(col_cat==0) %>%
  ## select only variables containing row
  select(contains('row'))
