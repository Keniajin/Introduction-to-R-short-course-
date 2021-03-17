
library(tidyverse)

## read in the data
bw_data <- read.csv("birthweight2.csv")

## compare proportion lbw and sex
##lbw=0 , normal birthweight and lbw=1 normals
tbl_sex <- table( bw_data$sex,bw_data$lbw2 )
tbl_sex

prop.table(tbl_sex , margin = 1)
prop.table(tbl_sex , margin = 2)

## test of proportion
prop.test(tbl_sex)
