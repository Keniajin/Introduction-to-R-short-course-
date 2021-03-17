
# set working directory
setwd("~/Dropbox/MYFILES/3. Consultancies/PwaniUni/2021_MPH_R/Introduction-to-R-short-course-/day3")


# Load required packages
library(dplyr)
library(freqtables)


# load dataset
bw <- read.csv("birthweight2.csv", header=T)

# use dplyr to create new categorical variable classifying children as low birth weight (<2900g) and normal birth weight (>=2900g)
bw <- bw %>%
  mutate(new_lbw= ifelse(bweight>2900, 2,1))


# test whether there is a significant difference between proportion of low birth-weight in males versus females

tbl_sex <- table(bw$sex,bw$new_lbw )
tbl_sex

prop.table(tbl_sex , margin = 1)
prop.table(tbl_sex , margin = 2)

prop.test(tbl_sex)


# INTERPRET THE RESULTS


## SEE HOW THE CONFIDENCE INTERVALS OVERLAP 
bw %>% 
  ## create the frequency table
  freq_table(sex, new_lbw) %>%
  ## filter only low birth-weights
  filter(col_cat==1) %>%
  ## select only variables containing row
  select(contains('row'))


