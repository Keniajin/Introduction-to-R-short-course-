library(tidyverse)

## read in the babies 

bw <- read_csv("babies.csv")

## check the structure of the data 
str(bw)

## draw a scatter plot with Birth weight vs gestation weeks
ggplot(data = bw ,mapping = aes(x = gestwks,
                     y = bweight)) +
  #Add a point geom
  geom_point() +
  ##Make it straight
  geom_smooth(method = "lm") +
  ## add labels
  labs(x = "Gestation weeks", y = "Birthweight",
       title = "Lower gestation weeks leads to low birthweight",
       subtitle = "Bith weight is in grams")+
  theme_bw()



## draw a scatter plot with Birth weight and matage

ggplot(data = bw ,mapping = aes(x = matage,
                                y = bweight)) +
  #Add a point geom
  geom_point() +
  ##Make it straight
  geom_smooth(method = "lm") +
  ## add labels
  labs(x = "Maternal Age", y = "Birthweight",
       title = "No lineear association",
       subtitle = "Bith weight is in grams")+
  theme_bw()


## draw a graph of mutiple correlations
## only the numeric data
library("PerformanceAnalytics")

## select the continous variables only
bw_numeric <- bw %>% 
  select(matage,hyp,gestwks,bweight)

chart.Correlation(bw_numeric, histogram=TRUE, pch=19,
                  method = "pearson")



## plot 2
## calculate the correlation matrices
res <- cor(bw_numeric)
round(res, 2)


## plot using GGally
## # Correlation plot
library(GGally)
ggcorr(bw_numeric , palette = "RdBu", label = TRUE)

