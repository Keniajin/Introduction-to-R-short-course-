
library(tidyverse)
library(gapminder)




## adding the data sets to the environment
canada  <- filter(gapminder, country == "Canada")

denmark_2000 <- filter(gapminder, country == "Denmark" & year > 2000)


##filter 2007 data for lifeexp>50 for Europe and Asia
europe_asia_2007 <- filter(gapminder, 
                           year == 2007& lifeExp > 50 &
                             continent %in% c('Europe','Asia'))


## solution 2

df_sol1 <- filter(gapminder,
                  year==2007,
                  lifeExp>50,
                  (continent=='Europe' | continent=='Asia'))


df_1960 <- mutate(gapminder,
       after_1960=ifelse(year>1960,"After 1960",
                         "Before 1960"))
