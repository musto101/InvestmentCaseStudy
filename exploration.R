library(tidyverse)
library(readxl)
library(lubridate)
library(ggthemes)
library(scales)

retail_transactions <- read_excel('data/Retail_Data.xlsx', sheet = 1)

table(retail_transactions$merchant_name)
table(retail_transactions$tag_user)

glimpse(retail_transactions)
sum(is.na(retail_transactions))

#### megaco analysis ####

## revenue by date ##

mega_dat <- retail_transactions %>% 
  filter(merchant_name == 'MegaCo', tag_user %in% c('Fuel', 'Supermarket'))

mega_dat_rs <- mega_dat %>% group_by(transaction_date) %>% 
  summarise(revenue = sum(amount), transactions = n())

mega_dat_rs %>% ggplot(aes(transaction_date, revenue)) + geom_line() +
  geom_smooth(method = lm)  

## revenue by year and quarter ##

mega_dat_qtr <- mega_dat_rs %>% 
  group_by(transaction_date = quarter(as.POSIXlt(transaction_date,
                                                 format="%Y-%m-%d"),
                                      with_year = T)) %>% 
  summarise(revenue = sum(revenue), transactions = sum(transactions))

mega_qtr_yr_plot <- mega_dat_qtr %>% ggplot(aes(as.factor(transaction_date), revenue)) +
  geom_line(group = 1) + theme_economist() + labs(x = 'year_quarter', 'revenue') 

## difference from last year ##

actual_difference <- (1 - 842013.6/889745.6) * 100 # down 5.36% from last year

#### market expectations ####

market_expectations <- read_excel('data/Retail_Data.xlsx', sheet = 3)


## as a whole ##

summary(market_expectations$Estimate) * 100
round(sd(market_expectations$Estimate) * 100,2)
boxplot(market_expectations$Estimate)


ggplot(market_expectations, aes(Estimate * 100)) + geom_boxplot() +
  coord_flip() + theme_economist() + labs(x = 'Estimate (%)') +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

n <- 16
xbar <- -2.169 
s <- 3.27

#calculate margin of error
margin <- qt(0.99,df=n-1)*s/sqrt(n)

#calculate lower and upper bounds of confidence interval
low <- xbar - margin
low # -4.296528

high <- xbar + margin
high # -0.04147236

## Old vs new predictions ##

table(market_expectations$`Date issued`)

old_me <- market_expectations %>% 
  filter(month(as.POSIXlt(`Date issued`, format="%Y-%m-%d"))== 09)

new_me <- market_expectations %>% 
  filter(month(as.POSIXlt(`Date issued`, format="%Y-%m-%d"))== 12)

summary(old_me$Estimate) * 100
round(sd(old_me$Estimate)* 100,2)


ggplot(old_me, aes(Estimate * 100)) + geom_boxplot() +
  coord_flip() + theme_economist() + labs(x = 'Estimate (%)') +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

n <- 4
xbar <- -7.30 
s <- 0.8

#calculate margin of error
margin <- qt(0.99,df=n-1)*s/sqrt(n)

#calculate lower and upper bounds of confidence interval
low <- xbar - margin
low # -4.296528

high <- xbar + margin
high # -0.04147236

summary(new_me$Estimate) * 100
round(sd(new_me$Estimate)* 100,2)


ggplot(new_me, aes(Estimate * 100)) + geom_boxplot() +
  coord_flip() + theme_economist() + labs(x = 'Estimate (%)') +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

n <- 12
xbar <- -0.4583 
s <- 1.27

#calculate margin of error
margin <- qt(0.99,df=n-1)*s/sqrt(n)

#calculate lower and upper bounds of confidence interval
low <- xbar - margin
low # -4.296528

high <- xbar + margin
high # -0.04147236
