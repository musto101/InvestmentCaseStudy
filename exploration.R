library(tidyverse)
library(readxl)
library(lubridate)
library(ggthemes)
library(scales)
library(RColorBrewer)

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

mega_qtr_yr_plot <- mega_dat_qtr %>% 
  ggplot(aes(as.factor(transaction_date), revenue)) +
  geom_line(group = 1) + theme_economist() + labs(x = 'year_quarter', 'revenue')

## difference from last year ##

actual_difference <- (1 - 842013.6/889745.6) * 100 # down 5.36% from last year

## past performance vs future performance in terms of transactions ##

mega_qtr_yr_plot <- mega_dat_qtr %>% 
  ggplot(aes(as.factor(transaction_date), transactions)) +
  geom_line(group = 1) + theme_economist() +
  labs(x = 'year_quarter', 'transactions')

## transaction difference ##

transact_diff <- (1- 36368/37074) * 100 # 1.9043

## customer value ##

customers <- read_excel('data/Retail_Data.xlsx', sheet = 2)

mega_customer_dat <- retail_transactions %>% 
  left_join(customers) %>% 
  filter(merchant_name == 'MegaCo', tag_user %in% c('Fuel', 'Supermarket')) %>% 
  

sum(is.na(mega_customer_dat))/nrow(mega_customer_dat)

mega_customer_dat <- na.omit(mega_customer_dat)

mega_customer_rs <- mega_customer_dat %>% 
  group_by(transaction_date = quarter(as.POSIXlt(transaction_date,
                                                 format="%Y-%m-%d"),
                                      with_year = T), salary_range) %>% 
  summarise(revenue = sum(amount), transactions = n(),
            num_customers = length(unique(user_ref)))

old_customer <- mega_customer_rs %>% filter(transaction_date == 2013.4)

options(scipen = 10000)
old_customer %>% ggplot(aes(as.factor(salary_range), revenue)) +
  geom_bar(stat = 'identity') + theme_economist() +
  labs(x = 'Salary Range', 'Revenue')

new_customer <- mega_customer_rs %>% filter(transaction_date == 2014.4)

new_customer %>% ggplot(aes(as.factor(salary_range), revenue)) +
  geom_bar(stat = 'identity') + theme_economist() +
  labs(x = 'Salary Range', 'Revenue')

## transactions ##

old_customer %>% ggplot(aes(as.factor(salary_range), transactions)) +
  geom_bar(stat = 'identity') + theme_economist() +
  labs(x = 'Salary Range', 'Revenue')

new_customer %>% ggplot(aes(as.factor(salary_range), transactions)) +
  geom_bar(stat = 'identity') + theme_economist() +
  labs(x = 'Salary Range', 'Revenue')

## number customers ##

old_customer %>% ggplot(aes(as.factor(salary_range), num_customers)) +
  geom_bar(stat = 'identity') + theme_economist() +
  labs(x = 'Salary Range', 'Revenue')

new_customer %>% ggplot(aes(as.factor(salary_range), num_customers)) +
  geom_bar(stat = 'identity') + theme_economist() +
  labs(x = 'Salary Range', 'Revenue')

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

### retail in general ##

table(retail_transactions$merchant_name)


retail_all <- retail_transactions %>% 
  group_by(year_quarter = quarter(as.POSIXlt(transaction_date,
                                                 format="%Y-%m-%d"),
                                      with_year = T), merchant_name) %>% 
  summarise(revenue = sum(amount), transactions = n(),
            num_customers = length(unique(user_ref)))

retail_all %>% ggplot(aes(as.factor(year_quarter), 
                          revenue, color = merchant_name, group = merchant_name)) +
  geom_line() + theme_economist() + scale_color_brewer(palette = "Dark2") +
  labs(x = 'Year Quarter', y = 'Revenue')

retail_all %>% ggplot(aes(as.factor(year_quarter), transactions,
                          color = merchant_name,  group = merchant_name)) +
  geom_line() + theme_economist() + scale_color_brewer(palette = "Dark2") +
  labs(x = 'Year Quarter', y = 'Transactions')

retail_all %>% ggplot(aes(as.factor(year_quarter), num_customers,
                          color = merchant_name,  group = merchant_name)) +
  geom_line() + theme_economist() + scale_color_brewer(palette = "Dark2") +
  labs(x = 'Year Quarter', y = 'Number of Customers')

retail_tog <- retail_all %>% group_by(year_quarter) %>% 
  summarise(revenue = sum(revenue), transactions = sum(transactions),
            num_customers = sum(num_customers))

retail_tog %>% ggplot(aes(as.factor(year_quarter), 
                          revenue, group = 1)) +
  geom_line() + theme_economist() + scale_color_brewer(palette = "Dark2") +
  labs(x = 'Year Quarter', y = 'Revenue')

retail_tog %>% ggplot(aes(as.factor(year_quarter), transactions,  group = 1)) +
  geom_line() + theme_economist() + scale_color_brewer(palette = "Dark2") +
  labs(x = 'Year Quarter', y = 'Transactions')

retail_tog %>% ggplot(aes(as.factor(year_quarter), num_customers,  group = 1)) +
  geom_line() + theme_economist() + scale_color_brewer(palette = "Dark2") +
  labs(x = 'Year Quarter', y = 'Number of Customers')

