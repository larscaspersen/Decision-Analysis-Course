#this follows the tasks given by seminar 2

#check version of rstudio
version

#use tidyverse library
library(tidyverse)

#check examples how to use tidyverse
browseVignettes(package = "tidyverse")

library(readr)
#link to data set on github
urlfile = "https://raw.githubusercontent.com/CWWhitney/teaching_R/master/participants_data.csv"
participants_data <- read_csv(url(urlfile)) #download data

#view data in console
participants_data

# head shows you by default first 6 rows, can be changed to e.g. 7
head(participants_data, 
     n = 7)

#check names of variables
names(participants_data)

#check structure of data frame
str(participants_data)

# access rows with $
participants_data$gender


##### data wrangling with dplyr
library(dplyr)
library(magrittr)

# select columns by name
select(participants_data, 
       batch,
       age)

#exclude columns with -
select(participants_data,
       -batch,
       -age)

#filter table by conditions
filter(participants_data, 
       working_hours_per_day >5)

#subsetting can be done with multiple criteria
filter(participants_data, 
       working_hours_per_day >5 & 
         letters_in_first_name >3)

#change varible name
# Rename the variable km_home_to_office as commute
rename(participants_data, 
       commute = km_home_to_office)

#add columns with mutate
# Mutate a new column named age_mean that is a function of the age multiplied by the mean of all ages in the group
mutate(participants_data, 
       age_mean = age *
         mean(age))

# Mutate new column named response_speed 
# populated by 'slow' if it took you 
# more than a day to answer my email and 
# 'fast' for others
mutate(participants_data, 
       response_speed = 
         ifelse(days_to_email_response > 3, 
                "slow", "fast"))

#get summary of df
# Create a summary of the participants_mutate data 
# with the mean number of siblings 
# and median years of study
summarize(participants_data,
          mean(number_of_siblings),
          median(years_of_study))

#use magrittr pipelines
# Use the magrittr pipe to summarize 
# the mean days to email response, 
# median letters in first name, 
# and maximum years of study by gender
participants_data %>% 
  group_by(gender) %>% 
  summarize(mean(days_to_email_response), 
            median(letters_in_first_name), 
            max(years_of_study))

#combine with mutate
# Use the magrittr pipe to create a new column 
# called commute, where those who travel 
# more than 10km to get to the office 
# are called "commuter" and others are "local". 
# Summarize the mean days to email response, 
# median letters in first name, 
# and maximum years of study. 
participants_data %>% 
  mutate(commute = ifelse(
    km_home_to_office > 10, 
    "commuter", "local")) %>% 
  group_by(academic_parents) %>% 
  summarize(mean(days_to_email_response), 
            median(letters_in_first_name), 
            max(years_of_study))

#####purr
library(purrr) #for regresssions

# Split the data frame by batch, 
# fit a linear model formula 
# (days to email response as dependent 
# and working hours as independent) 
# to each batch, compute the summary, 
# then extract the R^2.
participants_data %>%
  split(.$batch) %>% 
  map(~ 
        lm(days_to_email_response ~ 
             working_hours_per_day, 
           data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

#####own project on diamonds data
#should be done once in long and in short format

library(ggplot2)#to get diamonds data
#tasks:
#select: carat and price
#filter: only where carat is > 0.5
#rename: rename price as cost
#mutate: create a variable with 'expensive' if greater than mean of cost and 'cheap' otherwise
#group_by: split into cheap and expensive
#summarize: give some summary statistics of your choice

select(diamonds, carat, price)
filter(diamonds, carat > 0.5)
rename(diamonds, cost = price)
diamonds_mutate <- mutate(diamonds,price_tag = ifelse(price > mean(price),
       'expensive','cheap'))
group_by(diamonds_mutate,price_tag)
summarise(diamonds_mutate,mean(price))

diamonds %>%
  select(carat, price)%>%
  filter(carat > 0.5) %>%
  rename(cost = price) %>%
  mutate(price = ifelse(cost > mean(cost),'expensive','cheap'))%>%
  group_by(price) %>%
  summarise(mean(carat))