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


#####plotting
# Change the barplot by creating a table of gender 
participants_barplot <- table(participants_data$gender)
barplot(participants_barplot)

# Create a scatterplot of days to email response (y) 
# as a function of the letters in your first name (x) 
ggplot(data = participants_data, 
       aes(x = letters_in_first_name, 
           y = days_to_email_response)) + 
  geom_point()

# Create a scatterplot of days to email response (y) 
# as a function of the letters in your first name (x) 
# with colors representing binary data 
# related to academic parents (color) 
# and working hours per day as bubble sizes (size).
ggplot(data = participants_data, 
       aes(x = letters_in_first_name, 
           y = days_to_email_response,
           color = academic_parents, 
           size = working_hours_per_day)) + 
  geom_point()

# Create a scatterplot of iris petal length (y) 
# as a function of sepal length (x) 
# with colors representing iris species (color) 
# and petal width as bubble sizes (size).
ggplot(data = iris, 
       aes(x = Sepal.Length, 
           y = Petal.Length, 
           color = Species, 
           size = Petal.Width))+ 
  geom_point()

# Create a plot with the diamonds data 
# of the carat (x) and the price (y)
plot1 <- ggplot(data = diamonds, 
                aes(x = carat, y = price, 
                    alpha = 0.1)) +
  geom_point()
plot1

# Create a plot with the diamonds data 
# of the log of carat (x) 
# and the log of price (y)
ggplot(data = diamonds,
       aes(x = log(carat),
           y = log(price),
           alpha = 0.2)) +
  geom_point()

# Create a smaller diamonds data set (top 100 rows), 
# create a scatterplot with carat on the x-axis 
# and price on the y-xis and 
# with the color of the diamond as the color of the points. 
dsmall <- top_n(diamonds, n = 100)

ggplot(data = dsmall, aes(x = carat, 
                          y = price, 
                          color = color)) + 
  geom_point()

# Create a smaller diamonds data set (top 40 rows), 
# create a scatterplot with carat on the x-axis 
# and price on the y-xis and 
# with the cut of the diamond as the shapes for the points. 
dsmall <- top_n(diamonds, 
                n = 40)

ggplot( data = dsmall, 
        aes(x = carat, 
            y = price, 
            shape = cut)) + 
  geom_point()

# Create a plot of the diamonds data 
# with carat on the x-axis, price on the y-axis. 
# Use the inhibit function to set the alpha to 0.1 
# and color to blue.
ggplot(data = diamonds, 
       aes(x = carat, 
           y = price, 
           alpha = I(0.1), 
           color = I("blue"))) + 
  geom_point()

# Create a smaller data set of diamonds with 50 rows. 
# Create a scatterplot and smoothed conditional 
# means overlay with carat on the x-axis 
# and price on the y-axis.
dsmall <- top_n(diamonds, 
                n = 50)

ggplot(data = dsmall, 
       aes(x = carat, 
           y = price))+
  geom_point()+
  geom_smooth()

# Create a smaller data set of diamonds with 50 rows. 
# Create a scatterplot and smoothed conditional 
# means overlay with carat on the x-axis 
# and price on the y-axis.
# Use 'glm' as the option for the smoothing
dsmall <- top_n(diamonds, 
                n = 50)

ggplot(data = dsmall, 
       aes(x = carat, 
           y = price))+ 
  geom_point()+ 
  geom_smooth(method = 'glm')

# Change the boxplot so that the x-axis is cut and
#  the y-axis is price divided by carat
ggplot(data = diamonds, 
       aes(x = cut, 
           y = price / carat)) + 
  geom_boxplot()

# Change the jittered boxplot so that the x-axis is cut and
#  the y-axis is price divided by carat
ggplot(data = diamonds, 
       aes(x = cut, 
           y = price / carat)) + 
  geom_boxplot()+ 
  geom_jitter()

# Change the alpha to 0.4 to make 
# the scatter less transparent
ggplot(data = diamonds, 
       aes(x = color, 
           y = price/carat, 
           alpha = I(0.4))) + 
  geom_boxplot()+ 
  geom_jitter()

# Change the density plot so that the x-axis is carat 
# and the color is the diamond color
ggplot(data = diamonds, 
       aes(x = carat, color = color)) +
  geom_density()

# Change the density plot so that the x-axis is carat 
# the color is the diamond color
# and the alpha is set to 0.3 using the inhibit function
ggplot(data = diamonds, 
       aes(x = price, 
           color = color, 
           alpha = I(0.3))) +
  geom_density()

# Create a plot of the mpg data with 
# manufacturer as the color and a linear model 'lm'
# as the smooth method
ggplot(data = mpg, 
       aes(x = displ, 
           y = hwy,  
           color = manufacturer)) + 
  geom_point() +
  geom_smooth(method = "lm")

#examples on slow ggplot (skipped)

# subset the data to numeric only with select_if
part_data <- select_if(participants_data, 
                       is.numeric)
# use 'cor' to perform pearson correlation
# use 'round' to reduce correlation 
# results to 1 decimal
cormat <- round(cor(part_data), 
                digits = 1)
# use 'as.data.frame.table' to build a table
# with correlation values
melted_cormat <- as.data.frame.table(cormat, 
                                     responseName = "value")
# plot the result with 'geom-tile'
ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile()


#####animation
library(gganimate)
library(datasauRus)

# Check the names for the `datasaurus_dozen` data
names(datasaurus_dozen)

# Change the 'ease_aes()' option from default 'linear'
# to 'cubic-in-out' for a smoother appearance
ggplot(datasaurus_dozen, 
       aes(x = x,
           y = y))+
  geom_point()+
  theme_minimal() +
  transition_states(states = dataset) + 
  ease_aes(default = 'cubic-in-out')

#doesnt work, ask cory
