# Exploratory Data Analysis
# The United Nations Voting Set
# rcid - Roll call ID, describing a one round of voting such as to aprove a 
#        united Nations resolution 
# session - Year 
# vote - 1:'yes' 2:'Abstain' 3:'No' 8:'Not present' 9:'Not a memeber'
# ccode - Country code for each


library(dplyr)

# votes_processed es el data set final
rcid session vote ccode year country


# Change this code to summarize by year
votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Este data set tiene 4 columnas year(1946-2014), total (cantidad de 
# votos en ese año) y percent_yes







# You have the votes summarized by country
by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Print the by_country dataset
by_country

# Sort in ascending order of percent_yes
arrange(by_country, percent_yes)

# Now sort in descending order
arrange(by_country, desc(percent_yes))






# Change to scatter plot and add smoothing curve
ggplot(by_year, aes(year, percent_yes)) +
  geom_point() + 
  geom_smooth()







# Group by year and country: by_year_country
by_year_country <- votes_processed %>%
  group_by(year, country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Este dataset contiene: year, country, total y percent_yes





# ||| Ploting multiple countries |||
# Vector of four countries to examine
countries <- c("United States", "United Kingdom",
               "France", "India")

# Filter by_year_country: filtered_4_countries
filtered_4_countries <- by_year_country %>% filter(country %in% countries)

# Line plot of % yes in four countries
ggplot(filtered_4_countries, aes(x = year, y = percent_yes, color = country)) + 
  geom_line()






# ||| Faceting countries |||
# Vector of six countries to examine
countries <- c("United States", "United Kingdom",
               "France", "Japan", "Brazil", "India")

# Filtered by_year_country: filtered_6_countries
filtered_6_countries <- by_year_country %>%
  filter(country %in% countries)

# Line plot of % yes over time faceted by country
ggplot(filtered_6_countries, aes(year, percent_yes)) +
  geom_line() +
  facet_wrap(~ country, scales = "free_y")






# ||| Linear Regression on the US |||
# Percentage of yes votes from the US by year: US_by_year
US_by_year <- by_year_country %>%
  filter(country == "United States")

# Print the US_by_year data
US_by_year 

# Perform a linear regression of percent_yes by year: US_fit
US_fit <- lm(percent_yes ~ year, data = US_by_year)

# Perform summary() on the US_fit object
summary(US_fit)





# ||| Tidying models with broom |||
# Models are difficult to combine
# The broom package
# broom turns a model into a data frame
library(broom)
tidy(model)
# This is just to make "model" a data frame 

# Tidy models can be combined, for example:
model1 <- lm(percent_yes ~ year, data = afghanistan)
model2 <- lm(percent_yes ~ year, data = united_states)

bind_rows(tidy(model1), tidy(model2))



# ||| One model for each country |||
# This will allow us to find the countries whos level of agreement with the rest of
# united nations is increasing or decreasing most dramatically 

# Start with one row per country but we are going to split it into many small datasets
# one for each country 
# To do this we are going to use:
library(tidyr)
nest()

# Example:
nest(-country) # means "nest all but country"
# Así que las otras columnas(year, total, percent_yes) quedan en un data frame llamado "data"
#                 country      data<list>
# 1               Mexico     <tibble [34 X 3]>
                  

                  
                  
                  
# ||| Nesting a data frame |||
# Load the tidyr package
library(tidyr)

# Nest all columns besides country
by_year_country %>%
  nest(-country)





# All countries are nested besides country
nested <- by_year_country %>%
  nest(-country)

# Print the nested data for Brazil
nested$data[[7]]





# ||| Fiting multiple models |||
# purrr package offers functions to work with lists
# map() applies an operation to each item in a list 
# Ejemplin 
library(purrr)
v <- c(1,2,3)
map(v, ~ . * 10)


library(purrr)
by_year_country %>%
  nest(-country) %>%
  mutate(models = map(data, ~ lm(percent_yes ~ year, .)))

# El data frame sería así:
#       Country           data                models
#        <chr>           <list>               <list>
# 1     Mexico     <tibble [ 34 X 3]>        <S3: lm>



# tidy turns each model into a data frame:
by_year_country %>%
  nest(-country) %>%
  mutate(models = map(data, ~ lm(percent_yes ~ year, .))) %>%
  mutate(tidied = map(models, tidy))

# El data frame sería así:
#       Country           data                models            tidied
#        <chr>           <list>               <list>            <list>
# 1     Mexico     <tibble [ 34 X 3]>        <S3: lm>       <data.frame[2 X 5]>



# unnest() combines the tidied models
country_coefficients <- by_year_country %>%
  nest(-country) %>%
  mutate(models = map(data, ~ lm(percent_yes ~ year, .))) %>%
  mutate(tidied = map(models, tidy)) %>%
  unnest(tidied)
# Now we have a table of coefficients:
#        Country        term       estimate     std.error    statistic     p.value
# 1      Mexico     (Intercept)   -11.0056  
# 2      Mexico         year      0.00673

# El termino year y su estimado es el término que nos interesa (slope)

# Now filter for the year term(slope)
country_coefficients %>%
  filter(term == "year")
# We want to get the models that were statistical significant
# Here is a familiar issue, when we run many statistical tests and evaluate the p-values
# we need to do a multiple hypothesis correction, the basic problem is that if you try many 
# tests some p-vaues will be less than 0.05 by chance meaning that we need to be more
# restrivtive, here comes the p.adjust.
# By filtering for cases with the adjusted p-value is less than 0.05 we can feel more safe
# in our assumptions and get a set of country trends that we believe are real
country_coefficients %>%
  filter(term == "year") %>%
  filter(p.adjust(p.value) < 0.05)





# ||| Sorting by slope |||
#Now that you've filtered for countries where the trend is probably not due to chance, you 
# may be interested in countries whose percentage of "yes" votes is changing most quickly 
# over time. Thus, you want to find the countries with the highest and lowest slopes; 
# that is, the estimate column
# Filter by adjusted p-values
filtered_countries <- country_coefficients %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value)) %>%
  filter(p.adjusted < .05)

# Sort for the countries increasing most quickly
arrange(filtered_countries, desc(estimate))

# Sort for the countries decreasing most quickly
arrange(filtered_countries, estimate)







# ||| Joining datasets with inner_join |||
library(dplyr)
# Join them together based on the "rcid" and "session" columns
votes_joined <- inner_join(votes_processed, descriptions,
                           by = c ("rcid", "session"))






# ||| Visualizing for colonialism |||
# Load the ggplot2 package
library(ggplot2)

# Filter, then summarize by year: US_co_by_year
US_co_by_year <- votes_joined %>%
  filter(country == "United States", co == 1) %>%
  group_by(year) %>%
  summarize(percent_yes = mean(vote == 1))

# Graph the % of "yes" votes over time
ggplot(US_co_by_year, aes(year, percent_yes)) +
  geom_line()




# ||| Using gather to tidy a dataset
# Load the tidyr package
library(tidyr)

# Gather the six mu/nu/di/hr/co/ec columns
votes_joined %>%
  gather(topic, has_topic, me:ec)

# Perform gather again, then filter
votes_gathered <- votes_joined %>%
  gather(topic, has_topic, me:ec) %>%
  filter(has_topic == 1)






# ||| Recoding the topics ||| 
# Replace the two-letter codes in topic: votes_tidied
votes_tidied <- votes_gathered %>%
  mutate(topic = recode(topic,
                        me = "Palestinian conflict",
                        nu = "Nuclear weapons and nuclear material",
                        di = "Arms control and disarmament",
                        hr = "Human rights",
                        co = "Colonialism",
                        ec = "Economic development"))






# ||| Summarize by country, year and topic |||
# Summarize the percentage "yes" per country-year-topic
by_country_year_topic <- votes_tidied %>%
  group_by(country, year, topic) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1)) %>%
  ungroup(votes_tidied)

# names(votes_tidied)
# "rcid"      "session"   "vote"      "ccode"     "year"      "country"  
# [7] "date"      "unres"     "topic"     "has_topic"

# names(by_country_year_topic)
# [1] "country"     "year"        "topic"       "total"       "percent_yes"








# ||| Visualizing trends in topics for one country |||
# Load the ggplot2 package
library(ggplot2)

# Filter by_country_year_topic for just the US
US_by_country_year_topic <- 
  by_country_year_topic %>%
  filter(country == "United States")

# Plot % yes over time for the US, faceting by topic
ggplot(US_by_country_year_topic, aes(x = year, y = percent_yes)) +
  geom_line() + 
  facet_wrap( ~ topic)





# ||| Nesting by topic and country |||
# names(by_country_year_topic):
# [1] "country"     "year"        "topic"(defined-names)       "total"       "percent_yes"
# Load purrr, tidyr, and broom
library(purrr)
library(tidyr)
library(broom)

# Print by_country_year_topic
by_country_year_topic

# Fit model on the by_country_year_topic dataset
country_topic_coefficients <- by_country_year_topic %>%
  nest(-country, -topic) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
         tidied = map(model, tidy)) %>%
  unnest(tidied)

# Print country_topic_coefficients
country_topic_coefficients

# names(country_topic_coefficients)
# [1] "country"   "topic"     "term"      "estimate"  "std.error" "statistic"
# [7] "p.value"








# ||| Interpreting tidy models |||
# You'll also have to extract only cases that are statistically significant, which means 
# adjusting the p-value for the number of models, and then filtering to include only 
# significant changes.

# Create country_topic_filtered
country_topic_filtered <- country_topic_coefficients %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value)) %>%
  filter(p.adjusted < 0.05)








# ||| Checking models visually |||
# Create vanuatu_by_country_year_topic
vanuatu_by_country_year_topic <- by_country_year_topic %>%
  filter(country == "Mexico")

# Plot of percentage "yes" over time, faceted by topic
ggplot(vanuatu_by_country_year_topic, aes(x = year, y = percent_yes)) + 
  geom_line() + 
  facet_wrap(~ topic)










