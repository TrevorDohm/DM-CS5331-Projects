library("tidyverse")
library("DT")
library(caret)


# Read Data
cases <- read_csv("COVID-19_cases_plus_census.csv")
cases

# Make character factor for analysis
cases <- cases %>% mutate_if(is.character, factor)
dim(cases)

# Calculate rates (per 1000 people)
# And select important variables
# (We'll need more variables)
cases <- cases %>% filter(confirmed_cases > 0) 

cases <- cases %>% 
  arrange(desc(confirmed_cases)) #%>%    
#select(county_name, state, confirmed_cases, deaths, total_pop, median_income, median_age)
cases <- cases %>% mutate(
  cases_per_10000 = confirmed_cases/total_pop*10000, 
  deaths_per_10000 = deaths/total_pop*10000, 
  death_per_case = deaths/confirmed_cases)

cases

# dput(colnames(cases))

cases_sel <- cases %>% select(county_name, state, total_pop,
                              nonfamily_households, median_year_structure_built,        
                              female_pop, median_age, white_pop, 
                              black_pop, asian_pop, hispanic_pop, amerindian_pop,
                              commuters_by_public_transportation, 
                              households, median_income, housing_units, 
                              vacant_housing_units, 
                              percent_income_spent_on_rent,
                              employed_pop, unemployed_pop, 
                              in_school, in_undergrad_college,
                              cases_per_10000, deaths_per_10000, death_per_case)

# normalize by population 
cases_sel <- cases_sel %>% mutate(
  nonfamily_households = nonfamily_households / total_pop, 
  female_pop = female_pop / total_pop,
  white_pop = white_pop / total_pop, 
  black_pop = black_pop / total_pop, 
  asian_pop = asian_pop / total_pop, 
  hispanic_pop = hispanic_pop / total_pop, 
  amerindian_pop = amerindian_pop / total_pop,
  commuters_by_public_transportation = commuters_by_public_transportation/ total_pop, 
  households = households / total_pop, 
  housing_units = housing_units / total_pop, 
  vacant_housing_units = vacant_housing_units / total_pop, 
  employed_pop = employed_pop / total_pop, 
  unemployed_pop = unemployed_pop / total_pop, 
  in_school = in_school / total_pop, 
  in_undergrad_college = in_undergrad_college / total_pop 
)

cases_sel

# Summary
summary(cases_sel)


# Check for missing values
table(complete.cases(cases_sel))

# Check that class variable is a factor
# Otherwise many models will perform regression
str(cases_sel)

# Check correlation for numeric variables
library(seriation)
cm <- cor(cases_sel %>% select_if(is.numeric) %>% na.omit)
hmap(cm, margins = c(14,14))

# This focuses on states with covid 19 outbreaks
# Create class variable
# Bad means high fatality rate
cases_sel <- cases_sel %>% mutate(bad = as.factor(deaths_per_10000 > 10))

# check if class variable is very imbalanced
cases_sel %>% pull(bad) %>% table()

cases_sel %>% group_by(state) %>% 
  summarize(bad_pct = sum(bad == TRUE)/n()) %>%
  arrange(desc(bad_pct))


# SPLIT INTO TRAINING AND TEST DATA
# using TX, CA, FL, NY to train
cases_train <- cases_sel %>% filter(state %in% c("TX", "CA", "FL", "NY"))
cases_train %>% pull(bad) %>% table()

cases_test <-  cases_sel %>% filter(!(state %in% c("TX", "CA", "FL", "NY")))
cases_test %>% pull(bad) %>% table()

# Select Features
# Plot a map for test data
counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties 

counties_all <- counties %>% left_join(cases_train %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))

ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

# check variable importance
library(FSelector)
cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head()

# we need to remove the variable that was used to create the class variable
cases_train <- cases_train %>% select(-c(deaths_per_10000))
cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head()

# remove more covid 19 related variables
cases_train <- cases_train %>% select(-death_per_case, -cases_per_10000)

cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head(n = 10)

# BUILD A MODEL
# Don’t use county or state name. 
# The variables are not useful to compare between states 
# and variables with many levels will make tree-based algorithms very slow.
fit <- cases_train %>%
  train(bad ~ . - county_name - state,
        data = . ,
        #method = "rpart",
        method = "rf",
        #method = "nb",
        trControl = trainControl(method = "cv", number = 10)
  )
fit

#library(rpart.plot)
#rpart.plot(fit$finalModel, extra = 2)

varImp(fit)

# Note: You should probably take cases per day data instead of the total cases.

# USE MODEL FOR THE REST OF THE US
# caret does not make prediction with missing data
cases_test <- cases_test %>% na.omit
cases_test$bad_predicted <- predict(fit, cases_test)

# visualize the results
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
# ground truth
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

# predictions
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

# confusion matrix
confusionMatrix(data = cases_test$bad_predicted, ref = cases_test$bad)

