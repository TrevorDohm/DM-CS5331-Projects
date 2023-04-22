# IMPORTS

# library()
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggcorrplot)
library(dplyr)
library(readr)
library(rpart)
library(caret)
library(seriation)
library(FSelector)
library(mlbench)
library(vtable)
library(DataExplorer)
library(DT)

# Make Results Reproducible
set.seed(1000)





# DATA CLEANING

# Read Data (All Read, Most Likely Will Only Use Census Data)
casesCensus <- read_csv("Datasets/COVID-19_cases_plus_census_recent.csv")
vaccineInfo <- read_csv("Datasets/County_Vaccine_Information.csv")
casesDeaths <- read_csv("Datasets/COVID-19_cases_TX_updated.csv")
counties <- as_tibble(map_data("county"))

# Summary Before Manipulation
casesCensus
vaccineInfo
casesDeaths
counties
summary(casesCensus)
summary(vaccineInfo)
summary(casesDeaths)
summary(counties)
sumtable(casesCensus, out = 'htmlreturn')
sumtable(vaccineInfo, out = 'htmlreturn')
sumtable(casesDeaths, out = 'htmlreturn')
sumtable(counties, out = 'htmlreturn')

# Data Explorer Code
plot_intro(casesCensus, title = "Intro Plot for U.S. Covid-19 Cases and Census Dataset")
plot_intro(vaccineInfo, title = "Intro Plot for Texas County Vaccine Sites Dataset")
plot_intro(casesDeaths, title = "Intro Plot for Texas Cases And Deaths")
plot_intro(counties, title = "Intro Plot for United States County Positions")

# Clean Vaccine Data (Scale, Remove Columns)
vaccineInfo <- vaccineInfo %>% mutate_if(is.character, factor)
vaccineNonNumeric <- vaccineInfo %>% select_if(~!is.numeric(.))
vaccineNumeric <- vaccineInfo %>% select_if(is.numeric) %>% scale() %>% as_tibble()
vaccineInfo <- vaccineNumeric %>% add_column(vaccineNonNumeric$us_county) %>% 
  rename("us_county" = "vaccineNonNumeric$us_county")

# Clean Census Data (Factor, Remove Obsolete Column, Add New Columns)
# Note: We Want To Predict Death_Per_Case, So We Only Add This Column For Now
casesCensus <- casesCensus %>% mutate_if(is.character, factor) %>% select(-last_col())%>% mutate(
  death_per_case = deaths / confirmed_cases)

# Take Numeric Data, Remove NA To Perform Dimensionality Reduction
casesCensusUpdated <- casesCensus %>% select_if(is.numeric) %>% 
  add_column(casesCensus$county_name) %>% 
  rename("county_name" = "casesCensus$county_name") %>% na.omit()

# Take Numeric Data, Scale
casesCensusUpdatedNumeric <- casesCensusUpdated %>% 
  select_if(is.numeric) %>% scale() %>% as_tibble()

# Initial Correlation Matrix - Let's Reduce Dimension
corrMatrix <- cor(casesCensusUpdatedNumeric)

# Remove Highly Correlated Variables, Show New Matrix
corrMatrixRemove <- corrMatrix
corrMatrixRemove[upper.tri(corrMatrixRemove)] <- 0
diag(corrMatrixRemove) <- 0
casesCensusUpdatedNumeric <- casesCensusUpdatedNumeric[, !apply(corrMatrixRemove, 2, function(x) any(x > 0.95))]
corrMatrix <- cor(casesCensusUpdatedNumeric)
ggcorrplot(corrMatrix, insig = "blank", hc.order = TRUE) + ggtitle("Correlation Matrix After Removing Highly Correlated Variables")
hmap(corrMatrix, margins = c(10, 10))

# Good Example Of Feature Extraction (Predict Death_Per_Case)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model <- train(death_per_case~., data = casesCensusUpdatedNumeric, method = "lm", preProcess = "scale", trControl = control)
importance <- varImp(model, scale = FALSE)
plot(importance, top = 30)

# Subset Upon Important Features
ImpMeasure <- data.frame(importance$importance)
ImpMeasure$Vars <- row.names(ImpMeasure)
casesCensusUpdatedNumeric <- casesCensusUpdatedNumeric %>% 
  select(ImpMeasure[order(-ImpMeasure$Overall),][1:30,]$Vars)
corrMatrix <- cor(casesCensusUpdatedNumeric)
ggcorrplot(corrMatrix, insig = "blank", hc.order = TRUE) + ggtitle("Correlation Matrix With Top Important Variables (30)")
hmap(corrMatrix, margins = c(10, 10))

# PCA: SAMPLE CODE - DO NOT USE
# casesCensusUpdatedNumericPCA <- princomp(corrMatrix)
# summary(casesCensusUpdatedNumericPCA)
# casesCensusUpdatedNumericPCA$loadings[, 1:2]
# fviz_eig(casesCensusUpdatedNumericPCA, addlabels = TRUE) + ggtitle("Scree Plot - PCA")
# fviz_pca_var(casesCensusUpdatedNumericPCA, col.var = "black")
# fviz_cos2(casesCensusUpdatedNumericPCA, choice = "var", axes = 1:2) + ggtitle("Cos2 of Variables to Dim-1-2 - PCA")
# fviz_pca_var(casesCensusUpdatedNumericPCA, col.var = "cos2",
#              gradient.cols = c("black", "orange", "green"), repel = TRUE)

# Box Plot (See Outliers)
summary(casesCensusUpdatedNumeric)
boxplot(casesCensusUpdatedNumeric)$out
title("Box Plot of Normalized Census Data Showing Outliers")

# Introduce Country Name Into Dataset
dataFinal <- casesCensusUpdatedNumeric %>% 
  add_column(casesCensusUpdated$county_name) %>% 
  rename("county_name" = "casesCensusUpdated$county_name") %>% 
  select(county_name, everything())

# Outlier Removal
zScores <- as.data.frame(sapply(casesCensusUpdatedNumeric, function(data) (abs(data - mean(data)) / sd(data))))
dataFinal <- dataFinal[!rowSums(zScores > 3), ]
boxplot(dataFinal %>% select(-1))$out
title("Box Plot of Normalized Census Data After Removing Outliers")

# Subset Original Data With Found Features (+ Some Extra)
# Add More Columns - Classes For Classification
# Note: Omit Drops 5 Counties, Filter Drops 1 County
casesCensusFinal <- casesCensus %>% select(colnames(dataFinal), "employed_pop", "unemployed_pop", "in_school", "in_undergrad_college")
casesCensusFinal <- casesCensusFinal %>% select_if(is.numeric) %>% as_tibble() %>% 
  add_column(casesCensus$county_name) %>% rename("county" = "casesCensus$county_name") %>% 
  add_column(casesCensus$state) %>% rename("state" = "casesCensus$state") %>%
  add_column(casesCensus$confirmed_cases) %>%
  add_column(casesCensus$total_pop) %>%
  rename("confirmed_cases" = "casesCensus$confirmed_cases") %>% 
  rename("total_pop" = "casesCensus$total_pop") %>% mutate(
    cases_per_10000 = confirmed_cases / total_pop * 10000, 
    deaths_per_10000 = deaths / total_pop * 10000, 
    death_per_case = deaths / confirmed_cases) %>% na.omit() %>% 
  filter(confirmed_cases > 0) %>% select(state, county, total_pop, confirmed_cases, deaths, 
                                         cases_per_10000, deaths_per_10000, death_per_case, everything())

# Add County Name To Final Data For All Future Map Plots, Format
casesCensusFinal <- casesCensusFinal %>% 
  mutate(county = county %>% str_to_lower() %>% str_replace('\\s+county\\s*$', '')) %>%
  arrange(desc(confirmed_cases)) %>% mutate_if(is.character, factor)

# Data Explorer Code (Finalization)
plot_intro(casesCensusFinal, title = "Intro Plot for Finalized Census Dataset")
datatable(casesCensusFinal) %>% formatRound(c(5, 9, 10), 2) %>% formatPercentage(11, 2)
summary(casesCensusFinal)
table(complete.cases(casesCensusFinal))

# Check Correlation For Numeric Variables
corrMatrixFinal <- cor(casesCensusFinal %>% select_if(is.numeric))
hmap(corrMatrixFinal, margins = c(10, 10))

# NOTE: MAY NEED TO SCALE ADDED COLUMNS - IF SKEWED RESULTS, COME BACK TO THIS STEP





# Idea: Focus on states with Covid-19 outbreaks
# Use a few states with many cases (training data) to learn a model of how 
# demographics and socioeconomic factors affect fatalities and then apply the 
# model to the other states (test data). I define the class variable by discretizing 
# deaths per a population of 10,000 into below and above 10 deaths.

# CLASS CREATION

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












inTrain <- createDataPartition(y = Zoo$type, p = .8)[[1]]
Zoo_train <- Zoo %>% slice(inTrain)
Zoo_test <- Zoo %>% slice(-inTrain)


weights <- Zoo_train %>% chi.squared(type ~ ., data = .) %>%
  as_tibble(rownames = "feature") %>%
  arrange(desc(attr_importance))








# DATA

cases <- read_csv("COVID-19_cases_plus_census.csv")

# Make character factors for analysis

cases <- cases %>% mutate_if(is.character, factor)
dim(cases)

# Calculate rates (per 1000 people) and select important variables. You need more variables.

cases <- cases %>% filter(confirmed_cases > 0) 

cases <- cases %>% 
  arrange(desc(confirmed_cases)) #%>%    
  #select(county_name, state, confirmed_cases, deaths, total_pop, median_income, median_age)
cases <- cases %>% mutate(
  cases_per_10000 = confirmed_cases/total_pop*10000, 
  deaths_per_10000 = deaths/total_pop*10000, 
  death_per_case = deaths/confirmed_cases)

# CHOICE NOT INCLUDED (SEE NOTES)

summary(cases_sel)

# Check for missing values and if the data looks fine.
table(complete.cases(cases_sel))

# Check that class variable is a factor
# Otherwise many models will perform regression
str(cases_sel)

# Check correlation for numeric variables
cm <- cor(cases_sel %>% select_if(is.numeric) %>% na.omit)
hmap(cm, margins = c(10,10)) # it said anything above 10 was figure margins too large (error)

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

