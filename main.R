# Imports
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggcorrplot)
library(DT)
library(dplyr)
library(readr)
library(caret)
library(mlbench)
library(maps)

# Read Data
COVID_19_cases_plus_census <- read_csv("Datasets/COVID-19_cases_plus_census.csv")
COVID_19_cases_TX <- read_csv("Datasets/COVID-19_cases_TX.csv")
Global_Mobility_Report <- read_csv("Datasets/Global_Mobility_Report.csv", col_types =  cols(sub_region_2 = col_character()))

# View Data
# View(COVID_19_cases_plus_census)
# View(COVID_19_cases_TX)
# View(Global_Mobility_Report)

# Make Character Factors, Filter TX
cases <- COVID_19_cases_plus_census %>% mutate_if(is.character, factor)
dim(cases)
cases_TX <- COVID_19_cases_plus_census %>% filter(state == "TX")
dim(cases_TX)
summary(cases_TX[,1:10])

# Feature Ranking (After Factorizing) ?
transform_census <- as.data.frame(sapply(COVID_19_cases_plus_census, as.numeric))
transform_census <- transform_census %>% select_if(~ !any(is.na(.))) %>% select(-c(date, do_date))
cor_census <- cor(transform_census[,-1])
# ggcorrplot(cor_census, p.mat = cor_pmat(transform_census[,-1]), insig = "blank", hc.order = TRUE)
high_cor <- findCorrelation(cor_census, cutoff = 0.99995)
colnames(transform_census)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(confirmed_cases~., data=transform_census, method="lm", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
print(importance)
# plot(importance)

# Are there many counties with many cases?
ggplot(cases_TX, mapping = aes(confirmed_cases)) + geom_histogram(bins = 20)

# Relationship between cases and deaths
ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, size = total_pop)) + geom_point()
ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX, deaths >= 1000)) 

# Calculate rates (per 1000 people)
cases_TX_select <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income)

cases_TX_select <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)

head(cases_TX_select)
datatable(cases_TX_select) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)

ggplot(cases_TX_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))

# Does death per case depend on population?
ggplot(cases_TX_select, mapping = aes(x= total_pop, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))

# What variables are correlated?
cor_TX <- cor(cases_TX_select[,-1])
ggcorrplot(cor_TX, p.mat = cor_pmat(cases_TX_select[,-1]), insig = "blank", hc.order = TRUE)

# Plot as a map (add variables to map data)
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename("county" = "subregion")
cases_TX <- cases_TX_select %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
counties_TX <- counties_TX %>% left_join(cases_TX %>% select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))

ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  # geom_text_repel(data = counties_TX %>% filter(complete.cases(.)) %>% group_by(county) %>% 
  #    summarize(long = mean(long), lat = mean(lat)) %>% mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People", subtitle = "Only counties reporting 100+ cases")

# Look at Dallas County over time. Are we flattening the curve?
cases_Dallas <- COVID_19_cases_TX %>% filter(county_name == "Dallas County" & state == "TX")
dim(cases_Dallas)
ggplot(cases_Dallas, aes(x = date, y = confirmed_cases)) + geom_line() + geom_smooth()

# You probably should look at the new cases per day # The Effect of Staying at Home
mobility <- Global_Mobility_Report %>% mutate_if(is.character, factor)
dim(mobility)
head(mobility)
summary(mobility)

mobility_Dallas <- mobility %>% filter(sub_region_1 == "Texas" & sub_region_2 == "Dallas County")
dim(mobility_Dallas)
mobility_Dallas
ggplot(mobility_Dallas, mapping = aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) + geom_line() + geom_smooth()
