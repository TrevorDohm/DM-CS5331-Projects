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
library(DataExplorer)
library(ggnewscale)
library(reshape2)
library(pastecs)

# Note: COVID_19_cases_TX Has Incrementing Data
# Run With Updated Census Data, Interesting Results
# Do Not Need To Run With Old TX Data; Incorporated (Same With GMR)



# Read Data

COVID_19_cases_plus_census <- read_csv("Datasets/COVID-19_cases_plus_census.csv")
COVID_19_cases_plus_census <- read_csv("Datasets/COVID-19_cases_plus_census_recent.csv") # Try This!
COVID_19_cases_TX <- read_csv("Datasets/COVID-19_cases_TX_updated.csv")
Global_Mobility_Report <- read_csv("Datasets/Global_Mobility_Report_current.csv", col_types =  cols(sub_region_2 = col_character()))
County_Vaccine_Information <- read_csv("Datasets/County_Vaccine_Information.csv")



# Data Explorer Code
# Explain These Data

plot_intro(COVID_19_cases_plus_census)
plot_intro(COVID_19_cases_TX)
plot_intro(Global_Mobility_Report)
plot_correlation(COVID_19_cases_TX)



# View Data

# https://www.hhs.gov/coronavirus/covid-19-vaccines/index.html
# Plotting Deaths, Cases Against Time (Dallas)

dallas_cases <- subset(COVID_19_cases_TX, county_name == "Dallas County")
cbp1 <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
cbp2 <- c("#555555", "#C21E56")

ggplot(dallas_cases, mapping = aes(x = date, y = confirmed_cases, label_both)) + 
  
  geom_vline(aes(xintercept = as.Date("2020-04-30"), color = "1. Social Distancing Starts"), linetype = "dashed", size = 1) + # Social Distancing Starts (Dallas)
  geom_vline(aes(xintercept = as.Date("2020-12-11"), color = "2. First Vaccine Releases"), linetype = "dashed", size = 1) + # Note: For Emergency Use, Persons Aged Over 16
  geom_vline(aes(xintercept = as.Date("2021-08-23"), color = "3. FDA Approves First Vaccine"), linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = as.Date("2022-01-31"), color = "4. FDA Approves Second Vaccine"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = as.Date("2022-06-17"), color = "5. Vaccine Authorized For Children"), linetype = "dashed", size = 1) +
  
  scale_color_manual(values = cbp1) +
  labs(title = "Plot Displaying Cases / Deaths Over Three Years (Dallas, TX)", color = "Markers", linetype = "dashed", x = "Time", y = "Cases / Deaths") +
  new_scale_color() + 
  
  geom_line(aes(y = confirmed_cases, color = "Cases"), size = 1) + geom_line(aes(y = deaths, color = "Deaths"), size = 1) +
  scale_color_manual(values = cbp2) +
  labs(color = "Datasets") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



# Make Character Factors, Filter TX

cases <- COVID_19_cases_plus_census %>% mutate_if(is.character, factor)
dim(cases)
cases_TX <- COVID_19_cases_plus_census %>% filter(state == "TX")
dim(cases_TX)
summary(cases_TX[, 1:10])



# Project Two: Cluster Analysis

# Calculate Rates, Select Important Variable
# You Need More Variables
cases_TX <- cases_TX %>% 
  filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income, median_age, poverty, commuters_by_public_transportation)
cases_TX <- cases_TX %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)
summary(cases_TX)

# Visualize Some Data Using Map
datatable(cases_TX) %>% formatRound(c(5, 9, 10), 2) %>% formatPercentage(11, 2)
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename("county" = "subregion")

# Make County Name Match Map County Names
cases_TX <- cases_TX %>% mutate(county = county_name %>% 
                                  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
counties_TX_clust <- counties_TX %>% left_join(cases_TX)
ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")

# Cluster cases_TX With k-means
cases_TX_scaled <- cases_TX %>% 
  select(
    median_income,
    median_age, 
    # total_pop, # you should use density
    poverty, 
    commuters_by_public_transportation
  ) %>% 
  scale() %>% as_tibble()
summary(cases_TX_scaled)

# Perform k-means
km <- kmeans(cases_TX_scaled, centers = 4)
km

# Look At Cluster Profiles
ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"), 
  cols = colnames(km$centers)), 
  aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

# Counties For Map
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion))
cases_TX <- cases_TX %>% mutate(county = county_name %>% 
                                  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

# Make County Name Match Map County Names
counties_TX_clust <- counties_TX %>% left_join(cases_TX %>% add_column(cluster = factor(km$cluster)))
ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")

# Note: Think About Outliers, Appropriate #Clusters, What Clusters Mean For Decision Maker
# Check If Cases / Deaths By 1000 People Are Different In Different Clusters:
cases_TX_km <- cases_TX %>% add_column(cluster = factor(km$cluster))
cases_TX_km %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))



# Feature Ranking (After Factorizing)
# Note: First Transform - Curse Of Dimensionality Example

transform_census <- as.data.frame(sapply(COVID_19_cases_plus_census, as.numeric))
transform_census <- transform_census %>% select_if(~ !any(is.na(.))) %>% select(-c(date, do_date))
cor_census <- cor(transform_census[, -1])
high_cor <- findCorrelation(cor_census, cutoff = 0.99995)
colnames(transform_census)



# Good Example Of Feature Extraction

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model <- train(confirmed_cases~., data = transform_census, method = "lm", preProcess = "scale", trControl = control)
importance <- varImp(model, scale = FALSE)
print(importance, top = 20)
plot(importance, top = 20)

cor_census_new <- transform_census[c('deaths', 'income_less_10000', 'income_10000_14999', 
                                     'income_50000_59999', 'income_150000_199999')]
plot_correlation(cor_census_new, title = 'Correlation Among Important Census Data')
cor_census_new <- transform_census[c('deaths', 'black_pop', 'income_10000_14999', 'black_including_hispanic', 
                                     'different_house_year_ago_same_city', 'commuters_by_car_truck_van',
                                     'hispanic_male_45_54', 'white_including_hispanic', 'hispanic_pop',
                                     'million_dollar_housing_units', 'walked_to_work', 'income_50000_59999',
                                     'income_150000_199999', 'in_grades_9_to_12', 'white_pop',
                                     'management_business_sci_arts_employed', 'rent_35_to_40_percent',
                                     'male_45_64_high_school', 'income_less_10000', 'commuters_by_public_transportation')]
summary(cor_census_new)
options(scipen = 100)
options(digits = 3)
stat.desc(cor_census_new, basic = F)

# Are There Many Counties With Cases?
# Many Counties (Unpopulated) With Low Cases; Few (Dallas, Austin, Houston) With Many Cases

ggplot(cases_TX, mapping = aes(confirmed_cases)) + geom_histogram(bins = 20)



# Relationship Between Cases / Deaths
# More Population -> More Cases, More Deaths
# Note The Second Graph Simply Has An Added Fitting Line (Labels)

ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, size = total_pop)) + geom_point()
ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX, deaths >= 1000)) 



# Calculate Rates (Per 1000 People)
# As Cases Increases, Deaths Does (Positive Correlation)

cases_TX_select <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income)

cases_TX_select <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases / total_pop * 1000, 
  deaths_per_1000 = deaths / total_pop * 1000, 
  death_per_case = deaths / confirmed_cases)

head(cases_TX_select)
datatable(cases_TX_select) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)

ggplot(cases_TX_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))



# Does Death Per Case Depend On Population?
# Yes, It Seems There Are Less Deaths In Populated Areas

ggplot(cases_TX_select, mapping = aes(x = total_pop, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))



# What variables are correlated?
# Expected Results (Check Uncorrelated)

cor_TX <- cor(cases_TX_select[, -1])
ggcorrplot(cor_TX, p.mat = cor_pmat(cases_TX_select[, -1]), insig = "blank", hc.order = TRUE)



# Plot As Map (Add Vars To Data Map)
# Visualize Amount Of Cases Per County

counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename("county" = "subregion")
cases_TX <- cases_TX_select %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
counties_TX <- counties_TX %>% left_join(cases_TX %>% select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))
counties_TX[counties_TX$county == 'loving', 'cases_per_1000'] <- NA

ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  coord_quickmap() + 
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "COVID-19 Cases Per 1000 People", subtitle = "Only Counties Reporting 100+ Cases")




# Worst Counties - Interestingly, The Counties With The Most Deaths Did Not Have Any Vaccine Sites (Unlisted)
# Panhandle Needs To Get Their Act Together!

counties_TX %>% arrange((desc(cases_per_1000))) # Childress, Hale (Both Panhandle, Hale Is East Adjacent To Lamb)
counties_TX %>% arrange((desc(deaths_per_1000))) # Lamb, Cottle (Both Panhandle, Cottle Is South Adjacent To Childress)
counties_TX %>% arrange((desc(death_per_case))) # Sherman, Garza (Both Panhandle, Sherman Borders Oklahoma)
filter(County_Vaccine_Information, us_county %in% c("Childress County", "Hale County"))



# Note: In Current Data, Worst Counties Are Loving (Cases, Deaths) McMullen, (Deaths), And Sabine (Deaths / Case)
# Both Loving And McMullen Have No Vaccine Sites, Sabine With A Very Low Rate

filter(County_Vaccine_Information, us_county %in% c("Sabine County"))



# Best Counties (Same Terms As Above)
# Note For Current Data: El Paso, Hartley, Loving Did Best (However, The Reason Why Loving Did Best
# Is Because They Had So Many Cases And Deaths That The Death Per Case Rate Was Low)

counties_TX %>% arrange(cases_per_1000) # San Jacinto (Southeastern Texas)
counties_TX %>% arrange(deaths_per_1000) # Shackelford, Chambers (Central Panhandle)
counties_TX %>% arrange(death_per_case) # Chambers (Southeastern Texas)
filter(County_Vaccine_Information, us_county %in% c("San Jacinto County", "Chambers County"))



# Adds Labels, Too Many!
# geom_text_repel(data = counties_TX %>% filter(complete.cases(.)) %>% group_by(county) %>% 
# summarize(long = mean(long), lat = mean(lat)) %>% mutate(county = str_to_title(county))) +

# Dallas County Over Time. Are We Flattening The Curve?
# As Seen Above, Vaccine Releases Seem To Correlate To Curve Flattening (Not Social Distancing)

cases_Dallas <- COVID_19_cases_TX %>% filter(county_name == "Dallas County" & state == "TX")
dim(cases_Dallas)
ggplot(cases_Dallas, aes(x = date, y = confirmed_cases)) + geom_line() + geom_smooth()



# California (Same As Above)

cases_CA <- COVID_19_cases_plus_census %>% filter(state == "CA")
dim(cases_CA)
summary(cases_CA[, 1:10])

cases_CA_select <- cases_CA %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income)

cases_CA_select <- cases_CA_select %>% mutate(
  cases_per_1000 = confirmed_cases / total_pop * 1000, 
  deaths_per_1000 = deaths / total_pop * 1000, 
  death_per_case = deaths / confirmed_cases)

head(cases_CA_select)
datatable(cases_CA_select) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)

ggplot(cases_CA_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_CA_select, deaths_per_1000 > quantile(deaths_per_1000, .90)))

ggplot(cases_CA_select, mapping = aes(x = total_pop, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_CA_select, deaths_per_1000 > quantile(deaths_per_1000, .90)))

cor_CA <- cor(cases_CA_select[, -1])
ggcorrplot(cor_CA, p.mat = cor_pmat(cases_CA_select[, -1]), insig = "blank", hc.order = TRUE)

counties_CA <- counties %>% dplyr::filter(region == "california") %>% rename("county" = "subregion")
cases_CA <- cases_CA_select %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
counties_CA <- counties_CA %>% left_join(cases_CA %>% select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))
# counties_CA[counties_CA$county == 'loving', 'cases_per_1000'] <- NA

ggplot(counties_CA, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  coord_quickmap() + 
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "COVID-19 Cases Per 1000 People", subtitle = "Only Counties Reporting 100+ Cases")

counties_CA %>% arrange((desc(cases_per_1000))) # Kings
counties_CA %>% arrange((desc(deaths_per_1000))) # Imperial
counties_CA %>% arrange((desc(death_per_case))) # Siskiyou

counties_CA %>% arrange(cases_per_1000) # Modoc
counties_CA %>% arrange(deaths_per_1000) # Alpine
counties_CA %>% arrange(death_per_case) # Alpine



# Check New Cases Per Day, # Effect Of Staying At Home

mobility <- Global_Mobility_Report %>% mutate_if(is.character, factor)
dim(mobility)
head(mobility)
summary(mobility)

mobility_Dallas <- mobility %>% filter(sub_region_1 == "Texas" & sub_region_2 == "Dallas County")
dim(mobility_Dallas)
mobility_Dallas
ggplot(mobility_Dallas, mapping = aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) + geom_line() + geom_smooth()



# Perform Left Outer Join On Global Mobility Report, COVID-19 Census Data
# => Specifically Joining On Census Fips Code

# Rename Column To Match Them, Remove Non-US Data
Global_Mobility_ReportEdit <- Global_Mobility_Report %>% rename("county_fips_code" = "census_fips_code")
Global_Mobility_ReportEdit <- Global_Mobility_ReportEdit %>% filter(!is.na(county_fips_code)) 

# Remove Puerto Rico Data
Global_Mobility_ReportEdit <- subset(Global_Mobility_ReportEdit, Global_Mobility_ReportEdit$country_region_code != "PR")

# Remove NA, Unnecessary Columns
Global_Mobility_ReportEdit <- Global_Mobility_ReportEdit[ , colSums(is.na(Global_Mobility_ReportEdit)) < nrow(Global_Mobility_ReportEdit)]
Global_Mobility_ReportEdit <- Global_Mobility_ReportEdit[ , !names(Global_Mobility_ReportEdit) %in% c("iso_3166_2_code")]

# Select Columns From United States Data
columnsOfInterest <- COVID_19_cases_plus_census %>% select(county_fips_code, confirmed_cases, deaths)

# Perform Left Outer Join
casesAndDeathsVSColumnsOfInterest <- Global_Mobility_ReportEdit %>% left_join(columnsOfInterest, by="county_fips_code")

# Plot With Cleaner Data
plot_intro(casesAndDeathsVSColumnsOfInterest, title = "Intro Plot for US Cases/Deaths and US Mobility Statistics")

# Remove Non-Numeric Columns
transform_census2 <- casesAndDeathsVSColumnsOfInterest %>% select(!c(country_region_code, country_region, sub_region_1, sub_region_2, place_id, date, county_fips_code))

# Find Correlated Variables (Cases / Deaths X Mobility Data)
transform_census2 <- as.data.frame(sapply(transform_census2, as.numeric))
cor_census2 <- cor(transform_census2, use = "na.or.complete")
high_cor2 <- findCorrelation(cor_census2, cutoff = 0.99995)
colnames(transform_census2)

cor_Mobility_and_Census <- cor(transform_census2, use = "na.or.complete")
ggcorrplot(cor_Mobility_and_Census, p.mat = cor_pmat(transform_census2), type = "upper", title = "Correlation Matrix for US COVID-19 Cases/Deaths and US Mobility Statistics",insig = "blank", hc.order = TRUE, lab = TRUE ,colors = c("blue", "white", "orange"))

# Explore Correlation Between Deaths, Ethnicities
casesAndDeathsVSColumnsOfInterest <- COVID_19_cases_plus_census %>% select(confirmed_cases, deaths, white_pop, black_pop, asian_pop, hispanic_pop, amerindian_pop, other_race_pop, two_or_more_races_pop)

# Plot With Cleaner Data
plot_intro(casesAndDeathsVSColumnsOfInterest, title = "Intro Plot for US Cases/Deaths and US Ethnicity Statistics")

# Find Correlated Variables (Cases / Deaths X Mobility Data)
transform_census3 <- as.data.frame(sapply(casesAndDeathsVSColumnsOfInterest, as.numeric))
cor_census3 <- cor(transform_census3, use = "na.or.complete")
high_cor3 <- findCorrelation(cor_census3, cutoff = 0.99995)
colnames(transform_census3)

cor_Deaths_and_Census_Ethnicity <- cor(transform_census3, use = "na.or.complete")
ggcorrplot(cor_Deaths_and_Census_Ethnicity, p.mat = cor_pmat(transform_census3), type = "full", title = "Correlation Matrix for US COVID-19 Cases/Deaths and Ethnicity Statistics",insig = "blank", hc.order = TRUE, lab = TRUE ,colors = c("blue", "white", "orange"))



# Plots With Ethnicities And Deaths

casesAndDeathsVSColumnsOfInterest <- COVID_19_cases_plus_census %>% select(confirmed_cases, deaths, white_pop, black_pop, asian_pop, hispanic_pop, amerindian_pop, other_race_pop, two_or_more_races_pop)
casesAndDeathsVSColumnsOfInterest <- data.frame(casesAndDeathsVSColumnsOfInterest)
ggplot(data=data.frame(casesAndDeathsVSColumnsOfInterest), mapping = aes(x = hispanic_pop , y = deaths)) + 
  geom_point(aes(color = "red")) +
  geom_smooth() +
  labs(title = "Plot Displaying Deaths vs. Hispanic Population in each US county", x = "hispanic_pop", y = "deaths") +
  theme_bw()

ggplot(data=data.frame(casesAndDeathsVSColumnsOfInterest), mapping = aes(x = white_pop , y = deaths)) + 
  geom_point(aes(color = "red")) +
  geom_smooth() +
  labs(title = "Plot Displaying Deaths vs. White Population in each US county", x = "white_pop", y = "deaths") +
  theme_bw()

ggplot(data=data.frame(casesAndDeathsVSColumnsOfInterest), mapping = aes(x = black_pop , y = deaths)) + 
  geom_point(aes(color = "red")) +
  geom_smooth() +
  labs(title = "Plot Displaying Deaths vs. Black Population in each US county", x = "black_pop", y = "deaths") +
  theme_bw()

ggplot(data=data.frame(casesAndDeathsVSColumnsOfInterest), mapping = aes(x = asian_pop , y = deaths)) + 
  geom_point(aes(color = "red")) +
  geom_smooth() +
  labs(title = "Plot Displaying Deaths vs. Asian Population in each US county", x = "asian_pop", y = "deaths") +
  theme_bw()

ggplot(data=data.frame(casesAndDeathsVSColumnsOfInterest), mapping = aes(x = amerindian_pop , y = deaths)) + 
  geom_point(aes(color = "red")) +
  geom_smooth() +
  labs(title = "Plot Displaying Deaths vs. Amerindian Population in each US county", x = "amerindian_pop", y = "deaths") +
  theme_bw()

casesAndDeathsVSColumnsOfInterest <- subset(casesAndDeathsVSColumnsOfInterest, select=-c(confirmed_cases))



# Plotting On Same Plot:
# https://www.statology.org/plot-multiple-columns-in-r/

df <- melt(casesAndDeathsVSColumnsOfInterest, id.vars = 'deaths', variable.name = 'populations')

# Create Line Plot For Each Column In Data Frame
ggplot(df, aes(deaths, value)) +
  geom_point(aes(colour = populations)) +
  labs(title="Each US county's population vs. their deaths", y="Population", subtitle = "Filtered by Race/Ethnicity")

# Ethnicity Vs Deaths (Texas)
COVID_cases_plus_census_TX <- subset(COVID_19_cases_plus_census, state == "TX")
COVID_cases_plus_census_TX <- COVID_cases_plus_census_TX %>% select(deaths, white_pop, black_pop, asian_pop, hispanic_pop, amerindian_pop, other_race_pop, two_or_more_races_pop)
COVID_cases_plus_census_TX <- data.frame(COVID_cases_plus_census_TX)



# Graph By Ethnicity

ggplot(data=data.frame(COVID_cases_plus_census_TX), mapping = aes(x = hispanic_pop , y = deaths)) + 
  geom_point(aes(color = "red")) +
  geom_smooth() +
  labs(title = "Plot Displaying Deaths vs. Hispanic Population in each TX county", x = "hispanic_pop", y = "deaths") +
  theme_bw()

ggplot(data=data.frame(COVID_cases_plus_census_TX), mapping = aes(x = white_pop , y = deaths)) + 
  geom_point(aes(color = "red")) +
  geom_smooth() +
  labs(title = "Plot Displaying Deaths vs. White Population in each TX county", x = "white_pop", y = "deaths") +
  theme_bw()

ggplot(data=data.frame(COVID_cases_plus_census_TX), mapping = aes(x = black_pop , y = deaths)) + 
  geom_point(aes(color = "red")) +
  geom_smooth() +
  labs(title = "Plot Displaying Deaths vs. Black Population in each TX county", x = "black_pop", y = "deaths") +
  theme_bw()

ggplot(data=data.frame(COVID_cases_plus_census_TX), mapping = aes(x = asian_pop , y = deaths)) + 
  geom_point(aes(color = "red")) +
  geom_smooth() +
  ylim(0, 40000) +
  labs(title = "Plot Displaying Deaths vs. Asian Population in each TX county", x = "asian_pop", y = "deaths") +
  theme_bw()

ggplot(data=data.frame(COVID_cases_plus_census_TX), mapping = aes(x = amerindian_pop , y = deaths)) + 
  geom_point(aes(color = "red")) +
  geom_smooth() +
  labs(title = "Plot Displaying Deaths vs. Amerindian Population in each TX county", x = "amerindian_pop", y = "deaths") +
  theme_bw()

# Plot All On Same Plot
# https://www.statology.org/plot-multiple-columns-in-r/
df <- melt(COVID_cases_plus_census_TX, id.vars = 'deaths', variable.name = 'populations')

# Create Line Plot For Each Column
ggplot(df, aes(deaths, value)) +
  geom_point(aes(colour = populations)) +
  labs(title="Each TX county's population vs. their deaths", y="Population", subtitle = "Filtered by Race/Ethnicity")
