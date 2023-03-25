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
library(dbscan)
library(cluster)
library(FactoMineR)
library(factoextra)

# Note: COVID_19_cases_TX Has Incrementing Data
# Run With Updated Census Data, Interesting Results
# Do Not Need To Run With Old TX Data; Incorporated (Same With GMR)



# Read Data

COVID_19_cases_plus_census <- read_csv("Datasets/COVID-19_cases_plus_census_recent.csv")
County_Vaccine_Information <- read_csv("Datasets/County_Vaccine_Information.csv")



# Make Character Factors, Filter TX

cases <- COVID_19_cases_plus_census %>% mutate_if(is.character, factor)
dim(cases)
cases_TX <- COVID_19_cases_plus_census %>% filter(state == "TX")
dim(cases_TX)
summary(cases_TX[, 1:10])



# Project Two: Cluster Analysis

# Calculate Rates, Select Important Variables

cases_TX <- cases_TX %>% 
  filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income, median_age, poverty, commuters_by_public_transportation)
cases_TX <- cases_TX %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)
summary(cases_TX)
pairs(cases_TX[2:11])

# Visualize Some Data Using Map

datatable(cases_TX) %>% formatRound(c(5, 9, 10), 2) %>% formatPercentage(11, 2)
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename("county" = "subregion")

# Make County Name Match Map County Names

cases_TX <- cases_TX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
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
    deaths_per_1000,
    death_per_case,
    poverty, 
    commuters_by_public_transportation
  ) %>% 
  scale() %>% as_tibble()
summary(cases_TX_scaled)

# Perform k-means
km <- kmeans(cases_TX_scaled, centers = 3)
km
pairs(cases_TX_scaled, col = km$cluster + 1L)

# Look At Cluster Profiles
ggplot(pivot_longer(as_tibble(km$centers, rownames = "cluster"), 
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

d <- dist(cases_TX_scaled)
hc <- hclust(d, method = "complete")
plot(hc)
fviz_dend(hc, k = 4)

clusters <- cutree(hc, k = 4)
cluster_complete <- ruspini_scaled %>%
  add_column(cluster = factor(clusters))
cluster_complete




# KNN Distance Plot
kNNdist(cases_TX_scaled, k = 4)
kNNdist(cases_TX_scaled, k = 4)
kNNdistplot(cases_TX_scaled, k = 4)

# Uses Euclidean Distance
db <- dbscan(cases_TX_scaled, eps = 1.0, minPts = 5)
db
pairs(cases_TX_scaled, col = db$cluster + 1L)

data(iris)
iris <- as.matrix(iris[, 1:4])
kNNdist(iris, k = 4)
kNNdist(iris, k = 4, all = TRUE)
kNNdistplot(iris, k = 4)
cl <- dbscan(iris, eps = .7, minPts = 5)
pairs(iris, col = cl$cluster + 1L)

# Look At Cluster Profiles
ggplot(pivot_longer(as_tibble(km$centers, rownames = "cluster"), 
  cols = colnames(db$centers)), 
  aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

opt <- optics(cases_TX_scaled, eps = 1, minPts = 4)
opt
opt <- extractDBSCAN(opt, eps_cl = 0.4)
plot(opt)

hdb <- hdbscan(cases_TX_scaled, minPts = 4)
hdb
plot(hdb, show_flat = TRUE)






# Data Explorer Code
# Explain These Data

plot_intro(COVID_19_cases_plus_census, title="Intro Plot for U.S. Covid-19 Cases and Census Dataset")
plot_intro(COVID_19_cases_TX, title="Intro Plot for Texas Covid-19 Cases Dataset")
plot_intro(Global_Mobility_Report, title="Intro Plot for Global Mobility Report Dataset")
plot_intro(County_Vaccine_Information, title="Intro Plot for Texas County Vaccine Sites Dataset")
plot_correlation(COVID_19_cases_TX)
