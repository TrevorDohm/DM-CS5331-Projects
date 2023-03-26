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

# Read Data
COVID_19_cases_plus_census <- read_csv("Datasets/COVID-19_cases_plus_census_recent.csv")
County_Vaccine_Information <- read_csv("Datasets/County_Vaccine_Information.csv")

# Obtain County Names
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename("county" = "subregion")




# Vaccine Information Clustering

# Note Data Already Sorted In Ascending Order (sites_per_1k_ppl)
vaccineTX <- County_Vaccine_Information %>% mutate_if(is.character, factor)
vaccineTX <- vaccineTX %>% select(us_county, num_vaccine_sites, total_population, sites_per_1k_ppl)
summary(vaccineTX)
pairs(vaccineTX)

# Visualize Some Data Using Map
datatable(vaccineTX)
vaccineTX <- vaccineTX %>% mutate(county = us_county %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
counties_TX_clust <- counties_TX %>% left_join(vaccineTX)
ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = sites_per_1k_ppl)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Counties By Sites Per 1K People", subtitle = "Note Greyed Out Counties Are Non-Reporting")

# Take Numeric Data, Scale
scaledVaccineTX <- vaccineTX %>% 
  select(num_vaccine_sites, total_population, sites_per_1k_ppl) %>% 
  scale() %>% as_tibble()
summary(scaledVaccineTX)

# Perform K-Means
km <- kmeans(scaledVaccineTX, centers = 3)
km
pairs(scaledVaccineTX, col = km$cluster + 1L)

# Look At Cluster Profiles
ggplot(pivot_longer(as_tibble(km$centers, rownames = "cluster"), 
  cols = colnames(km$centers)), 
  aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

# Make County Name Match Map County Names
counties_TX_clust <- counties_TX %>% left_join(vaccineTX %>% add_column(cluster = factor(km$cluster)))
ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  labs(title = "Clusters - Vaccine Site Data", subtitle = "Note Greyed Out Counties Are Non-Reporting")

fviz_cluster(kmeans(scaledVaccineTX, centers = 3), data = scaledVaccineTX,
             centroids = TRUE,  geom = "point", ellipse.type = "norm")

d <- dist(scaledVaccineTX)
hc <- hclust(d, method = "complete")
fviz_dend(hc, k = 4)
fviz_cluster(list(data = scaledVaccineTX, cluster = cutree(hc, k = 4)), geom = "point")



# Project Two: Cluster Analysis

# Make Character Factors, Filter TX
cases <- COVID_19_cases_plus_census %>% mutate_if(is.character, factor)
cases_TX <- COVID_19_cases_plus_census %>% filter(state == "TX")

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
cluster_complete <- cases_TX_scaled %>%
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
  cols = colnames(km$centers)), 
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


# Housing

cases_TX <- COVID_19_cases_plus_census %>% filter(state == "TX")
cases_TX <- cases_TX %>% 
  filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, housing_units, housing_built_2005_or_later, housing_built_2000_to_2004, housing_built_1939_or_earlier)
pairs(cases_TX[2:5])
plot(cases_TX$housing_built_1939_or_earlier~ cases_TX$housing_units, data = cases_TX)

# Cluster cases_TX With K-means for years that housing was built
cases_TX_scaled <- cases_TX %>% 
  select(
    housing_built_1939_or_earlier,
    housing_built_2000_to_2004,
    housing_built_2005_or_later,
    housing_units
  ) %>% 
  scale() %>% as_tibble()
summary(cases_TX_scaled)

# Perform k-means
km <- kmeans(cases_TX_scaled, centers = 4)
km
pairs(cases_TX_scaled, col = km$cluster + 1L)

# Make County Name Match Map County Names
counties_TX_clust <- counties_TX %>% left_join(cases_TX %>% add_column(cluster = factor(km$cluster)))
ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")



# Data Explorer Code
# Explain These Data

plot_intro(COVID_19_cases_plus_census, title="Intro Plot for U.S. Covid-19 Cases and Census Dataset")
plot_intro(County_Vaccine_Information, title="Intro Plot for Texas County Vaccine Sites Dataset")
