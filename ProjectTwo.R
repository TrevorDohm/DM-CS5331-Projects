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

# Data Explorer Code
plot_intro(COVID_19_cases_plus_census, title="Intro Plot for U.S. Covid-19 Cases and Census Dataset")
plot_intro(County_Vaccine_Information, title="Intro Plot for Texas County Vaccine Sites Dataset")

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
vaccineClustTX <- counties_TX %>% left_join(vaccineTX)
ggplot(vaccineClustTX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = sites_per_1k_ppl)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Counties By Vaccine Sites Per 1K People", subtitle = "Note Greyed Out Counties Are Non-Reporting")

# Take Numeric Data, Scale
scaledVaccineTX <- vaccineTX %>% 
  select(num_vaccine_sites, total_population, sites_per_1k_ppl) %>% 
  scale() %>% as_tibble()

# Perform K-Means
vaccineKM <- kmeans(scaledVaccineTX, centers = 3, nstart = 10)
vaccineKM
pairs(scaledVaccineTX, col = vaccineKM$cluster + 1L)

# Visualize Singular Plots (Cluster)
clustersVaccineKM <- scaledVaccineTX %>% add_column(cluster = factor(vaccineKM$cluster))
vaccineCentroids <- as_tibble(vaccineKM$centers, rownames = "cluster")
fviz_cluster(vaccineKM, data = scaledVaccineTX, choose.vars = c("sites_per_1k_ppl", "total_population"), 
             centroids = TRUE, ellipse.type = "norm", geom = "point", main = "Single Pair Plot; Total Population Vs. Sites Per 1K People")
fviz_cluster(vaccineKM, data = scaledVaccineTX, choose.vars = c("num_vaccine_sites", "total_population"), 
             centroids = TRUE, ellipse.type = "norm", geom = "point", main = "Single Pair Plot; Number Of Vaccine Sites Vs. Sites Per 1K People")

# Look At Cluster Profiles
ggplot(pivot_longer(vaccineCentroids, 
  cols = colnames(vaccineKM$centers)), 
  aes(x = value, y = name, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) +
  scale_fill_viridis_d() 

# Look At First Cluster
vaccineC1 <- clustersVaccineKM %>% filter(cluster == 1)
summary(vaccineC1)
ggplot(vaccineC1, aes(sites_per_1k_ppl, total_population)) + geom_point()

# Make County Name Match Map County Names
vaccineClustKMTX <- counties_TX %>% left_join(vaccineTX %>% add_column(cluster = factor(vaccineKM$cluster)))
ggplot(vaccineClustKMTX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d(na.value = "gray50") +
  labs(title = "Clusters - Vaccine Site Data", subtitle = "Note Greyed Out Counties Are Non-Reporting")

# Hierarchical Clustering
distVaccine <- dist(scaledVaccineTX)
hcVaccine <- hclust(distVaccine, method = "complete")
fviz_dend(hcVaccine, k = 3)
fviz_cluster(list(data = scaledVaccineTX, cluster = cutree(hcVaccine, k = 3)), choose.vars = c("num_vaccine_sites", "total_population"), geom = "point")

# Visualize Single-Link Dendrogram
singleVaccine <- hclust(distVaccine, method = "single")
fviz_dend(singleVaccine, k = 3)

# Visualize Clustering
clustersVaccineH <- cutree(hcVaccine, k = 3)
completeVaccineH <- scaledVaccineTX %>%
  add_column(cluster = factor(clustersVaccineH))
completeVaccineH
ggplot(completeVaccineH, aes(num_vaccine_sites, total_population, color = cluster)) + geom_point()

# Make County Name Match Map County Names
vaccineClustHTX <- counties_TX %>% left_join(vaccineTX %>% add_column(cluster = factor(clustersVaccineH)))
ggplot(vaccineClustHTX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d(na.value = "gray50") +
  labs(title = "Clusters - Vaccine Site Data", subtitle = "Note Greyed Out Counties Are Non-Reporting")



# Housing Clustering

# Sort Data In Descending Order (Amount Housing Units)
housingTX <- COVID_19_cases_plus_census %>% mutate_if(is.character, factor) %>% filter(state == "TX")
housingTX <- housingTX %>% arrange(desc(housing_units)) %>%    
  select(county_name, housing_units, housing_built_2005_or_later, housing_built_2000_to_2004, housing_built_1939_or_earlier)
summary(housingTX)
pairs(housingTX)

# Visualize Some Data Using Map
datatable(housingTX)
housingTX <- housingTX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
housingClustTX <- counties_TX %>% left_join(housingTX)
ggplot(housingClustTX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = housing_units)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Counties By Number Housing Units")

# Take Numeric Data, Scale
scaledHousingTX <- housingTX %>% 
  select(housing_built_1939_or_earlier, housing_built_2000_to_2004, housing_built_2005_or_later, housing_units) %>% 
  scale() %>% as_tibble()

# Perform K-Means
housingKM <- kmeans(scaledHousingTX, centers = 5, nstart = 10)
housingKM
pairs(scaledHousingTX, col = housingKM$cluster + 1L)

# Visualize Singular Plots (Cluster)
clustersHousingKM <- scaledHousingTX %>% add_column(cluster = factor(housingKM$cluster))
housingCentroids <- as_tibble(housingKM$centers, rownames = "cluster")
fviz_cluster(housingKM, data = scaledHousingTX, choose.vars = c("housing_units", "housing_built_2000_to_2004"), 
             centroids = TRUE, ellipse.type = "norm", geom = "point", main = "Single Pair Plot; Number Housing Units Vs. Housing Built: 2000 - 2004")

# Look At Cluster Profiles
ggplot(pivot_longer(housingCentroids, 
  cols = colnames(housingKM$centers)), 
  aes(x = value, y = name, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) +
  scale_fill_viridis_d() 

# Look At First Cluster
housingC1 <- clustersHousingKM %>% filter(cluster == 1)
summary(housingC1)
ggplot(housingC1, aes(housing_units, housing_built_2000_to_2004)) + geom_point()

# Make County Name Match Map County Names
housingClustKMTX <- counties_TX %>% left_join(housingTX %>% add_column(cluster = factor(housingKM$cluster)))
ggplot(housingClustKMTX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d(na.value = "gray50") +
  labs(title = "Clusters - Housing Data")



# Education Level (??? Clustering)

educationLeveLTX <- COVID_19_cases_plus_census %>% filter(state == "TX")
educationLeveLTX <- educationLeveLTX %>% 
  filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, total_pop, some_college_and_associates_degree, associates_degree, bachelors_degree, high_school_diploma, masters_degree, less_than_high_school_graduate)
pairs(educationLeveLTX[2:7])

# Cluster cases_TX With K-means for years that housing was built
scaledEducationLevelTX <- educationLeveLTX %>% 
  select(
    total_pop, 
    some_college_and_associates_degree, 
    associates_degree, 
    bachelors_degree, 
    high_school_diploma, 
    masters_degree, 
    less_than_high_school_graduate
  ) %>% 
  scale() %>% as_tibble()
summary(scaledEducationLevelTX)

# Perform k-means
km <- kmeans(scaledEducationLevelTX, centers = 4)
km
pairs(scaledEducationLevelTX, col = km$cluster + 1L)

# Make County Name Match Map County Names
educationLeveLTX <- educationLeveLTX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
counties_TX_clust <- counties_TX %>% left_join(educationLeveLTX %>% add_column(cluster = factor(km$cluster)))
ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")

# Hierarchical Clustering
d <- dist(scaledEducationLevelTX)
hc <- hclust(d, method = "complete")
plot(hc)
fviz_dend(hc, k = 3)

clusters <- cutree(hc, k = 3)
cluster_complete <- scaledEducationLevelTX %>%
  add_column(cluster = factor(clusters))
cluster_complete

fviz_cluster(list(data = scaledEducationLevelTX, cluster = cutree(hc, k = 3)), geom = "point", choose.vars = c("bachelors_degree", "total_pop"))

# Make County Name Match Map County Names
educationLeveLTX <- educationLeveLTX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
counties_TX_clust <- counties_TX %>% left_join(educationLeveLTX %>% add_column(cluster = factor(cluster_complete$cluster)))
ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")

# DBSCAN
kNNdistplot(scaledEducationLevelTX, k = 3)
abline(h = 1, col = "red")
# Uses Euclidean Distance
db <- dbscan(scaledEducationLevelTX, eps = 1, minPts = 5)
db
str(db)
ggplot(scaledEducationLevelTX %>% add_column(cluster = factor(db$cluster)),
       aes(bachelors_degree, total_pop, color = cluster)) + geom_point()
fviz_cluster(db, scaledEducationLevelTX, choose.vars = c("bachelors_degree", "total_pop"), geom = "point")



# Commute (??? Clustering)

commuteTX <- COVID_19_cases_plus_census %>% filter(state == "TX")
commuteTX <- commuteTX %>% 
  filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, 
         total_pop, 
         commute_5_9_mins,
         commute_10_14_mins, 
         commute_15_19_mins, 
         commute_20_24_mins, 
         commute_25_29_mins,
         commute_30_34_mins,
         commute_35_39_mins,
         commute_40_44_mins,
         commute_45_59_mins,
         commute_60_89_mins,
         commute_90_more_mins)
pairs(commuteTX[2:6])

# Cluster cases_TX With K-means for commute
scaledCommuteTX <- commuteTX %>% 
  select(
    total_pop, 
    commute_5_9_mins,
    commute_10_14_mins, 
    commute_15_19_mins, 
    commute_20_24_mins, 
    commute_25_29_mins,
    commute_30_34_mins,
    commute_35_39_mins,
    commute_40_44_mins,
    commute_45_59_mins,
    commute_60_89_mins,
    commute_90_more_mins
  ) %>% 
  scale() %>% as_tibble()
summary(scaledCommuteTX)

# Perform k-means
km <- kmeans(scaledCommuteTX, centers = 5)
km
pairs(scaledCommuteTX, col = km$cluster + 1L)

# Make County Name Match Map County Names
commuteTX <- commuteTX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
counties_TX_clust <- counties_TX %>% left_join(commuteTX %>% add_column(cluster = factor(km$cluster)))
ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")

# Hierarchical Clustering
d <- dist(scaledCommuteTX)
hc <- hclust(d, method = "complete")
plot(hc)
fviz_dend(hc, k = 5)

clusters <- cutree(hc, k = 5)
cluster_complete <- scaledCommuteTX %>%
  add_column(cluster = factor(clusters))
cluster_complete

fviz_cluster(list(data = scaledCommuteTX, cluster = cutree(hc, k = 5)), geom = "point", choose.vars = c("commute_15_19_mins", "total_pop"))

# Make County Name Match Map County Names
commuteTX <- commuteTX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
counties_TX_clust <- counties_TX %>% left_join(commuteTX %>% add_column(cluster = factor(cluster_complete$cluster)))
ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")

# DBSCAN
kNNdistplot(scaledCommuteTX, k = 3)
abline(h = 1, col = "red")
# Uses Euclidean Distance
db <- dbscan(scaledCommuteTX, eps = 1, minPts = 5)
db
str(db)
ggplot(scaledCommuteTX %>% add_column(cluster = factor(db$cluster)),
       aes(commute_15_19_mins, total_pop, color = cluster)) + geom_point()
fviz_cluster(db, scaledCommuteTX, choose.vars = c("commute_15_19_mins", "total_pop"), geom = "point")










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



