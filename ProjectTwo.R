# IMPORTS

# library()
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
library(vtable)
library(NbClust)
library(seriation)





# FUNCTIONS

# Calculate Entropy Given Ground Truth
entropy <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  w <- table(cluster)/length(cluster)
  cnts <- sapply(split(truth, cluster), table)
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  e <- -p * log(p, 2)
  sum(w * rowSums(e, na.rm = TRUE))
}

# Calculate Purity Given Ground Truth
purity <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  w <- table(cluster)/length(cluster)
  cnts <- sapply(split(truth, cluster), table)
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  sum(w * apply(p, 1, max))
}





# DATA CLEANING

# Read Data
casesCensus <- read_csv("Datasets/COVID-19_cases_plus_census_recent.csv")
vaccineInfo <- read_csv("Datasets/County_Vaccine_Information.csv")
casesDeaths <- read_csv("Datasets/COVID-19_cases_TX_updated.csv")

# Summary Before Manipulation
summary(casesCensus)
summary(vaccineInfo)
summary(casesDeaths)
sumtable(casesCensus, out = 'htmlreturn')
sumtable(vaccineInfo, out = 'htmlreturn')
sumtable(casesDeaths, out = 'htmlreturn')

# Obtain County Names
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename("county" = "subregion")

# Data Explorer Code
plot_intro(casesCensus, title = "Intro Plot for U.S. Covid-19 Cases and Census Dataset")
plot_intro(vaccineInfo, title = "Intro Plot for Texas County Vaccine Sites Dataset")
plot_intro(casesDeaths, title = "Intro Plot for Texas Cases And Deaths")
plot_intro(counties_TX, title = "Intro Plot for Texas County Positions")

# Clean Vaccine Data (Scale, Remove Columns)
vaccineInfo <- vaccineInfo  %>% mutate_if(is.character, factor)
vaccineNonNumeric <- vaccineInfo %>% select_if(~!is.numeric(.))
vaccineNumeric <- vaccineInfo %>% select_if(is.numeric) %>% scale() %>% as_tibble()
vaccineInfo <- vaccineNumeric %>% add_column(vaccineNonNumeric$us_county) %>% 
  rename("us_county" = "vaccineNonNumeric$us_county")

# Clean Census Data (Filter, Factor, Remove Obsolete Column, Add New Columns)
casesCensus <- casesCensus %>% filter(state == "TX") %>% 
  mutate_if(is.character, factor) %>% select(-last_col()) %>% mutate(
  cases_per_1000 = confirmed_cases / total_pop * 1000, 
  deaths_per_1000 = deaths / total_pop * 1000, 
  death_per_case = deaths / confirmed_cases)

# Take Numeric Data, Remove NA To Perform Dimensionality Reduction
casesCensusUpdated <- casesCensus %>% select_if(is.numeric) %>% 
  add_column(casesCensus$county_name) %>% 
  rename("county_name" = "casesCensus$county_name") %>% na.omit()

# Take Numerical Data, Scale
casesCensusUpdatedNumeric <- casesCensusUpdated %>% select_if(is.numeric) %>% scale() %>% as_tibble()

# Initial Correlation Matrix - Let's Reduce Dimension
corrMatrix <- cor(casesCensusUpdatedNumeric)

# Remove Highly Correlated Variables, Show New Matrix
corrMatrixRemove <- corrMatrix
corrMatrixRemove[upper.tri(corrMatrixRemove)] <- 0
diag(corrMatrixRemove) <- 0
corrMatrixRemove
casesCensusUpdatedNumeric <- casesCensusUpdatedNumeric[, !apply(corrMatrixRemove, 2, function(x) any(x > 0.95))]
corrMatrix <- cor(casesCensusUpdatedNumeric)
ggcorrplot(corrMatrix, insig = "blank", hc.order = TRUE) + ggtitle("Correlation Matrix After Removing Highly Correlated Variables")

# Perform PCA
casesCensusUpdatedNumericPCA <- princomp(corrMatrix)
summary(casesCensusUpdatedNumericPCA)
casesCensusUpdatedNumericPCA$loadings[, 1:2]

# Plot PCA Results
fviz_eig(casesCensusUpdatedNumericPCA, addlabels = TRUE) + ggtitle("Scree Plot - PCA")
fviz_pca_var(casesCensusUpdatedNumericPCA, col.var = "black")
fviz_cos2(casesCensusUpdatedNumericPCA, choice = "var", axes = 1:2) + ggtitle("Cos2 of Variables to Dim-1-2 - PCA")
fviz_pca_var(casesCensusUpdatedNumericPCA, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"), repel = TRUE)

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


# Summary After Manipulation
summary(dataFinal)
sumtable(dataFinal, out = 'htmlreturn')

# Subset Original Data With Found Features
casesCensusFinal <- casesCensus %>% select(colnames(dataFinal), 'poverty', 'commuters_by_public_transportation')
casesCensusFinal <- casesCensusFinal %>% select_if(is.numeric) %>% 
  scale() %>% as_tibble() %>% add_column(casesCensus$county_name) %>% 
  rename("county" = "casesCensus$county_name") %>% 
  select(county, everything())

# Data Explorer Code
plot_intro(casesCensusFinal, title = "Intro Plot for Finalized Census Dataset")
datatable(casesCensusFinal) %>% formatRound(c(5, 9, 10), 2) %>% formatPercentage(11, 2)
# NOTE: (dataFinal$owner_occupied_housing_units_upper_value_quartile) Has Two NA Values

# Add County Name To Final Data For All Future Map Plots
casesCensusFinal <- casesCensusFinal %>% mutate(county = county %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))


# Subset Original Data With Found Features
casesCensus2Final <- dataFinal %>% select(colnames(dataFinal))
casesCensus2Final <- casesCensus2Final %>% select_if(is.numeric) %>% 
  scale() %>% as_tibble() %>% add_column(dataFinal$county_name) %>% 
  rename("county" = "dataFinal$county_name") %>% 
  select(county, everything())

# Data Explorer Code
plot_intro(casesCensus2Final, title = "Intro Plot for Finalized Census Dataset")
datatable(casesCensus2Final) %>% formatRound(c(5, 9, 10), 2) %>% formatPercentage(11, 2)
# NOTE: (dataFinal$owner_occupied_housing_units_upper_value_quartile) Has Two NA Values

# Add County Name To Final Data For All Future Map Plots
casesCensus2Final <- casesCensus2Final %>% mutate(county = county %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))




# SUBSET ONE - KMEANS

# Cluster With K-Means
subsetOne <- casesCensusFinal %>% 
  select(county, renter_occupied_housing_units_paying_cash_median_gross_rent, median_age, percent_income_spent_on_rent) 
subsetOne[, 2:length(subsetOne)] %>% scale() %>% as_tibble()
summary(subsetOne)

# Perform K-Mean
subsetOneKM <- kmeans(subsetOne[, 2:length(subsetOne)], centers = 5)
subsetOneKM
pairs(subsetOne[, 2:length(subsetOne)], col = subsetOneKM$cluster + 1L, main = "Subset One Pairs Plot")

# Visualize K-Means Plot
clustersSubsetOneKM <- subsetOne %>% add_column(cluster = factor(subsetOneKM$cluster))
subsetOneCentroids <- as_tibble(subsetOneKM$centers, rownames = "cluster")
fviz_cluster(subsetOneKM, data = subsetOne[, 2:length(subsetOne)], centroids = TRUE, ellipse.type = "norm", 
             geom = "point", main = "Subset One KMeans Dimension Plot")

# Look At Cluster Profiles
ggplot(pivot_longer(subsetOneCentroids, 
                    cols = colnames(subsetOneKM$centers)), 
       aes(x = value, y = name, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) +
  scale_fill_viridis_d() +
  ggtitle("Subset One K-Means Cluster Profiles")

# Visualize Some Data Using Map (K-Means)
subsetOneClustKMTX <- counties_TX %>% left_join(subsetOne %>% add_column(cluster = factor(subsetOneKM$cluster)))
ggplot(subsetOneClustKMTX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d(na.value = "gray50") +
  labs(title = "K-Means Clusters - Subset One Data", subtitle = "Note Greyed Out Counties Are Non-Reporting")

# Gather Information On Which Clusters Are More At Risk
subsetOneClustersKM <- casesCensus %>% add_column(cluster = factor(subsetOneKM$cluster))
subsetOneClustersKM %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))





# SUBSET ONE - HIERARCHICAL CLUSTERING

# Hierarchical Clustering
distSubsetOne <- dist(subsetOne[, 2:length(subsetOne)])
hcSubsetOne <- hclust(distSubsetOne, method = "complete")
fviz_dend(hcSubsetOne, k = 4)
fviz_cluster(list(data = subsetOne[, 2:length(subsetOne)], cluster = cutree(hcSubsetOne, k = 4)), geom = "point") + ggtitle("Subset One Hierarchical Clustering Plot")

# Visualize Single-Link Dendrogram
singleSubsetOne <- hclust(distSubsetOne, method = "single")
fviz_dend(singleSubsetOne, k = 4) + ggtitle("Subset One Hierarchical Clustering Single-Link Dendrogram")

# Visualize Clustering
HClustersSubsetOne <- cutree(hcSubsetOne, k = 4)
completeSubsetOneHC <- subsetOne[, 2:length(subsetOne)] %>%
  add_column(cluster = factor(HClustersSubsetOne))
completeSubsetOneHC
ggplot(completeSubsetOneHC, aes(median_age, renter_occupied_housing_units_paying_cash_median_gross_rent, color = cluster)) + geom_point() + ggtitle("Subset One Hierarchical Clustering Complete-linkage Plot")
ggplot(completeSubsetOneHC, aes(median_age, percent_income_spent_on_rent, color = cluster)) + geom_point() + ggtitle("Subset One Hierarchical Clustering Complete-linkage Plot")


# Visualize Some Data Using Map (HC)
subsetOneHClustTX <- counties_TX %>% left_join(subsetOne %>% add_column(cluster = factor(HClustersSubsetOne)))
ggplot(subsetOneHClustTX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d(na.value = "gray50") +
  labs(title = "Hierarchical Clusters - Subset One Data", subtitle = "Note Greyed Out Counties Are Non-Reporting")





# SUBSET ONE - DBSCAN

# Uses Euclidean Distance
kNNdistplot(subsetOne[, 2:length(subsetOne)], k = 4)
abline(h = 1.1, col = "red")
title("Subset One Elbow Method for DBSCAN")
subsetOneDB <- dbscan(subsetOne[, 2:length(subsetOne)], eps = 1.1, minPts = 5)

# DBSCAN Understanding
subsetOneDB
str(subsetOneDB)

# Visualize DBSCAN Plot
ggplot(subsetOne[, 2:length(subsetOne)] %>% add_column(cluster = factor(subsetOneDB$cluster)),
       aes(median_age, percent_income_spent_on_rent, color = cluster)) + geom_point() + ggtitle("Subset One DBSCAN Clustering Plot")
fviz_cluster(subsetOneDB, subsetOne[, 2:length(subsetOne)], geom = "point") + ggtitle("Subset One DBSCAN Clustering Plot")





# SUBSET ONE - INTERNAL VALIDATION

# Set Seed, Number Clusters For Internal Validation
set.seed(1234)
ks <- 2:10

# K-Means Cluster Statistics
fpc::cluster.stats(distSubsetOne, subsetOneKM$cluster)

# Show Important Indices
sapply(list(
  KM = subsetOneKM$cluster,
  completeHC = cutree(hcSubsetOne, k = 4),
  singleHC = cutree(singleSubsetOne, k = 4)
), FUN = function(x)
  fpc::cluster.stats(distSubsetOne, x))[c("within.cluster.ss", "avg.silwidth"), ]

# Silhouette Plots
plot(silhouette(subsetOneKM$cluster, distSubsetOne))
fviz_silhouette(silhouette(subsetOneKM$cluster, distSubsetOne))

# Find Optimal Number Clusters For K-Means
WCSS <- sapply(ks, FUN = function(k) { kmeans(subsetOne[, 2:length(subsetOne)], centers = k, nstart = 5)$tot.withinss })
ggplot(as_tibble(WCSS), aes(ks, WCSS)) + geom_line() +
  geom_vline(xintercept = 5, color = "red", linetype = 2) + ggtitle("Elbow Method: Optimal Number of Clusters")

# Average Silhouette Width
subsetOneASW <- sapply(ks, FUN = function(k) { fpc::cluster.stats(distSubsetOne, kmeans(subsetOne[, 2:length(subsetOne)], centers = k, nstart = 5)$cluster)$avg.silwidth })
subsetOneBestK <- ks[which.max(subsetOneASW)]
subsetOneBestK
ggplot(as_tibble(subsetOneASW), aes(ks, subsetOneASW)) + geom_line() +
  geom_vline(xintercept = subsetOneBestK, color = "red", linetype = 2) + ggtitle("Average Silhouette Width: Optimate Number of Clusters")

# Dunn Index
subsetOneDI <- sapply(ks, FUN = function(k) { fpc::cluster.stats(distSubsetOne, kmeans(subsetOne[, 2:length(subsetOne)], centers = k, nstart = 5)$cluster)$dunn })
subsetOneBestK <- ks[which.max(subsetOneDI)]
subsetOneBestK
ggplot(as_tibble(subsetOneDI), aes(ks, subsetOneDI)) + geom_line() +
  geom_vline(xintercept = subsetOneBestK, color = "red", linetype = 2) + ggtitle("Dunn Index: Optimal Number of Clusters")

# Use Gap Statistic To Determine Number of Clusters
subsetOneGapStat <- clusGap(subsetOne[, 2:length(subsetOne)], FUN = hcut, K.max = 10, B = 100)
fviz_gap_stat(subsetOneGapStat) + ggtitle("Gap Statistic: Optimal Number of Clusters")

# Visualize Distance Matrix
pimage(distSubsetOne, col = bluered(100))
pimage(distSubsetOne, order = order(subsetOneKM$cluster), col = bluered(100))
dissplot(distSubsetOne, labels = subsetOneKM$cluster, options = list(main = "K-Means; K = 3"))
dissplot(distSubsetOne, labels = kmeans(subsetOne[, 2:length(subsetOne)], centers = 3)$cluster, col = bluered(100))
dissplot(distSubsetOne, labels = kmeans(subsetOne[, 2:length(subsetOne)], centers = 9)$cluster, col = bluered(100))
fviz_dist(distSubsetOne)





# SUBSET ONE - EXTERNAL VALIDATION

# Prepare Ground Truth
subsetOneGT <- casesCensusFinal %>% select(county, death_per_case) %>% arrange(desc(death_per_case))
subsetOneGT$cluster <- factor(case_when(
  subsetOneGT$death_per_case < -0.66 ~ 5,
  -0.66 <= subsetOneGT$death_per_case & subsetOneGT$death_per_case < -0.3 ~ 4,
  -0.3 <= subsetOneGT$death_per_case & subsetOneGT$death_per_case < 0.2 ~ 3,
  0.2 <= subsetOneGT$death_per_case & subsetOneGT$death_per_case < 0.45 ~ 2,
  subsetOneGT$death_per_case >= 0.45 ~ 1
))
subsetOneGTTX <- counties_TX %>% left_join(subsetOneGT)
ggplot(subsetOneGTTX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d(na.value = "gray50") +
  labs(title = "K-Means Clusters - Subset One Ground Truth", subtitle = "Note Greyed Out Counties Are Non-Reporting")

# Call Entropy, Purity Functions
subsetOneEV <- rbind(
  KM = c(
    unlist(fpc::cluster.stats(distSubsetOne, as.numeric(subsetOneKM$cluster), as.numeric(subsetOneGT$cluster), compareonly = TRUE)),
    entropy = entropy(as.numeric(subsetOneKM$cluster), as.numeric(subsetOneGT$cluster)),
    purity = purity(as.numeric(subsetOneKM$cluster), as.numeric(subsetOneGT$cluster))
  ),
  HC = c(
    unlist(fpc::cluster.stats(distSubsetOne, as.numeric(HClustersSubsetOne), as.numeric(subsetOneGT$cluster), compareonly = TRUE)),
    entropy = entropy(as.numeric(HClustersSubsetOne), as.numeric(subsetOneGT$cluster)),
    purity = purity(as.numeric(HClustersSubsetOne), as.numeric(subsetOneGT$cluster))
  )
)
subsetOneEV






# SUBSET TWO - KMEANS

# Cluster With K-Means
subsetTwo <- casesCensusFinal %>% 
  select(county, income_per_capita, owner_occupied_housing_units_upper_value_quartile, gini_index) 
subsetTwo[, 2:length(subsetTwo)] %>% scale() %>% as_tibble()
summary(subsetTwo)

# Take Out NA Values
subsetTwo <- subsetTwo %>% na.omit()

# Perform K-Means
subsetTwoKM <- kmeans(subsetTwo[, 2:length(subsetTwo)], centers = 3)
subsetTwoKM
pairs(subsetTwo[, 2:length(subsetTwo)], col = subsetTwoKM$cluster + 1L, main = "Subset Two Pairs Plot")

# Visualize K-Means Plot
clusterssubsetTwoKM <- subsetTwo %>% add_column(cluster = factor(subsetTwoKM$cluster))
subsetTwoCentroids <- as_tibble(subsetTwoKM$centers, rownames = "cluster")
fviz_cluster(subsetTwoKM, data = subsetTwo[, 2:length(subsetTwo)], centroids = TRUE, ellipse.type = "norm", 
             geom = "point", main = "Subset Two KMeans Dimension Plot")

# Look At Cluster Profiles
ggplot(pivot_longer(subsetTwoCentroids, 
  cols = colnames(subsetTwoKM$centers)), 
  aes(x = value, y = name, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) +
  scale_fill_viridis_d() +
  ggtitle("Subset Two K-Means Cluster Profiles")

# Visualize Some Data Using Map (K-Means)
subsetTwoClustKMTX <- counties_TX %>% left_join(subsetTwo %>% add_column(cluster = factor(subsetTwoKM$cluster)))
ggplot(subsetTwoClustKMTX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d(na.value = "gray50") +
  labs(title = "K-Means Clusters - Subset Two Data", subtitle = "Note Greyed Out Counties Are Non-Reporting")

# Gather Information On Which Clusters Are More At Risk
casesCensusEdit <- subset(casesCensus,!is.na(casesCensus$owner_occupied_housing_units_upper_value_quartile) )
subsetTwoClustersKM <- casesCensusEdit %>% add_column(cluster = factor(subsetTwoKM$cluster))
subsetTwoClustersKM %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))





# SUBSET TWO - HIERARCHICAL CLUSTERING

# Hierarchical Clustering
distsubsetTwo <- dist(subsetTwo[, 2:length(subsetTwo)])
hcsubsetTwo <- hclust(distsubsetTwo, method = "complete")
fviz_dend(hcsubsetTwo, k = 4)
fviz_cluster(list(data = subsetTwo[, 2:length(subsetTwo)], cluster = cutree(hcsubsetTwo, k = 4)), geom = "point") + ggtitle("Subset Two Hierarchical Clustering Plot")

# Visualize Single-Link Dendrogram
singlesubsetTwo <- hclust(distsubsetTwo, method = "single")
fviz_dend(singlesubsetTwo, k = 4) + ggtitle("Subset Two Hierarchical Clustering Single-Link Dendrogram")

# Visualize Clustering
HClusterssubsetTwo <- cutree(hcsubsetTwo, k = 4)
completesubsetTwoHC <- subsetTwo[, 2:length(subsetTwo)] %>%
  add_column(cluster = factor(HClusterssubsetTwo))
completesubsetTwoHC
ggplot(completesubsetTwoHC, aes(gini_index, death_per_case, color = cluster)) + geom_point() + ggtitle("Subset Two Hierarchical Clustering Complete-linkage Plot")
ggplot(completesubsetTwoHC, aes(owner_occupied_housing_units_upper_value_quartile, death_per_case, color = cluster)) + geom_point() + ggtitle("Subset Two Hierarchical Clustering Complete-linkage Plot")


# Visualize Some Data Using Map (HC)
subsetTwoHClustTX <- counties_TX %>% left_join(subsetTwo %>% add_column(cluster = factor(HClusterssubsetTwo)))
ggplot(subsetTwoHClustTX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d(na.value = "gray50") +
  labs(title = "Hierarchical Clusters - Subset Two Data", subtitle = "Note Greyed Out Counties Are Non-Reporting")





# SUBSET TWO - DBSCAN

# Uses Euclidean Distance
kNNdistplot(subsetTwo[, 2:length(subsetTwo)], k = 4)
abline(h = 1.15, col = "red")
title("Subset Two Elbow Method for DBSCAN")
subsetTwoDB <- dbscan(subsetTwo[, 2:length(subsetTwo)], eps = 1.1, minPts = 5)

# DBSCAN Understanding
subsetTwoDB
str(subsetTwoDB)

# Visualize DBSCAN Plot
ggplot(subsetTwo[, 2:length(subsetTwo)] %>% add_column(cluster = factor(subsetTwoDB$cluster)),
       aes(gini_index, death_per_case, color = cluster)) + geom_point() + ggtitle("Subset Two DBSCAN Clustering Plot")
fviz_cluster(subsetTwoDB, subsetTwo[, 2:length(subsetTwo)], geom = "point") + ggtitle("Subset Two DBSCAN Clustering Plot")





# SUBSET TWO - INTERNAL VALIDATION

# Set Seed, Number Clusters For Internal Validation
set.seed(1234)
ks <- 2:10

# K-Means Cluster Statistics
fpc::cluster.stats(distsubsetTwo, subsetTwoKM$cluster)

# Show Important Indices
sapply(list(
  KM = subsetTwoKM$cluster,
  completeHC = cutree(hcsubsetTwo, k = 4),
  singleHC = cutree(singlesubsetTwo, k = 4)
), FUN = function(x)
  fpc::cluster.stats(distsubsetTwo, x))[c("within.cluster.ss", "avg.silwidth"), ]

# Silhouette Plots
plot(silhouette(subsetTwoKM$cluster, distsubsetTwo))
fviz_silhouette(silhouette(subsetTwoKM$cluster, distsubsetTwo))

# Find Optimal Number Clusters For K-Means
WCSS <- sapply(ks, FUN = function(k) { kmeans(subsetTwo[, 2:length(subsetTwo)], centers = k, nstart = 5)$tot.withinss })
ggplot(as_tibble(WCSS), aes(ks, WCSS)) + geom_line() +
  geom_vline(xintercept = 4, color = "red", linetype = 2) + ggtitle("Elbow Method: Optimal Number of Clusters")

# Average Silhouette Width
subsetTwoASW <- sapply(ks, FUN = function(k) { fpc::cluster.stats(distsubsetTwo, kmeans(subsetTwo[, 2:length(subsetTwo)], centers = k, nstart = 5)$cluster)$avg.silwidth })
subsetTwoBestK <- ks[which.max(subsetTwoASW)]
subsetTwoBestK
ggplot(as_tibble(subsetTwoASW), aes(ks, subsetTwoASW)) + geom_line() +
  geom_vline(xintercept = subsetTwoBestK, color = "red", linetype = 2) + ggtitle("Average Silhouette Width: Optimate Number of Clusters")

# Dunn Index
subsetTwoDI <- sapply(ks, FUN = function(k) { fpc::cluster.stats(distsubsetTwo, kmeans(subsetTwo[, 2:length(subsetTwo)], centers = k, nstart = 5)$cluster)$dunn })
subsetTwoBestK <- ks[which.max(subsetTwoDI)]
subsetTwoBestK
ggplot(as_tibble(subsetTwoDI), aes(ks, subsetTwoDI)) + geom_line() +
  geom_vline(xintercept = subsetTwoBestK, color = "red", linetype = 2) + ggtitle("Dunn Index: Optimal Number of Clusters")

# Use Gap Statistic To Determine Number of Clusters
subsetTwoGapStat <- clusGap(subsetTwo[, 2:length(subsetTwo)], FUN = hcut, K.max = 10, B = 100)
fviz_gap_stat(subsetTwoGapStat) + ggtitle("Gap Statistic: Optimal Number of Clusters")

# Visualize Distance Matrix
pimage(distsubsetTwo, col = bluered(100))
pimage(distsubsetTwo, order = order(subsetTwoKM$cluster), col = bluered(100))
dissplot(distsubsetTwo, labels = subsetTwoKM$cluster, options = list(main = "K-Means; K = 3"))
dissplot(distsubsetTwo, labels = kmeans(subsetTwo[, 2:length(subsetTwo)], centers = 3)$cluster, col = bluered(100))
dissplot(distsubsetTwo, labels = kmeans(subsetTwo[, 2:length(subsetTwo)], centers = 9)$cluster, col = bluered(100))
fviz_dist(distsubsetTwo)





# SUBSET TWO - EXTERNAL VALIDATION

# Prepare Ground Truth
subsetTwoGT <- casesCensusFinal %>% select(county, death_per_case) %>% arrange(desc(death_per_case))

subsetTwoGT <- subsetTwoGT[!(subsetTwoGT$county %in% c("kenedy","king")),]

subsetTwoGT$cluster <- factor(case_when(
  subsetTwoGT$death_per_case < -0.3 ~ 3,
  -0.3 <= subsetTwoGT$death_per_case & subsetTwoGT$death_per_case < 1.20 ~ 1,
  subsetTwoGT$death_per_case >= 1.20 ~ 2
))
subsetTwoGTTX <- counties_TX %>% left_join(subsetTwoGT)
ggplot(subsetTwoGTTX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d(na.value = "gray50") +
  labs(title = "K-Means Clusters - Subset Two Ground Truth", subtitle = "Note Greyed Out Counties Are Non-Reporting")

# Call Entropy, Purity Functions
subsetTwoEV <- rbind(
  KM = c(
    unlist(fpc::cluster.stats(distsubsetTwo, as.numeric(subsetTwoKM$cluster), as.numeric(subsetTwoGT$cluster), compareonly = TRUE)),
    entropy = entropy(as.numeric(subsetTwoKM$cluster), as.numeric(subsetTwoGT$cluster)),
    purity = purity(as.numeric(subsetTwoKM$cluster), as.numeric(subsetTwoGT$cluster))
  ),
  HC = c(
    unlist(fpc::cluster.stats(distsubsetTwo, as.numeric(HClusterssubsetTwo), as.numeric(subsetTwoGT$cluster), compareonly = TRUE)),
    entropy = entropy(as.numeric(HClusterssubsetTwo), as.numeric(subsetTwoGT$cluster)),
    purity = purity(as.numeric(HClusterssubsetTwo), as.numeric(subsetTwoGT$cluster))
  )
)
subsetTwoEV






# COMPARE RESPONSE TO VACCINE 
subsetOneClustersKM %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))

vaccineInfo <- vaccineInfo %>% mutate(us_county = us_county %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
vaccineInfo <- vaccineInfo %>% rename("county" = "us_county") 
vaccineInfoSubsetOne <- vaccineInfo %>% left_join( clustersSubsetOneKM, 
                             by=c('county'))

vaccineInfoCluster1 <- filter(vaccineInfoSubsetOne, cluster==1)
summary(vaccineInfoCluster1$sites_per_1k_ppl) # mean -0.21
vaccineInfoCluster2 <- filter(vaccineInfoSubsetOne, cluster==2)
summary(vaccineInfoCluster2$sites_per_1k_ppl) # mean -0.18
vaccineInfoCluster3 <- filter(vaccineInfoSubsetOne, cluster==3)
summary(vaccineInfoCluster3$sites_per_1k_ppl) # mean 0.51
vaccineInfoCluster4 <- filter(vaccineInfoSubsetOne, cluster==4)
summary(vaccineInfoCluster4$sites_per_1k_ppl) # mean 0.19
vaccineInfoCluster5 <- filter(vaccineInfoSubsetOne, cluster==5)
summary(vaccineInfoCluster5$sites_per_1k_ppl) # mean -0.22

# Simple Horizontal Bar Plot with Added Labels
counts <- table(vaccineInfoSubsetOne$cluster)
barplot(counts, main="Subset One Cluster Distribution", col="turquoise", horiz=TRUE,
        xlim = c(0, 60), axisnames = TRUE, 
        xlab = "Number of Counties", ylab = "Cluster Number",
        names.arg=c("1", "2", "3", "4", "5"))

# Box Plots for Vaccine Information
# Visualize how each cluster performed in terms of vaccine sites
boxplot(sites_per_1k_ppl~cluster,
        data=vaccineInfoSubsetOne,
        main="Subset One Clusters - Vaccine Sites Box Plots",
        xlab="Cluster Number",
        ylab="Normalized Vaccine Sites per 1k People",
        col="turquoise",
        border="black"
)
means <- tapply(vaccineInfoSubsetOne$sites_per_1k_ppl,vaccineInfoSubsetOne$cluster,mean)
points(means,col="red",pch=18)



# subset two
subsetTwoClustersKM %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))

vaccineInfoSubsetTwo <- vaccineInfo %>% left_join( clusterssubsetTwoKM, 
                                          by=c('county'))

vaccineInfo2Cluster1 <- filter(vaccineInfoSubsetTwo, cluster==1)
summary(vaccineInfo2Cluster1$sites_per_1k_ppl) # mean -0.21
vaccineInfo2Cluster2 <- filter(vaccineInfoSubsetTwo, cluster==2)
summary(vaccineInfo2Cluster2$sites_per_1k_ppl) # mean 0.40
vaccineInfo2Cluster3 <- filter(vaccineInfoSubsetTwo, cluster==3)
summary(vaccineInfo2Cluster3$sites_per_1k_ppl) # mean 0.02

# Subset two's cluster 2 did the best in providing vaccine sites
# cluster 1 did the worst. 




# # PREVIOUS CODE
# 
# 
# 
# # Vaccine Information Clustering
# 
# # Note Data Already Sorted In Ascending Order
# vaccineTX <- vaccineInfo %>% select(us_county, num_vaccine_sites, total_population, sites_per_1k_ppl)
# summary(vaccineTX)
# pairs(vaccineTX)
# 
# # Visualize Some Data Using Map (Ground Truth)
# datatable(vaccineTX)
# vaccineTX <- vaccineTX %>% mutate(county = us_county %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
# vaccineClustTX <- counties_TX %>% left_join(vaccineTX)
# ggplot(vaccineClustTX, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = num_vaccine_sites)) +
#   coord_quickmap() +
#   scale_fill_continuous(type = "viridis") +
#   labs(title = "Counties By Vaccine Sites Per 1K People", subtitle = "Note Greyed Out Counties Are Non-Reporting")
# 
# # Take Numeric Data, Scale
# scaledVaccineTX <- vaccineTX %>% 
#   select(total_population, sites_per_1k_ppl) %>% 
#   scale() %>% as_tibble()
# 
# # Perform K-Means
# vaccineKM <- kmeans(scaledVaccineTX, centers = 3, nstart = 10)
# vaccineKM
# pairs(scaledVaccineTX, col = vaccineKM$cluster + 1L)
# 
# # Visualize Singular Plots (Cluster)
# clustersVaccineKM <- scaledVaccineTX %>% add_column(cluster = factor(vaccineKM$cluster))
# vaccineCentroids <- as_tibble(vaccineKM$centers, rownames = "cluster")
# fviz_cluster(vaccineKM, data = scaledVaccineTX, choose.vars = c("num_vaccine_sites", "total_population"), 
#              centroids = TRUE, ellipse.type = "norm", geom = "point", main = "Single Pair Plot; Total Population Vs. Number Vaccine Sites")
# 
# # Look At Cluster Profiles
# ggplot(pivot_longer(vaccineCentroids, 
#   cols = colnames(vaccineKM$centers)), 
#   aes(x = value, y = name, fill = cluster)) +
#   geom_bar(stat = "identity") +
#   facet_grid(rows = vars(cluster)) +
#   scale_fill_viridis_d() 
# 
# # Look At First Cluster
# vaccineC1 <- clustersVaccineKM %>% filter(cluster == 1)
# summary(vaccineC1)
# ggplot(vaccineC1, aes(num_vaccine_sites, total_population)) + geom_point()
# 
# # Make County Name Match Map County Names
# vaccineClustKMTX <- counties_TX %>% left_join(vaccineTX %>% add_column(cluster = factor(vaccineKM$cluster)))
# ggplot(vaccineClustKMTX, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = cluster)) +
#   coord_quickmap() + 
#   scale_fill_viridis_d(na.value = "gray50") +
#   labs(title = "Clusters - Vaccine Site Data", subtitle = "Note Greyed Out Counties Are Non-Reporting")
# 
# # Hierarchical Clustering
# distVaccine <- dist(scaledVaccineTX)
# hcVaccine <- hclust(distVaccine, method = "complete")
# fviz_dend(hcVaccine, k = 3)
# fviz_cluster(list(data = scaledVaccineTX, cluster = cutree(hcVaccine, k = 3)), choose.vars = c("num_vaccine_sites", "total_population"), geom = "point")
# 
# # Visualize Single-Link Dendrogram
# singleVaccine <- hclust(distVaccine, method = "single")
# fviz_dend(singleVaccine, k = 3)
# 
# # Visualize Clustering
# clustersVaccineH <- cutree(hcVaccine, k = 3)
# completeVaccineH <- scaledVaccineTX %>%
#   add_column(cluster = factor(clustersVaccineH))
# completeVaccineH
# ggplot(completeVaccineH, aes(num_vaccine_sites, total_population, color = cluster)) + geom_point()
# 
# # Make County Name Match Map County Names
# vaccineClustHTX <- counties_TX %>% left_join(vaccineTX %>% add_column(cluster = factor(clustersVaccineH)))
# ggplot(vaccineClustHTX, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = cluster)) +
#   coord_quickmap() + 
#   scale_fill_viridis_d(na.value = "gray50") +
#   labs(title = "Clusters - Vaccine Site Data", subtitle = "Note Greyed Out Counties Are Non-Reporting")
# 
# 
# 
# # Housing Clustering
# 
# # Sort Data In Descending Order (Amount Housing Units)
# housingTX <- casesCensus %>% arrange(desc(housing_units)) %>%    
#   select(county_name, confirmed_cases, housing_units, housing_built_2005_or_later, housing_built_2000_to_2004, housing_built_1939_or_earlier)
# summary(housingTX)
# pairs(housingTX)
# 
# # Visualize Some Data Using Map
# datatable(housingTX)
# housingTX <- housingTX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
# housingClustTX <- counties_TX %>% left_join(housingTX)
# ggplot(housingClustTX, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = confirmed_cases)) +
#   coord_quickmap() +
#   scale_fill_continuous(type = "viridis") +
#   labs(title = "Counties By Number Housing Units")
# 
# 
# groundTruth <- within(groundTruth, {   
#   Income.cat <- NA # need to initialize variable
#   Income.cat[Income < 4000] <- "Low"
#   Income.cat[Income >= 4000 & Income < 5000] <- "Middle"
#   Income.cat[Income >= 5000] <- "High"
# } )
# 
# # Take Numeric Data, Scale
# scaledHousingTX <- housingTX %>% 
#   select(housing_built_1939_or_earlier, housing_built_2000_to_2004, housing_built_2005_or_later, housing_units) %>% 
#   scale() %>% as_tibble()
# 
# # Perform K-Means
# housingKM <- kmeans(scaledHousingTX, centers = 5, nstart = 10)
# housingKM
# pairs(scaledHousingTX, col = housingKM$cluster + 1L)
# 
# # Visualize Singular Plots (Cluster)
# clustersHousingKM <- scaledHousingTX %>% add_column(cluster = factor(housingKM$cluster))
# housingCentroids <- as_tibble(housingKM$centers, rownames = "cluster")
# fviz_cluster(housingKM, data = scaledHousingTX, choose.vars = c("housing_units", "housing_built_2000_to_2004"), 
#              centroids = TRUE, ellipse.type = "norm", geom = "point", main = "Single Pair Plot; Number Housing Units Vs. Housing Built: 2000 - 2004")
# 
# # Look At Cluster Profiles
# ggplot(pivot_longer(housingCentroids, 
#   cols = colnames(housingKM$centers)), 
#   aes(x = value, y = name, fill = cluster)) +
#   geom_bar(stat = "identity") +
#   facet_grid(rows = vars(cluster)) +
#   scale_fill_viridis_d() 
# 
# # Look At First Cluster
# housingC1 <- clustersHousingKM %>% filter(cluster == 1)
# summary(housingC1)
# ggplot(housingC1, aes(housing_units, housing_built_2000_to_2004)) + geom_point()
# 
# # Make County Name Match Map County Names
# housingClustKMTX <- counties_TX %>% left_join(housingTX %>% add_column(cluster = factor(housingKM$cluster)))
# ggplot(housingClustKMTX, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = cluster)) +
#   coord_quickmap() + 
#   scale_fill_viridis_d(na.value = "gray50") +
#   labs(title = "Clusters - Housing Data")
# 
# 
# 
# # Education Level (??? Clustering)
# 
# educationLeveLTX <- COVID_19_cases_plus_census %>% filter(state == "TX")
# educationLeveLTX <- educationLeveLTX %>% 
#   filter(confirmed_cases > 100) %>% 
#   arrange(desc(confirmed_cases)) %>%    
#   select(county_name, total_pop, some_college_and_associates_degree, associates_degree, bachelors_degree, high_school_diploma, masters_degree, less_than_high_school_graduate)
# pairs(educationLeveLTX[2:7])
# 
# # Cluster cases_TX With K-means for years that housing was built
# scaledEducationLevelTX <- educationLeveLTX %>% 
#   select(
#     total_pop, 
#     some_college_and_associates_degree, 
#     associates_degree, 
#     bachelors_degree, 
#     high_school_diploma, 
#     masters_degree, 
#     less_than_high_school_graduate
#   ) %>% 
#   scale() %>% as_tibble()
# summary(scaledEducationLevelTX)
# 
# # Perform k-means
# km <- kmeans(scaledEducationLevelTX, centers = 4)
# km
# pairs(scaledEducationLevelTX, col = km$cluster + 1L)
# 
# # Make County Name Match Map County Names
# educationLeveLTX <- educationLeveLTX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
# counties_TX_clust <- counties_TX %>% left_join(educationLeveLTX %>% add_column(cluster = factor(km$cluster)))
# ggplot(counties_TX_clust, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = cluster)) +
#   coord_quickmap() + 
#   scale_fill_viridis_d() + 
#   labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")
# 
# # Hierarchical Clustering
# d <- dist(scaledEducationLevelTX)
# hc <- hclust(d, method = "complete")
# plot(hc)
# fviz_dend(hc, k = 3)
# 
# clusters <- cutree(hc, k = 3)
# cluster_complete <- scaledEducationLevelTX %>%
#   add_column(cluster = factor(clusters))
# cluster_complete
# 
# fviz_cluster(list(data = scaledEducationLevelTX, cluster = cutree(hc, k = 3)), geom = "point", choose.vars = c("bachelors_degree", "total_pop"))
# 
# # Make County Name Match Map County Names
# educationLeveLTX <- educationLeveLTX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
# counties_TX_clust <- counties_TX %>% left_join(educationLeveLTX %>% add_column(cluster = factor(cluster_complete$cluster)))
# ggplot(counties_TX_clust, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = cluster)) +
#   coord_quickmap() + 
#   scale_fill_viridis_d() + 
#   labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")
# 
# # DBSCAN
# kNNdistplot(scaledEducationLevelTX, k = 3)
# abline(h = 1, col = "red")
# # Uses Euclidean Distance
# db <- dbscan(scaledEducationLevelTX, eps = 1, minPts = 5)
# db
# str(db)
# ggplot(scaledEducationLevelTX %>% add_column(cluster = factor(db$cluster)),
#        aes(bachelors_degree, total_pop, color = cluster)) + geom_point()
# fviz_cluster(db, scaledEducationLevelTX, choose.vars = c("bachelors_degree", "total_pop"), geom = "point")
# 
# 
# 
# # Commute (??? Clustering)
# 
# commuteTX <- COVID_19_cases_plus_census %>% filter(state == "TX")
# commuteTX <- commuteTX %>% 
#   filter(confirmed_cases > 100) %>% 
#   arrange(desc(confirmed_cases)) %>%    
#   select(county_name, 
#          total_pop, 
#          commute_5_9_mins,
#          commute_10_14_mins, 
#          commute_15_19_mins, 
#          commute_20_24_mins, 
#          commute_25_29_mins,
#          commute_30_34_mins,
#          commute_35_39_mins,
#          commute_40_44_mins,
#          commute_45_59_mins,
#          commute_60_89_mins,
#          commute_90_more_mins)
# pairs(commuteTX[2:6])
# 
# # Cluster cases_TX With K-means for commute
# scaledCommuteTX <- commuteTX %>% 
#   select(
#     total_pop, 
#     commute_5_9_mins,
#     commute_10_14_mins, 
#     commute_15_19_mins, 
#     commute_20_24_mins, 
#     commute_25_29_mins,
#     commute_30_34_mins,
#     commute_35_39_mins,
#     commute_40_44_mins,
#     commute_45_59_mins,
#     commute_60_89_mins,
#     commute_90_more_mins
#   ) %>% 
#   scale() %>% as_tibble()
# summary(scaledCommuteTX)
# 
# # Perform k-means
# km <- kmeans(scaledCommuteTX, centers = 5)
# km
# pairs(scaledCommuteTX, col = km$cluster + 1L)
# 
# # Make County Name Match Map County Names
# commuteTX <- commuteTX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
# counties_TX_clust <- counties_TX %>% left_join(commuteTX %>% add_column(cluster = factor(km$cluster)))
# ggplot(counties_TX_clust, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = cluster)) +
#   coord_quickmap() + 
#   scale_fill_viridis_d() + 
#   labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")
# 
# # Hierarchical Clustering
# d <- dist(scaledCommuteTX)
# hc <- hclust(d, method = "complete")
# plot(hc)
# fviz_dend(hc, k = 5)
# 
# clusters <- cutree(hc, k = 5)
# cluster_complete <- scaledCommuteTX %>%
#   add_column(cluster = factor(clusters))
# cluster_complete
# 
# fviz_cluster(list(data = scaledCommuteTX, cluster = cutree(hc, k = 5)), geom = "point", choose.vars = c("commute_15_19_mins", "total_pop"))
# 
# # Make County Name Match Map County Names
# commuteTX <- commuteTX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
# counties_TX_clust <- counties_TX %>% left_join(commuteTX %>% add_column(cluster = factor(cluster_complete$cluster)))
# ggplot(counties_TX_clust, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = cluster)) +
#   coord_quickmap() + 
#   scale_fill_viridis_d() + 
#   labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")
# 
# # DBSCAN
# kNNdistplot(scaledCommuteTX, k = 3)
# abline(h = 1, col = "red")
# # Uses Euclidean Distance
# db <- dbscan(scaledCommuteTX, eps = 1, minPts = 5)
# db
# str(db)
# ggplot(scaledCommuteTX %>% add_column(cluster = factor(db$cluster)),
#        aes(commute_15_19_mins, total_pop, color = cluster)) + geom_point()
# fviz_cluster(db, scaledCommuteTX, choose.vars = c("commute_15_19_mins", "total_pop"), geom = "point")
# 
# 
# ### Ethnicities Clustering
# ethnicitiesTX <- casesCensus %>% filter(state == "TX")
# ethnicitiesTX <- ethnicitiesTX %>% 
#   filter(confirmed_cases > 100) %>% 
#   arrange(desc(confirmed_cases)) %>%    
#   select(county_name, 
#          white_pop,
#          black_pop,
#          asian_pop,
#          amerindian_pop,
#          hispanic_pop,
#          other_race_pop)
# pairs(ethnicitiesTX[2:7])
# # Cluster cases_TX With K-means for ethnicities
# scaledEthnicitiesTX <- ethnicitiesTX %>% 
#   select(
#     white_pop,
#     black_pop,
#     asian_pop,
#     amerindian_pop,
#     hispanic_pop,
#     other_race_pop
#   ) %>% 
#   scale() %>% as_tibble()
# summary(scaledEthnicitiesTX)
# 
# # Perform k-means
# km <- kmeans(scaledEthnicitiesTX, centers = 6)
# km
# pairs(scaledEthnicitiesTX, col = km$cluster + 1L)
# 
# # Make County Name Match Map County Names
# ethnicitiesTX <- ethnicitiesTX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
# counties_TX_clust <- counties_TX %>% left_join(ethnicitiesTX %>% add_column(cluster = factor(km$cluster)))
# ggplot(counties_TX_clust, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = cluster)) +
#   coord_quickmap() + 
#   scale_fill_viridis_d() + 
#   labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")
# 
# # Hierarchical Clustering
# d <- dist(scaledEthnicitiesTX)
# hc <- hclust(d, method = "complete")
# plot(hc)
# fviz_dend(hc, k = 5)
# 
# clusters <- cutree(hc, k = 5)
# cluster_complete <- scaledEthnicitiesTX %>%
#   add_column(cluster = factor(clusters))
# cluster_complete
# 
# fviz_cluster(list(data = scaledEthnicitiesTX, cluster = cutree(hc, k = 5)), geom = "point", choose.vars = c("hispanic_pop", "white_pop"))
# 
# # Make County Name Match Map County Names
# ethnicitiesTX <- ethnicitiesTX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
# counties_TX_clust <- counties_TX %>% left_join(ethnicitiesTX %>% add_column(cluster = factor(cluster_complete$cluster)))
# ggplot(counties_TX_clust, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = cluster)) +
#   coord_quickmap() + 
#   scale_fill_viridis_d() + 
#   labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")
# 
# # DBSCAN
# kNNdistplot(scaledEthnicitiesTX, k = 3)
# abline(h = 1, col = "red")
# # Uses Euclidean Distance
# db <- dbscan(scaledEthnicitiesTX, eps = 1, minPts = 5)
# db
# str(db)
# ggplot(scaledEthnicitiesTX %>% add_column(cluster = factor(db$cluster)),
#        aes(white_pop, hispanic_pop, color = cluster)) + geom_point()
# fviz_cluster(db, scaledCommuteTX, choose.vars = c("white_pop", "hispanic_pop"), geom = "point")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Project Two: Cluster Analysis
# 
# # Make Character Factors, Filter TX
# cases <- COVID_19_cases_plus_census %>% mutate_if(is.character, factor)
# cases_TX <- COVID_19_cases_plus_census %>% filter(state == "TX")
# 
# # Calculate Rates, Select Important Variables
# cases_TX <- cases_TX %>% 
#   filter(confirmed_cases > 100) %>% 
#   arrange(desc(confirmed_cases)) %>%    
#   select(county_name, confirmed_cases, deaths, total_pop, median_income, median_age, poverty, commuters_by_public_transportation)
# cases_TX <- cases_TX %>% mutate(
#   cases_per_1000 = confirmed_cases/total_pop*1000, 
#   deaths_per_1000 = deaths/total_pop*1000, 
#   death_per_case = deaths/confirmed_cases)
# summary(cases_TX)
# pairs(cases_TX[2:11])
# 
# # Visualize Some Data Using Map
# datatable(cases_TX) %>% formatRound(c(5, 9, 10), 2) %>% formatPercentage(11, 2)
# counties <- as_tibble(map_data("county"))
# counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
#   rename("county" = "subregion")
# 
# # Make County Name Match Map County Names
# cases_TX <- cases_TX %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
# counties_TX_clust <- counties_TX %>% left_join(cases_TX)
# ggplot(counties_TX_clust, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = deaths_per_1000)) +
#   coord_quickmap() +
#   scale_fill_continuous(type = "viridis") +
#   labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")
# 
# # Cluster cases_TX With k-means
# cases_TX_scaled <- cases_TX %>% 
#   select(
#     median_income,
#     median_age, 
#     # total_pop, # you should use density
#     deaths_per_1000,
#     death_per_case,
#     poverty, 
#     commuters_by_public_transportation
#   ) %>% 
#   scale() %>% as_tibble()
# summary(cases_TX_scaled)
# 
# # Perform k-means
# km <- kmeans(cases_TX_scaled, centers = 3)
# km
# pairs(cases_TX_scaled, col = km$cluster + 1L)
# 
# # Look At Cluster Profiles
# ggplot(pivot_longer(as_tibble(km$centers, rownames = "cluster"), 
#   cols = colnames(km$centers)), 
#   aes(y = name, x = value)) +
#   geom_bar(stat = "identity") +
#   facet_grid(rows = vars(cluster))
# 
# # Make County Name Match Map County Names
# counties_TX_clust <- counties_TX %>% left_join(cases_TX %>% add_column(cluster = factor(km$cluster)))
# ggplot(counties_TX_clust, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = cluster)) +
#   coord_quickmap() + 
#   scale_fill_viridis_d() + 
#   labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")
# 
# # Note: Think About Outliers, Appropriate #Clusters, What Clusters Mean For Decision Maker
# # Check If Cases / Deaths By 1000 People Are Different In Different Clusters:
# cases_TX_km <- cases_TX %>% add_column(cluster = factor(km$cluster))
# cases_TX_km %>% group_by(cluster) %>% summarize(
#   avg_cases = mean(cases_per_1000), 
#   avg_deaths = mean(deaths_per_1000))
# 
# # KNN Distance Plot
# kNNdist(cases_TX_scaled, k = 4)
# kNNdist(cases_TX_scaled, k = 4)
# kNNdistplot(cases_TX_scaled, k = 4)
# 
# # Uses Euclidean Distance
# db <- dbscan(cases_TX_scaled, eps = 1.0, minPts = 5)
# db
# pairs(cases_TX_scaled, col = db$cluster + 1L)
# 
# data(iris)
# iris <- as.matrix(iris[, 1:4])
# kNNdist(iris, k = 4)
# kNNdist(iris, k = 4, all = TRUE)
# kNNdistplot(iris, k = 4)
# cl <- dbscan(iris, eps = .7, minPts = 5)
# pairs(iris, col = cl$cluster + 1L)
# 
# # Look At Cluster Profiles
# ggplot(pivot_longer(as_tibble(km$centers, rownames = "cluster"), 
#   cols = colnames(km$centers)), 
#   aes(y = name, x = value)) +
#   geom_bar(stat = "identity") +
#   facet_grid(rows = vars(cluster))
# 
# opt <- optics(cases_TX_scaled, eps = 1, minPts = 4)
# opt
# opt <- extractDBSCAN(opt, eps_cl = 0.4)
# plot(opt)
# 
# hdb <- hdbscan(cases_TX_scaled, minPts = 4)
# hdb
# plot(hdb, show_flat = TRUE)
# 
# 
# 
# 
# # Visualize Some Data Using Map
# datatable(no_outliers) %>% formatRound(c(5, 9, 10), 2) %>% formatPercentage(11, 2)
# 
# # Cluster cases_TX With k-means
# no_outliers <- no_outliers %>% 
#   select(
#     median_income,
#     median_age, 
#     income_per_capita
#   ) %>% 
#   scale() %>% as_tibble()
# summary(no_outliers)
# 
# # Perform k-means
# km <- kmeans(no_outliers, centers = 3)
# km
# pairs(no_outliers, col = km$cluster + 1L)
# 
# # Look At Cluster Profiles
# ggplot(pivot_longer(as_tibble(km$centers, rownames = "cluster"), 
#                     cols = colnames(km$centers)), 
#        aes(y = name, x = value)) +
#   geom_bar(stat = "identity") +
#   facet_grid(rows = vars(cluster))
# 
# # Make County Name Match Map County Names
# counties_TX_clust <- counties_TX %>% left_join(cases_TX %>% add_column(cluster = factor(km$cluster)))
# ggplot(counties_TX_clust, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = cluster)) +
#   coord_quickmap() + 
#   scale_fill_viridis_d() + 
#   labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")
# 
# # Note: Think About Outliers, Appropriate #Clusters, What Clusters Mean For Decision Maker
# # Check If Cases / Deaths By 1000 People Are Different In Different Clusters:
# cases_TX_km <- cases_TX %>% add_column(cluster = factor(km$cluster))
# cases_TX_km %>% group_by(cluster) %>% summarize(
#   avg_cases = mean(cases_per_1000), 
#   avg_deaths = mean(deaths_per_1000))
# 
# # KNN Distance Plot
# kNNdist(no_outliers, k = 4)
# kNNdist(no_outliers, k = 4)
# kNNdistplot(no_outliers, k = 4)
# 
# # Uses Euclidean Distance
# db <- dbscan(no_outliers, eps = 1.0, minPts = 5)
# db
# pairs(no_outliers, col = db$cluster + 1L)
# 
# # Look At Cluster Profiles
# ggplot(pivot_longer(as_tibble(km$centers, rownames = "cluster"), 
#                     cols = colnames(km$centers)), 
#        aes(y = name, x = value)) +
#   geom_bar(stat = "identity") +
#   facet_grid(rows = vars(cluster))
# 
# opt <- optics(no_outliers, eps = 1, minPts = 4)
# opt
# opt <- extractDBSCAN(opt, eps_cl = 0.4)
# plot(opt)
# 
# hdb <- hdbscan(no_outliers, minPts = 4)
# hdb
# plot(hdb, show_flat = TRUE)
# 
