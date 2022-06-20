# TASK 3: K-MEANS CLUSTERING

# DATA IMPORT AND PREP
#install.packages("fastDummies")
library(tidyverse)
library(skimr)
library(fastDummies)
library(factoextra)

datatype <- c('iiiiiiiiiiiiiiiiiffffffffffffffiff')
frs_housing_data <- read_csv('D:/ASDM_Coursework/Datasets/frs_housing_data.csv', col_types = datatype)

frs_clust <- frs_housing_data %>% select(ADULTH, BEDROOM6, ROOMS10, DEPCHLDH, DISCHHA1, DISCHHC1,
                                        HHINC, HEARNS, HSEINC, HHRPINC, HHINV, HBENINC,
                                        HHDISBEN, TUHHRENT, HHRENT, YEARNO, RENTYP, YEARBND)

# NOTE: CLUSTERING can only be applied to numeric values. Hence, all the factor variables
# were dropped except RENTYP and YEARBND. RENTYP and YEARBND were retained so that the created
# clusters could be interpreted based on rent type (RENTYP) and duration of tenancy (YEARBND).

# These two factor variables were converted into numeric values by creating their dummies
# thereafter the original factor variables were dropped.

frs_clust <- dummy_cols(frs_clust)
colnames(frs_clust)

frs_clust <- frs_clust %>% select (-c(17,18)) 
colnames(frs_clust)

head(frs_clust)
glimpse(frs_clust)
skim(frs_clust)
# A total of 20 integer variables are selected for clustering

# K-MEANS CLUSTERING

# Normalization of the 'frs_clust' dataset

# creating a normalization function called 'normalize'.
normalize <- function (x){nf =(x-min(x))/ (max(x)- min(x)); return(nf)}
frs_clust_n <- as.data.frame(lapply(frs_clust, normalize))
head(frs_clust_n)

# Determining the number of clusters to select (k)
fviz_nbclust(frs_clust_n, kmeans, method = 'wss')
fviz_nbclust(frs_clust_n, kmeans, method = 'silhouette')

# Accessing Clustering tendency
get_clust_tendency(frs_clust_n, n=4)
# Hopkins stat showed that the dataset had a high clust
set.seed(102)
frs_kmeans <- kmeans(frs_clust_n, 4)



frs_kmeans
frs_kmeans$cluster
frs_kmeans$centers
frs_kmeans$betweenss
frs_kmeans$tot.withinss
frs_kmeans$size

summary(frs_kmeans)
library(cluster)
clusplot(frs_clust_n, frs_kmeans$cluster, color = TRUE, shade = TRUE, lines = 0)

plot(HBENINC~HHRENT, frs_clust, col= frs_kmeans$cluster)

# INTERPRETATION OF CLUSTERS
frs_clust_means <- aggregate(frs_clust, list(frs_kmeans$cluster), mean)

# Creating a function to round values to 1dp.
approxi <- function(x){y =round(x, 0); return(y)}

apply(frs_clust_means, 2, approxi)

# PREPING AND SAVING DATA FOR SAS IMPLEMENTATION
frs_clust_SAS <- frs_clust
frs_clust_SAS['id'] <- rownames(frs_clust_SAS)
frs_clust_SAS

write_csv(frs_clust_SAS, 'D:/ASDM_coursework/Datasets/frs_clust_SAS.csv')
