### simulation permanova - adonis ###
rm(list=ls())
library(vegan)
library(tidyr)
library(dplyr)

# Simulate a dataset
set.seed(123)
n_samples <- 10  # number of samples per location
n_locations <- 4
n_treatments <- 2

# Create a distance matrix based on simulated data
data_matrix <- matrix(rnorm(n_samples * n_locations * n_treatments), ncol = n_locations)
dist_matrix <- vegdist(data_matrix)

# PERMANOVA
#permanova_result <- adonis(dist_matrix ~ factor(rep(1:n_treatments, each = n_samples)), permutations = 999)
#print(permanova_result)
#adonis is deprecated - use adonis2 instead
adonis2(dist_matrix ~ factor(rep(1:n_treatments, each = n_samples)), permutations = 999)

###############################
# using my data
df = read.csv("./data/raw/test_permanova.csv", stringsAsFactors = T)

wide_df <- df %>%
  pivot_wider(names_from = c(treatment, location), 
              values_from = maxn, 
              values_fill = list(maxn = 0))

# Remove the 'sample' column to only keep the abundance data
abundance_matrix <- as.matrix(wide_df[-1])

# Calculate the Euclidean distance matrix
distance_matrix <- vegdist(abundance_matrix, method = "euclidean")

# Print the distance matrix
print(distance_matrix)

#prepare grouping factors
grouping_factors <- df %>%
  distinct(sample, treatment, location)

# Run PERMANOVA
permanova_result <- adonis2(distance_matrix ~ treatment + location, 
                           data = grouping_factors, permutations = 9999)

# Print the results
print(permanova_result)

## doesn't seem to be able to handle random effect for location here - better to use LMER?
## chatgpt recommends using alternative approach with lme4 package to ahdnle random effects