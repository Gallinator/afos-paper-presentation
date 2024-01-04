library(palmerpenguins)
library(fsdaR)
library(GGally)
library(corrplot)

# Preprocessing, keep only continuous features
data.full <- penguins[c('species',
             'bill_length_mm',
             'bill_depth_mm',
             'flipper_length_mm',
             'body_mass_g')]
data.full <- na.omit(data.full)
data.numeric <- data.full[, 2:5]

# Data inspection
summary(data.numeric)
cov(data.numeric)

# High correlation between body_mass_g and flipper_length_mm
corrplot(cor(data.numeric), method = 'color')

# PCA, keep only 3 components which explain about 97% of variance
data.pca <- prcomp(data.numeric, scale = TRUE)
summary(data.pca)
data.pca <- data.pca$x[, 1:3]

ggpairs(as.data.frame(data.pca), aes(color = data.full$species), upper = list(continuous = 'points'))

# K selection algorithms, best solution has 3 components which coincide with real species
out <- tclustIC( data.pca, numpool = 16, plot = TRUE, whichIC = "MIXMIX", kk = 1:5 )
sol <- tclustICsol( out, plot = TRUE, Rand = TRUE, NumberOfBestSolutions = 3, whichIC = "MIXMIX")
