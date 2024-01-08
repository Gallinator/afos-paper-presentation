library(fsdaR)
library(GGally)
library(corrplot)
library(maps)
library(patchwork)

# Goal: group countries based on their development status to aid NGO funds allocation (more detail on the Kaggle page).
# Data has been modified to correct some wrong values and to conform countries names to the geometry dataset ones.
class.colors <- c("red", "orange", "yellow", "green", "blue", "purple")

# Plot density for feature "colname" with values from "raw_data"
plot.density <- function(classes, raw_data, colname) {
  density <- ggplot(raw_data, aes_string(x = colname, colour = as.factor(classes))) +
    geom_density() +
    scale_color_manual(values = class.colors)
  return(density)
}

# Plot world map 
plot.world <- function(world_geom, class_data, merge_by = "region") {
  # Join country geometric data with classes
  d <- merge(world_geom, class_data, by = merge_by)
  d <- d[order(d$order, decreasing = FALSE), ]
  
  map <- ggplot(data = d, mapping = aes(x = long, y = lat, group = group)) +
    geom_polygon(data = world_geom) +
    geom_polygon(aes(fill = as.factor(class))) +
    scale_fill_manual(name = "Class", values = class.colors) +
    geom_path() +
    theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'white'))
  return(map)
}

# Plot map and density 
plot.model.map <- function(classes) {
  cluster.data <- classes
  cluster.data$region <- Country_data$country
  return(plot.world(map_data("world"), cluster.data, "region"))
}

# Plot all solutions
plot.solutions <- function(raw_data, out, sol) {
  for (i in 1:nrow(sol$MIXMIXbs))
  {
    k <- paste("k=", sol$MIXMIXbs[i, 1], sep = '')
    c <- paste("c=", sol$MIXMIXbs[i, 2], sep = '')
    classes <- as.data.frame(out$IDXMIX[k, c])
    colnames(classes)[1] <- "class"
    
    map.plot <- plot.model.map(classes)
    
    densities.plot <- plot.density(classes$class, raw_data, names(raw_data)[1])
    for (j in 2:ncol(raw_data))
    {
      densities.plot <- densities.plot + plot.density(classes$class, raw_data, names(raw_data)[j])
    }
    
    comb.plot <- map.plot / densities.plot + plot_annotation(title = paste("Solution", i, k, c))
    print(comb.plot)
  }
}

#Remove country names
data <- Country_data[, 2:10]

summary(data)
cov(data)
# High correlation between (life_expec, child_mort), (total_fer, child_mor), (gdpp, income), (total_fer, life_expec)
corrplot(cor (data), method = 'color')
# Some outliers, won't be removed as we want to have a group for all countries
boxplot(scale(data))

#PCA, keep 5 components which explain about 94% of variance
data.pca <- prcomp(data, scale = TRUE)
summary(data.pca)
data.pca <- as.data.frame(data.pca$x[, 1:5])

ggpairs(data.pca)

# Try with default parameters.
out <- tclustIC(data.pca, whichIC = 'MIXMIX', plot = TRUE)
sol <- tclustICsol(out, NumberOfBestSolutions = 2, plot = TRUE, whichIC = 'MIXMIX')

# Not what we wanted. For example solution 1 puts countries like Russia and China together with African countries.
# Those countries have on average low income, gdpp and life expectancy, therefore those two are definetly not belonging in this group.
# Moreover from the scatter plots we see that one cluster is elongated and sparse on some axes.
plot.solutions(data, out, sol)

# Try with lower maximum cc values.
out <- tclustIC(data.pca, whichIC = 'MIXMIX', plot = TRUE, cc = c(1, 2, 3, 4, 5, 6, 7))
sol <- tclustICsol(out, NumberOfBestSolutions = 2, plot = TRUE, whichIC = 'MIXMIX')

plot.solutions(data, out, sol)

# The scatter plots show better cluster shapes with less elongated and sparse clusters.
# For solution 1 from the density plots we can see that he three groups have the following characteristics:
# 1. Low child mortality, high income, low inflation, high life expectancy, low child mortality
#    Developed countries
# 2. Medium child mortality, medium gdpp, medium inflation, medium life expectancy
#    Developing countries
# 3. High child mortality (high variance), low income, low life expectancy, high fertility,low gdpp
#    Underdeveloped countries
# Solution 2 is similar to 1 but the differences between groups are more evident.
# 1. Low gdpp, low income, high child mortality
#    Underdeveloped countries
# 2. High gdpp, high income, low child mortality
#    Developed countries
# It must be noted that the procedure always found the correct k but we still had to tune cc to get a reasonable result

