library(fsdaR)
library(GGally)
library(corrplot)
library(maps)
library(patchwork)
library(ufs)

# Goal: group countries based on their development status to aid NGO funds allocation (more detail on the Kaggle page).
# Data has been modified to correct some wrong values and to conform countries names to the geometry dataset ones.
class.colors <- c("orangered", "orange","green4", "royalblue", "plum","yellow3")

BoxPlotWithNames<-function(countries,data){
  final.plot <- NULL
  for (c in 1:ncol(data)){
    col<-colnames(data)[c]
    
    outliers<- iqrOutlier(data[[col]])
    labels<-countries
    labels[!outliers]<-as.numeric(NA)
    d <- data
    d["countries"]<-labels
    d["group"]<-rep(1,nrow(data))

    plot<-ggplot(d,aes_string(y=col,x="group"))+
      geom_boxplot(width=0.5)+
      geom_text(aes(label=countries),na.rm = TRUE,hjust = -.2,size=3)+
      theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks.x=element_blank())
    
    if (is.null(final.plot))
      final.plot<-plot
    else
      final.plot<-final.plot+plot
  }
  print(final.plot + plot_layout(nrow = 2))
}

# Plot density for feature "colname" with values from "raw_data"
PlotDensity <- function(classes, raw_data, colname) {
  density <- ggplot(raw_data, aes_string(x = colname, colour = as.factor(classes))) +
    geom_density() +
    scale_color_manual(values = class.colors)
  return(density)
}

# Plot world map 
PlotWorld <- function(world_geom, classes, merge_by = "region") {
  # Join classes with country names
  d<- classes
  d$region <- Country_data$country
  
  # Join country geometric data with classes
  d <- merge(world_geom, d, by = merge_by)
  d <- d[order(d$order, decreasing = FALSE), ]
  
  map <- ggplot(data = d, mapping = aes(x = long, y = lat, group = group)) +
    geom_polygon(data = world_geom) +
    geom_polygon(aes(fill = as.factor(class))) +
    scale_fill_manual(name = "Group", values = class.colors) +
    geom_path() +
    theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'white'))
  return(map)
}

# Plot all solutions
PlotSolutions <- function(raw_data, out, sol) {
  for (i in 1:nrow(sol$MIXMIXbs))
  {
    k <- paste("k=", sol$MIXMIXbs[i, 1], sep = '')
    c <- paste("c=", sol$MIXMIXbs[i, 2], sep = '')
    is.solution <- sol$MIXMIXbs[i, 5]
    classes <- as.data.frame(out$IDXMIX[k, c])
    colnames(classes)[1] <- "class"
    
    plot.title<- paste("MIXMIX Solution", i,  k, c, "solution:", is.solution)
    
    map.plot<-PlotWorld(map_data("world"), classes, "region")
    
    densities.plot <- plot.density(classes$class, raw_data, names(raw_data)[1])
    for (j in 2:ncol(raw_data))
    {
      densities.plot <- densities.plot + plot.density(classes$class, raw_data, names(raw_data)[j])
    }
    
    comb.plot <- map.plot / densities.plot +
      plot_annotation(title = plot.title,theme = theme(plot.title = element_text(hjust = 0.5)))
    print(comb.plot)
  }
}

#Remove country names
data <- Country_data[, 2:10]

summary(data)
cov(data)
# High correlation between (life_expec, child_mort), (total_fer, child_mor), (gdpp, income), (total_fer, life_expec)
corrplot(cor (data), method = 'color',type = 'lower')
# Some outliers, won't be removed as we want to have a group for all countries
BoxPlotWithNames(Country_data$country,data)


#PCA, keep 5 components which explain about 94% of variance
data.pca <- prcomp(data, scale = TRUE)
summary(data.pca)
data.pca <- as.data.frame(data.pca$x[, 1:5])

ggpairs(data.pca,upper = list(continuous = 'points'),aes(color=rep(1,nrow(data.pca))))

# Try with default parameters.
out <- tclustIC(data.pca, whichIC = 'MIXMIX', plot = TRUE)
sol <- tclustICsol(out, NumberOfBestSolutions = 2, plot = TRUE, whichIC = 'MIXMIX')
carbikeplot(sol,SpuriousSolutions = TRUE)

# Not what we wanted. For example solution 1 puts countries like Russia and China together with African countries.
# Those countries have on average low income, gdpp and life expectancy, therefore those two are definetly not belonging in this group.
# Moreover from the scatter plots we see that one cluster is elongated and sparse on some axes.
PlotSolutions(data, out, sol)

# Try with lower maximum cc values.
out <- tclustIC(data.pca, whichIC = 'MIXMIX', plot = TRUE, cc = c(1, 2, 3, 4, 5, 6, 7))
sol <- tclustICsol(out, NumberOfBestSolutions = 2, plot = TRUE, whichIC = 'MIXMIX')

PlotSolutions(data, out, sol)

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

