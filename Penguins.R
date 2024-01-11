library(palmerpenguins)
library(fsdaR)
library(GGally)
library(corrplot)

# Returns a vector of groups for the solution with k and c
GetGroups<-function(out, sol, k, c){
  k <- paste("k=", k, sep = '')
  c <- paste("c=", c, sep = '')
  return(out$IDXMIX[[k, c]])
}

# Calculate ARI for all solution with respect to original groups
GetAri<-function(out,sol,raw_groups){
  nsol<-nrow(sol$MIXMIXbs)
  df<-data.frame(matrix(nrow = nsol,ncol = 3))
  colnames(df)<-c("k","c","ARI")
  
  for (i in 1:nsol)
  {
    k<-sol$MIXMIXbs[i, 1]
    c<-sol$MIXMIXbs[i, 2]
    groups<-GetGroups(out,sol,k,c)
    ari<-adjustedRandIndex(groups,as.factor(raw_groups))
    df[i,1]<-k
    df[i,2]<-c
    df[i,3]<-ari
  }
  
  return(df)
}

# Plot densities for each solution
PlotSolutions <- function(raw_data, out, sol) {
  for (i in 1:nrow(sol$MIXMIXbs))
  {
    k <- sol$MIXMIXbs[i, 1]
    c <- sol$MIXMIXbs[i, 2]
    is.solution <- sol$MIXMIXbs[i, 5]
    classes <- as.data.frame(GetGroups(out,sol,k,c))
    colnames(classes)[1] <- "class"
    
    plot.title <- paste("Solution", i,"k =", k ,"c =", c, "solution =",is.solution)
    plot <- ggpairs((raw_data),
              aes(color = as.factor(classes$class)),
              upper = list(continuous = 'points'),
              legend = 1,
              title = plot.title) +
      theme(legend.position = 'bottom',plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Species")
    print(plot)
  }
}

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

# Groups are well separated, so the procedure should work well
ggpairs( data.numeric, aes(color = data.full$species), upper = list(continuous = 'points'), legend = 1) +
  theme(legend.position = 'top') +
  labs(fill = "Species")

# PCA, keep only 3 components which explain about 97% of variance
data.pca <- prcomp(data.numeric, scale = TRUE)
summary(data.pca)
data.pca <- as.data.frame(data.pca$x[, 1:3])

ggpairs(data.pca, aes(color = data.full$species), upper = list(continuous = 'points'), legend = 1) +
  theme(legend.position = 'top') +
  labs(fill = "Species")

# The best solution has 3 components which coincide with real species
out <- tclustIC(data.pca, numpool = 16, plot = TRUE, whichIC = "MIXMIX", kk = 1:5)
sol <- tclustICsol(out, plot = TRUE, Rand = TRUE, NumberOfBestSolutions = 3, whichIC = "MIXMIX")
carbikeplot(sol,SpuriousSolutions = TRUE)
# Good performance with respect to the original groups for first and second solution
ari <- GetAri(out,sol,unclass(data.full$species))
ari

PlotSolutions(data.numeric, out, sol)
#About the other solutions:
# 2. 4 groups found. Two of them have slightly different average values.
#    By looking at he original species we can see that Adelie and Chinstrap are almost the same while Gentoo is split in two subspecies.
#    This may be due to gender differences
# 3. 2 well separated groups. Adelie and Chinstrap are merged while Gentoo is in its own group.
#    This suggests that two of the species might be relatively similar.