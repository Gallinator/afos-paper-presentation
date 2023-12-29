library(fsdaR)


data <- iris[, 3:4]
summary(data)
cov(data)
data <- scale(data)
plot(data)

? tclustIC
out <- tclustIC(data, plot = TRUE, whichIC = 'MIXMIX')
? tclustICsol
solutions <- tclustICsol(out, NumberOfBestSolutions = 3, plot = TRUE, whichIC = 'MIXMIX', Rand = TRUE )
