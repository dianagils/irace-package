
## clustering function that clusters the configurations according to categorical parameters

clusterCategorical <- function(parameters, configurations) {
  # parameters: list of parameters
  # configurations: data frame of configurations
  # returns: list of clusters
  # clusters: list of configurations
  
  # get categorical parameters
  categoricalParameters <- parameters[parameters$types == "c",]
  
  cat("Clustering categorical parameters...\n")
  print(categoricalParameters)
  cat("\n")
}