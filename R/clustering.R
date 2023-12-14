
## clustering function that clusters the configurations according to categorical parameters

clusterCategorical <- function(parameters, configurations) {
  # parameters: list of parameters
  # configurations: data frame of configurations
  # returns: list of clusters
  # clusters: list of configurations
  
  # get categorical parameters
  categoricalParameters <- parameters[parameters$types == "c",]

  # get number of categorical parameters
  nbCategoricalParameters <- nrow(categoricalParameters)

  # get number of clusters: 2^nbCategoricalParameters
  nbClusters <- 2^nbCategoricalParameters

  clusters <- vector("list", nbClusters)

  # combine all domains of categorical parameters
  domains <- categoricalParameters$domain
  combinations <- expand.grid(domains)

  # check each configuration and assign it to the corresponding cluster
  for (i in 1:nrow(configurations)) {
    # get current configuration
    currentConfiguration <- configurations[i,]

    # get current configuration's categorical parameters
    currentCategoricalParameters <- currentConfiguration[categoricalParameters$names]

    # get index of current configuration's cluster
    index <- which(combinations == currentCategoricalParameters)

    # add current configuration to its cluster
    clusters[[index]] <- c(clusters[[index]], currentConfiguration)
  }
  #print information
  print("Number of clusters: \n")
  print(nbClusters)
  print("\nConfigurations per cluster: \n")
  for (i in 1:nbClusters) {
    print((clusters[[i]]))
    print("\n")
  }
  
  
}