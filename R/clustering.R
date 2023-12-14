
## clustering function that clusters the configurations according to categorical parameters

clusterCategorical <- function(parameters, configurations) {
  # parameters: list of parameters
  # configurations: data frame of configurations
  # returns: list of clusters
  # clusters: list of configurations
  # get categorical parameters
  categoricalParameters <- parameters$names[parameters$types == "c"]

  # Assuming 'categoricalParameters' is an atomic vector with names
  domains <- parameters$domain[parameters$names %in% categoricalParameters]

  combinations <- expand.grid(domains)
  # get number of clusters:
  nbClusters <- nrow(combinations)

  clusters <- vector("list", nbClusters)

  no.configurations <- nrow(configurations)
  # check each configuration and assign it to the corresponding cluster
  for (i in 1:no.configurations) {
    # get current configuration
    currentConfiguration <- configurations[i,]

    # get current configuration's categorical parameters
    currentCategoricalParameters <- currentConfiguration[categoricalParameters]
    print(currentCategoricalParameters)
    cat(",\n")

    index <- which(apply(combinations, 1, function(row) all(row == currentCategoricalParameters)))

    if (length(index) == 0) {
     # A categorical para,eter is involved, check whether its the case
      conditions <- parameters$conditions[parameters$names %in% categoricalParameters]
      if (length(conditions) > 0) {
        cat("A categorical parameter is involved, check whether its the case\n")
      }
    } else {
      # Add the current configuration to its cluster, check if it's the first configuration in the cluster
      if (is.null(clusters[[index]])) {
        clusters[[index]] <- currentConfiguration
      } else {
        clusters[[index]] <- rbind(clusters[[index]], currentConfiguration)
      }
    }
  }

  #remove empty clusters
  clusters <- clusters[!sapply(clusters, is.null)]

  #print information
  print("Number of clusters: \n")
  print(nbClusters)
  print("\nConfigurations per cluster: \n")
  for (i in 1:length(clusters)) {
    print((clusters[[i]]))
    print("\n")
  }
  
  clusterNumerical(parameters = parameters, configurations = configurations, threshold = 0.5)

}

clusterNumerical <- function(parameters, configurations, threshold) {
  # parameters: list of parameters
  # configurations: data frame of configurations
  # threshold: threshold for clustering
  # returns: list of clusters
  # clusters: list of configurations
  # get numerical parameters
  numericalParameters <- parameters$names[parameters$types == "i" | parameters$types == "r"]

  # Assuming 'numericalParameters' is an atomic vector with names
  domains <- parameters$domain[parameters$names %in% numericalParameters]

  #sort the configurations according to the numerical parameters
  configurations <- configurations[order(configurations[,numericalParameters]),]
  
  print(configurations[,numericalParameters])
  cat(",\n")
}