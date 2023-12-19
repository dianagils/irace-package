clustering <- function(clusters, configurations, parameters) {
  # if clusters is empty, call clusterCatetgorical
  if (length(clusters) == 0) {
    return(clusterCategorical(parameters, configurations))
  } else {
    return(clusterCategorical(parameters, configurations, clusters))
  }

}

clusterCategorical <- function(parameters, configurations, existingClusters = NULL) {
  # parameters: list of parameters
  # configurations: data frame of configurations
  # returns: list of clusters
  # clusters: list of configurations
  # get categorical parameters
  categoricalParameters <- parameters$names[parameters$types == "c" | parameters$types == "o" ]

  # Assuming 'categoricalParameters' is an atomic vector with names
  domains <- parameters$domain[parameters$names %in% categoricalParameters]

  combinations <- expand.grid(domains)
  # get number of clusters:
  nbClusters <- nrow(combinations)

  # TODO : Check no conditional parameters case
  # add clusters for adding conditional parameters
  conditionalsNames <- getConditionalParameters(parameters, categoricalParameters)
  # create df of conditionals with NA values
  conditionals <- data.frame(matrix("NA", nrow = 1, ncol = length(conditionalsNames)))
  names(conditionals) <- conditionalsNames
  conditionalsCombination <- expand.grid(conditionals)

  # add clusters for each conditional parameter and their values
  nbClustersCat <- nbClusters + length(conditionalsCombination)

  clusters <- vector("list", nbClustersCat)

  # If existing clusters are provided, initialize with them
  if (!is.null(existingClusters)) {
    cat("Existing clusters are detected.\n")
    clusters <- existingClusters
  }

  no.configurations <- nrow(configurations)

  # check each configuration and assign it to the corresponding cluster
  for (i in 1:no.configurations) {
    # get current configuration
    currentConfiguration <- configurations[i,]

    # get current configuration's categorical parameters
    currentCategoricalParameters <- currentConfiguration[categoricalParameters]
    # change NA values to "NA" for comparison
    currentCategoricalParameters[is.na(currentCategoricalParameters)] <- "NA"
    # Check if it matches any existing cluster
    matchedCluster <- NULL
    for (j in seq_along(clusters)) {
      #cat("Checking cluster ", j, "\n")
      #print(clusters[[j]])
      clusterConfigurations <- clusters[[j]]
      # change NA values to "NA" for comparison
      clusterConfigurations[is.na(clusterConfigurations)] <- "NA"
      #print(clusterConfigurations)
      if (!is.null(clusters[[j]]) && all(apply(clusterConfigurations[categoricalParameters], 1, function(x) all(x == currentCategoricalParameters)))) {
        matchedCluster <- j
        break
      }
    }

    if (is.null(matchedCluster)) {
      # No match found, find the first empty cluster and add the current configuration to it
      newClusterIndex <- which(sapply(clusters, is.null))[1]
      # if there are no empty clusters, add a new cluster
      if (is.null(newClusterIndex)) {
        newClusterIndex <- length(clusters) + 1
      }
      clusters[[newClusterIndex]] <- currentConfiguration
    } else {
      # Add the current configuration to its matched cluster
      clusters[[matchedCluster]] <- rbind(clusters[[matchedCluster]], currentConfiguration)
    }
  }

  # remove empty clusters
  clusters <- clusters[!sapply(clusters, is.null)]
  cat ("Categorical clustering finished.\n")
  cat ("Number of clusters: ", length(clusters), "\n")
  printCluster(clusters, configurations)

  finalClusters <- vector("list", length(clusters))
  
  cat ("Starting numerical clustering.\n")
  # for each cluster, apply the clustering numerical function
  for (i in 1:length(clusters)) {
    # create list of sub-clusters
    clusterConfigurations <- clusters[[i]]
    cat ("Cluster ", i, "\n")
    # apply clustering numerical function
    numericalClusters <- clusterNumerical(parameters = parameters, clusterConfigurations = clusterConfigurations, configurations = configurations, threshold = 0.9)
    # add sub-clusters to final clusters
    finalClusters[[i]] <- numericalClusters
  }

  return(finalClusters)
}

clusterNumerical <- function(parameters, clusterConfigurations, configurations, threshold) {
  # parameters: list of parameters
  # configurations: data frame of configurations
  # threshold: threshold for clustering
  # returns: list of clusters
  # clusters: list of configurations
  # get numerical parameters
  numericalParameters <- parameters$names[parameters$types == "i" | parameters$types == "r"]

  # Assuming 'numericalParameters' is an atomic vector with names
  domains <- parameters$domain[parameters$names %in% numericalParameters]

  clusters <- vector("list", length(clusterConfigurations))
  print(clusterConfigurations)

  # pick initial config for comparison and add to first cluster
  id <- as.numeric(clusterConfigurations[1])
  initialConfig <- configurations[configurations$.ID. == id, ]

  clusters[[1]] <- id

  # check for each cluster if the configurations are similar, consider cluster with only one configuration
  if (length(clusterConfigurations) == 1) {
    return(clusters)
  }

  for (i in 2:length(clusterConfigurations)) {
    cat ("Checking config ", i, "\n")
    # get current configuration
    currentConfiguration <- clusterConfigurations[i,]
    print(currentConfiguration)
    # check if current configuration is similar to initial configuration
    if (!checkConfigDifference(initialConfig, currentConfiguration, numericalParameters, threshold)) {
      # add Id of config to new cluster
      clusters[[i]] <- currentConfiguration
      # set new initial configuration
      initialConfig <- currentConfiguration
    } else {
      # add configuration to current cluster
      clusters[[i]] <- rbind(clusters[[i]], currentConfiguration)
    }
  }
  #remove empty clusters
  clusters <- clusters[!sapply(clusters, is.null)]

  return(clusters)
  
}

checkConfigDifference <- function(config1, config2, parameters, threshold) {
  # config1: first configuration
  # config2: second configuration
  # parameters: list of parameters names
  # threshold: threshold for clustering
  # returns: boolean value, true if configurations are similar, false otherwise
  cat ("Checking difference between configurations.\n")
  cat ("Config 1: \n")
  print(config1)
  cat ("Config 2: \n")
  print(config2)
  if (any(is.na(config1)) || any(is.na(config2))) {
    # get the NA values parameter names 
    naParametersConfig1 <- names(config1)[is.na(config1)]
    naParametersConfig2 <- names(config2)[is.na(config2)]
    # check if names are equal
    if (identical(naParametersConfig1, naParametersConfig2)) {
     # if they are the same, remove names from parameters list
      parameters <- parameters[!parameters %in% naParametersConfig1]
    } else {
      # if they are not the same, return false
      return(FALSE)
    }
  }

  # get vector of differences for each parameter
  difference <- sapply(parameters, function(parameter) {
    parameterValue1 <- config1[parameter]
    # get parameter value from config2
    parameterValue2 <- config2[parameter]
    # check if parameter is categorical
      # if parameter is numerical, check if values are equal
      if (parameterValue1 == parameterValue2) {
        return(0)
      } else {
        # if values are not equal, return the difference between them
        return(abs(parameterValue1 - parameterValue2))
    }
  })
  
  # Check for missing values in the difference vector
  if (any(is.na(difference))) {
    return(FALSE)  # If there are missing values, consider the configurations different
  }
  
  # If any of the differences is bigger than the threshold, then the configs are not similar. Add to a new cluster
  if (any(difference > threshold)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# return number of parameters that are conditional 
getConditionalParameters <- function(parameters, parameterType) {
  # parameters: list of parameters
  # returns: number of conditional parameters
  # get conditional parameters
  conditions <- parameters$conditions[parameters$names %in% parameterType]
  filtered_conditions <- Filter(function(x) !identical(x, TRUE), conditions)
  return(names(filtered_conditions))
}

printCluster <- function(clusters, configurations) {
  # clusters: list of clusters (containing IDs)
  # configurations: data frame of configurations
  # print clusters
  for (i in 1:length(clusters)) {
    # get IDs of configurations in the cluster
    clusterIDs <- clusters[[i]]
    
    # get configurations of cluster using the IDs
    clusterConfigurations <- configurations[configurations$.ID. %in% clusterIDs, ]
    
    # print cluster
    cat("Cluster ", i, ": \n")
    
    # print configurations with their IDs
    print(clusterConfigurations)
  }
}

dynamic_partition <- function(interval, partition_width) {
  interval_width <- diff(interval)
  num_partitions <- floor(interval_width / partition_width)
  
  # Calculate the partition intervals
  partitions <- lapply(0:num_partitions, function(i) {
    start <- interval[1] + i * partition_width
    end <- start + partition_width
    return(c(start, end))
  })
  
  return(partitions)
}

clustering.partition <- function(parameters) {
  # parameters: list of parameters
  # returns: list of clusters
  # clusters: list of configurations
  # get numerical parameters
  numericalParameters <- parameters$names[parameters$types == "i" | parameters$types == "r"]
  
  # Assuming 'numericalParameters' is an atomic vector with names
  domains <- parameters$domain[parameters$names %in% numericalParameters]
  
  # for each parameter, get the partition intervals
  partitions <- lapply(domains, function(domain) {
    width <- 0.1 * diff(domain)
    partition <- dynamic_partition(domain, width)
    return(partition)
  })
  cat("Partitions: \n")
  print(partitions)
}
