clustering <- function(clusters, configurations, parameters, partitions) {
  # if clusters is empty, call clusterCatetgorical
  if (length(clusters) == 0) {
    return(clusterCategorical(parameters = parameters, configurations = configurations, partitions = partitions))
  } else {
    updatedClusters <- updateClusters(clusters = clusters, aliveConfigurations = configurations)
    return(clusterCategorical(parameters = parameters, configurations = configurations, existingClusters = updatedClusters, partitions = partitions))
  }
}

updateClusters <- function(clusters, aliveConfigurations) {
  # clusters: list of clusters
  # aliveConfigurations: list of alive configurations
  # returns: list of clusters
  # clusters: list of configurations
  # for each cluster, check if it contains alive configurations
  cat("Updating clusters.\n")
  for (i in 1:length(clusters)) {
    # get current cluster
    currentCluster <- clusters[[i]]
    # modify configurations that are not alive in .Alive. column and set as FALSE
    currentCluster$.ALIVE.[!currentCluster$.ID. %in% aliveConfigurations$.ID.] <- FALSE
    # update rank of alive configurations
    currentCluster$.RANK.[currentCluster$.ID. %in% aliveConfigurations$.ID.] <- aliveConfigurations$.RANK.[aliveConfigurations$.ID. %in% currentCluster$.ID.]
    # remove configurations from alive that are already in the cluster
    aliveConfigurations <- aliveConfigurations[!aliveConfigurations$.ID. %in% currentCluster$.ID.,]
  }
  return(clusters)
}

clusterCategorical <- function(parameters, configurations, existingClusters = NULL, partitions) {
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
      # add the current configuration to the new cluster
      print(currentConfiguration)
      print(newClusterIndex)
      clusters[[newClusterIndex]] <- currentConfiguration
    } else {
      # Add the current configuration to its matched cluster
      clusters[[matchedCluster]] <- rbind(clusters[[matchedCluster]], currentConfiguration)
    }
  }

  # remove empty clusters
  clusters <- clusters[!sapply(clusters, is.null)]
  cat ("Categorical clustering finished.\n")

  printCluster(clusters, configurations)
  
  cat ("Starting numerical clustering.\n")
  #empty list of final clusters
  finalClusters <- list()
  # for each cluster, apply the clustering numerical function
  for (i in 1:length(clusters)) {
    # create list of sub-clusters
    clusterConfigurations <- clusters[[i]]
    cat ("Cluster ", i, "\n")
    # apply clustering numerical function
    numericalClusters <- clusterNumerical.boxes(parameters, clusterConfigurations, partitions)
    # add sub-clusters to final clusters
    finalClusters <- c(finalClusters, numericalClusters)
  }
  printCluster(finalClusters, configurations)
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
  initialConfig <- clusterConfigurations[1,]

  clusters[[1]] <- id

  # check for each cluster if the configurations are similar, consider cluster with only one configuration
  if (length(clusterConfigurations) == 1) {
    return(clusters)
  }

  for (i in 2:length(clusterConfigurations)) {
    # get current configuration
    currentConfiguration <- clusterConfigurations[i,]
    cat ("Current configuration: \n")
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
clusterNumerical.boxes <- function(parameters, clusterConfigurations, combinations) {
  # parameters: list of parameters
  # configurations: data frame of configurations
  # partitions: list of partitions
  # returns: list of clusters
  # clusters: list of configurations
  # get numerical parameters
  numericalParameters <- parameters$names[parameters$types == "i" | parameters$types == "r"]
  clusters <- vector("list", nrow(combinations))

  # for each config
  for (i in 1:nrow(clusterConfigurations)) {
    # get current configuration
    currentConfiguration <- clusterConfigurations[i,]
    # get current configuration's numerical parameters
    currentNumericalParameters <- currentConfiguration[numericalParameters]
    # for each partition
    for (j in 1:nrow(combinations)) {
      # get current partition
      currentPartition <- combinations[j,]
      inCluster <- TRUE  # Initialize outside of the loop
      # check for each value in config if it is in the interval for its parameters
      for (k in 1:length(numericalParameters)) {
        # get current parameter
        currentParameter <- numericalParameters[k]
        # get current parameter's value
        currentParameterValue <- currentNumericalParameters[[currentParameter]]
        # replace NA values with "NA" for comparison
        if (is.na(currentParameterValue)) {
          currentParameterValue <- "NA"
        }
        # get current partition's interval
        currentInterval <- currentPartition[[currentParameter]][[1]]

        # check if value is inside interval
        if (!(currentParameterValue >= currentInterval[1] && currentParameterValue <= currentInterval[2]) && currentParameterValue != "NA") {
          inCluster <- FALSE
          break
        }
      }
      # if all values are in the interval (or are NA), add config to cluster
      if (inCluster) {
        cat ("Adding config ", currentConfiguration$.ID., " to cluster ", j, "\n")
        if (is.null(clusters[[j]])) {
          clusters[[j]] <- currentConfiguration
        } else {
          clusters[[j]] <- rbind(clusters[[j]], currentConfiguration)
        }
        break
      }
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
    clusterConfigurations <- clusters[[i]]

    # print cluster
    cat("Cluster ", i, ": \n")
    
    # print configurations with their IDs
    print(clusterConfigurations)
  }
}

dynamic_partition <- function(interval, num_partitions) {
  # interval: vector of length 2
  # num_partitions: number of partitions
  # returns: list of partitions
  # get interval bounds
  lower_bound <- interval[1]
  upper_bound <- interval[2]
  
  # get partition width
  width <- (upper_bound - lower_bound) / num_partitions
  # get partition intervals
  partitions <- lapply(1:num_partitions, function(i) {
    lower <- lower_bound + (i - 1) * width
    upper <- lower_bound + i * width
    return(c(lower, upper))
  })
  return(partitions)
}

clustering.partition <- function(parameters, num_partitions) {
  # parameters: list of parameters
  # returns: list of clusters
  # clusters: list of configurations
  # get numerical parameters
  numericalParameters <- parameters$names[parameters$types == "i" | parameters$types == "r"]
  
  # Assuming 'numericalParameters' is an atomic vector with names
  domains <- parameters$domain[parameters$names %in% numericalParameters]
  
  # for each parameter, get the partition intervals
  partitions <- lapply(domains, function(domain) {
    partition <- dynamic_partition(domain, num_partitions)
    return(partition)
  })
  ## combine partitions
  combinations <- expand.grid(partitions)
  return(combinations)
}
