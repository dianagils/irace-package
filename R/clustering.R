clustering <- function(clusters, configurations, parameters, partitions, results) {
  configurations <- addResultsToConfigurations(aliveConfigurations = configurations, results = results)
  if (nrow(clusters) == 0) {
    return(clusterConfigurations(parameters = parameters, configurations = configurations, partitions = partitions))
  } else {
    # update clusters
    result <- updateClusters(clusteredConfigs = clusters, aliveConfigurations = configurations)
    return(clusterConfigurations(parameters = parameters, configurations = result$configurations, existingClusters = result$clusters, partitions = partitions))
  }
}

addResultsToConfigurations <- function(aliveConfigurations, results) {
  # configurations: data frame of configurations
  # results: list of results
  # returns: data frame of configurations with results
  # add results to configurations
  # columns are configurations, rows are instances and value is the result
  # for each config, add a column with the average of the results
  for (i in 1:nrow(aliveConfigurations)) {
    currentID <- aliveConfigurations[i, ]$.ID.
    currentResults <- results[[currentID]]
    # add column to configurations
    aliveConfigurations[i, ".RESULTS."] <- mean(currentResults)
  }
  return(aliveConfigurations)
}

filterClusteringParams <- function(parameters) {
  # parameters: list of parameters
  # returns: list of parameters
  # remove parameter names that have clustering = FALSE
  parameters$names <- parameters$names[parameters$clustering == TRUE]
  parameters$types <- parameters$types[parameters$clustering == TRUE]
  parameters$domain <- parameters$domain[parameters$clustering == TRUE]
  parameters$conditions <- parameters$conditions[parameters$clustering == TRUE]
  print(parameters)
  return (parameters)
}

updateClusters <- function(clusteredConfigs, aliveConfigurations) {
  cat("Updating clusters.\n")
  for (i in 1:nrow(aliveConfigurations)) {
    currentID <- aliveConfigurations[i, ]$.ID.    # get index of config in clusteredConfigs with the same ID
    index <- which(clusteredConfigs$.ID. == currentID)
    # if there is no config with the same ID, skip
    if (length(index) == 0) {
      next
    }
    # update rank and alive
    config <- clusteredConfigs[index, ]
    config$.RANK. <- aliveConfigurations[i, ]$.RANK.
    config$.ALIVE. <- aliveConfigurations[i, ]$.ALIVE.
    config$.RESULTS. <- aliveConfigurations[i, ]$.RESULTS.

    # update cluster
    clusteredConfigs[index, ] <- config  # Use single brackets here
  }
  # remove from aliveConfigurations the configurations that are already in clusteredConfigs
  aliveConfigurations <- aliveConfigurations[!aliveConfigurations$.ID. %in% clusteredConfigs$.ID., ]

  return(list(clusters = clusteredConfigs, configurations = aliveConfigurations))
}


clusterConfigurations <- function(parameters, configurations, existingClusters = NULL, partitions) {
  # parameters: list of parameters
  # configurations: data frame of configurations
  # returns: list of clusters
  # clusters: list of configurations
  # get categorical parameters
  categoricalParameters <- parameters$names[parameters$types == "c" | parameters$types == "o" ]

  # Assuming 'categoricalParameters' is an atomic vector with names
  domains <- parameters$domain[parameters$names %in% categoricalParameters]
  # Add NA value to conditional parameters when 

  combinations <- createCategoricalParamGrid(parameters)

  nbClusters <- nrow(combinations)

  no.configurations <- nrow(configurations)
  # add .CLUSTER. column to configurations
  configurations$.CLUSTER. <- NA

  if (length(categoricalParameters) == 0) {
    cat("No categorical parameters detected.\n")
    nbClusters <- 1
    # if there are no categorical parameters, add all configurations to the same cluster == 1
    configurations$.CLUSTER. <- 1
  } else {
  # check each configuration and assign it to the corresponding cluster
    for (i in 1:no.configurations) {
      # get current configuration
      currentConfiguration <- configurations[i,]
      # get current configuration's categorical parameters
      currentCategoricalParameters <- currentConfiguration[categoricalParameters]
      # change NA values to "NA" for comparison
      currentCategoricalParameters[is.na(currentCategoricalParameters)] <- "NA"
      # apply lapply to each row of combinations to check if it is equal to current configuration
      currentCombination <- which(apply(combinations, 1, function(x) all(x == currentCategoricalParameters)))

      # if the combination is not found, check if it is a conditional parameter
      if (length(currentCombination) == 0) {
        currentCombination <- nbClusters + 1
        }
      # add cluster column to configuration
      currentConfiguration$.CLUSTER. <- currentCombination
      configurations[i,] <- currentConfiguration
    }
  }
  cat ("Categorical clustering finished.\n")

  numericalParameters <- parameters$names[parameters$types == "i" | parameters$types == "r"]
  if (length(numericalParameters) == 0) {
    cat("No numerical parameters detected.\n")
    printCluster(nbClusters, configurations)
    return(configurations)

  } else {
  # for each cluster, apply the clustering numerical function
  for (i in 1:nbClusters) {
    cat("Clustering cluster ", i, "\n")
        # Add subcluster column to configurations
    configurations[configurations$.CLUSTER. == i, ".SUBCLUSTER."] <- NA
    # Subset configurations for the current cluster
    currentCluster <- configurations[configurations$.CLUSTER. == i,]
    
    # If there are no configs, skip
    if (nrow(currentCluster) == 0) {
      next
    }
    
    # Apply clustering function
    updatedCluster <- clusterNumerical.boxes(numericalParameters, currentCluster, partitions)
    print(updatedCluster)
    
    # Update configurations with the results
    configurations[configurations$.CLUSTER. == i, ] <- updatedCluster
  }

  cat ("Numerical clustering finished.\n")
   if (!is.null(existingClusters)) {
    #rbind existing clusters and new clusters
    configurations <- rbind(existingClusters, configurations)
  }
  printCluster(nbClusters, configurations, nbSubClusters = nrow(partitions))
  }
  summarizeClusters(configurations)
  return(configurations)
}

clusterNumerical.boxes <- function(numericalParameters, clusterConfigurations, combinations) {
  # add column to clusterConfigurations
  clusterConfigurations$.SUBCLUSTER. <- NA
  nbSubClusters <- nrow(combinations)
  
  # for each config
  for (i in 1:nrow(clusterConfigurations)) {
    currentConfiguration <- clusterConfigurations[i,]
    currentNumericalParameters <- currentConfiguration[numericalParameters]

    subcluster <- find_partition(currentNumericalParameters, combinations, numericalParameters)
    
    cat("Adding config ", i ," to subcluster ", subcluster, "\n")
    clusterConfigurations[i,]$.SUBCLUSTER. <- subcluster
  }
  return(clusterConfigurations)
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


printCluster <- function(nbClusters, configurations, nbSubClusters = NULL) {
  # nbClusters: number of clusters
  # configurations: data frame of configurations with .CLUSTER. column
  # print the configurations in each cluster
  cat("Printing clusters.\n")
  
  for (i in 1:nbClusters) {
    cat("Cluster ", i, "\n")
    configs <- configurations[configurations$.CLUSTER. == i,]
    
    if (!is.null(nbSubClusters)) {
      # get .SUBCLUSTER. column
      subclusters <- configs$.SUBCLUSTER.
      # get unique subclusters
      uniqueSubclusters <- unique(subclusters)
      
      for (j in 1:length(uniqueSubclusters)) {
        cat("Subcluster ", uniqueSubclusters[j], "\n")
        subclusterConfigs <- configs[configs$.SUBCLUSTER. == uniqueSubclusters[j],]
        print(subclusterConfigs)
      }
    } else {
      print(configs)
    }
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

find_partition <- function(configuration, partitions, parameters) {
  # check las parameter
  parameter <- parameters[length(parameters)]
  parameter_value <- configuration[parameter][1,]
  parameter_partition <- partitions[[parameter]]
  # check first interval of last parameter definition
  intervalFound <- FALSE
  while (!intervalFound) {
    interval <- parameter_partition[[1]]
    if ((parameter_value >= interval[1] && parameter_value <= interval[2]) | (is.na(parameter_value))) {
      intervalFound <- TRUE
      if (length(parameters) == 1) {
        # return index of interval
        # remove all rows that do not match interval in current parameter from partitions
        rows_to_remove <- sapply(partitions[[parameter]], function(x) x[1] != interval[1] | x[2] != interval[2])
        partitions <- partitions[!rows_to_remove,, drop = FALSE]
        row_index <- as.numeric(rownames(partitions))
        return(row_index)
      } else {
        # remove all rows that do not match interval in current parameter from partitions
        rows_to_remove <- sapply(partitions[[parameter]], function(x) x[1] != interval[1] | x[2] != interval[2])
        partitions <- partitions[!rows_to_remove, , drop = FALSE]
        parameter_partition <- partitions[[parameter]]
      }
    } else {
      # remove all rows that match interval in current parameter from partitions
      rows_to_remove <- sapply(partitions[[parameter]], function(x) x[1] == interval[1] & x[2] == interval[2])
      partitions <- partitions[!rows_to_remove, , drop = FALSE]
      parameter_partition <- partitions[[parameter]]
      }
    }
  # remove last parameter from partitions
  partitions <- partitions[!names(partitions) == parameter]
  # remove last parameter from parameters
  parameters <- parameters[parameters != parameter]
  # remove last parameter from configuration
  configuration <- configuration[!names(configuration) == parameter,]
  # find partition for remaining parameters

  return(find_partition(configuration, partitions, parameters))
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

createCategoricalParamGrid <- function(parameters) {
  # parameters: list of parameters
  # domains: list of domains
  # returns: list of combinations
  # get categorical parameters
  categoricalParameters <- parameters$names[parameters$types == "c" | parameters$types == "o" ]
  # Assuming 'categoricalParameters' is an atomic vector with names
  domains <- parameters$domain[parameters$names %in% categoricalParameters]
  # get conditional parameters
  conditions <- parameters$conditions[parameters$names %in% categoricalParameters]
  # create param grid
  combinations <- expand.grid(domains)
  # check conditions for each combination
  for (i in seq_len(nrow(combinations))) {
    config <- as.data.frame(combinations[i, , drop = FALSE])
    for (param in names(config)) {
      if (!conditionsSatisfied(parameters, config, param)) {
        combinations[i, param] <- "NA"
      }
    }
  }
  return(combinations)
}

summarizeClusters <- function(configurations) {
  summary_df <- data.frame(cluster = integer(),
                          subcluster = integer(),
                          MeanPerformance = numeric(),
                          StdDevPerformance = numeric())

  # Iterate over unique clusters and subclusters
  unique_clusters <- unique(configurations$.CLUSTER.)
  unique_subclusters <- unique(configurations$.SUBCLUSTER.)

  for (cluster in unique_clusters) {
    for (subcluster in unique_subclusters) {
      # Subset the configurations dataframe for the current cluster and subcluster
      subset_df <- configurations[configurations$.CLUSTER. == cluster & configurations$.SUBCLUSTER. == subcluster, ]

      # Remove NA from results, replace with 0
      subset_df$.RESULTS.[is.na(subset_df$.RESULTS.)] <- 0

      # Calculate mean and standard deviation of the RESULTS column
      mean_performance <- mean(subset_df$.RESULTS.)
      std_dev_performance <- sd(subset_df$.RESULTS.)

      # Append the summary information to the summary dataframe
      summary_df <- rbind(summary_df, data.frame(cluster = cluster,
                                                subcluster = subcluster,
                                                MeanPerformance = mean_performance,
                                                StdDevPerformance = std_dev_performance))
    }
  }
  print(summary_df)
  return(summary_df)
}

chooseClusterRepresentative <- function(configurations, summary_df) {
  # configurations: data frame of configurations
  # summary_df: data frame of summary information
  # returns: data frame of configurations with .REPRESENTATIVE. column
  # add .REPRESENTATIVE. column to summary_df
  configurations$.REPRESENTATIVE. <- NA

  # Iterate over unique clusters
  unique_clusters <- unique(configurations$.CLUSTER.)
  for (cluster in unique_clusters) {
   
  }
  return(configurations)
}