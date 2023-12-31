####################################################
## INITIALISE AND UPDATE THE MODEL
####################################################

## Initialisation of the model after the first iteration
##
# IN: configurations matrix, parameters datastructure
##
# OUTPUT: A list of list of vectors. The higher-level list contains
# one element per categorical parameter.  Each categorical parameter
# contains a list of vector. This list contains elements which are the
# .ID. of the configuration. 
initialiseModel <- function (parameters, configurations)
{
  nbConfigurations <- nrow(configurations)
  ids <- as.character(configurations[[".ID."]])
  param_names <- parameters$names[!parameters$isFixed]
  model <- setNames(vector("list", length(param_names)), param_names)
                    
  for (currentParameter in param_names) {
    type <- parameters$types[[currentParameter]]
    if (type == "r" || type == "i") {
      value <- init.model.numeric(currentParameter, parameters)
      # Assign current parameter value to model
      param <- mapply(c, value, configurations[[currentParameter]],
                      SIMPLIFY=FALSE, USE.NAMES=FALSE)
    } else {
      nbValues <- length(parameters$domain[[currentParameter]])
      if (type == "c") {
        value <- rep((1 / nbValues), nbValues)
      } else {
        irace.assert(type == "o")
        value <- (nbValues - 1) / 2
      }
      param <- rep(list(value), nbConfigurations)
    }
    names(param) <- ids
    model[[currentParameter]] <- param
  }
  model
}

## FIXME (MANUEL): This function needs a description.
## Update the model 
updateModel <- function (parameters, eliteConfigurations, oldModel,
                         indexIteration, nbIterations, nbNewConfigurations, scenario)
{
  newModel <- list()
  
  for (idxConfiguration in seq_len(nrow(eliteConfigurations))) {
    idCurrentConfiguration <- eliteConfigurations[idxConfiguration, ".ID."]
    idCurrentConfiguration <- as.character(idCurrentConfiguration)

    for (currentParameter in parameters$names[!parameters$isFixed]) {
      type <- parameters$types[[currentParameter]]
      
      ## If the elite is older than the current iteration, it has
      ## its own model that has evolved with time. If the elite is
      ## new (generated in the current iteration), it does not have
      ## any, and we have to copy the one from its parent. The
      ## condition of the IF statement is for checking wether the
      ## configuration already has its model or not.
      
      # FIXME: FIX character IDs, they should be numeric!
      if (idCurrentConfiguration  %in% names(oldModel[[currentParameter]])) {
        # cat("This configuration has already an entry, to be updated\n")
        probVector <- oldModel[[currentParameter]][[idCurrentConfiguration]]
      } else {
        # cat("This configuration does not have any entry, copy the parent one\n")
        idParent <- eliteConfigurations[idxConfiguration, ".PARENT."]
        irace.assert(as.integer(idParent) < as.integer(idCurrentConfiguration))
        idParent <- as.character(idParent)
        # cat("The parent found is ", idParent, "\n")
        probVector <- oldModel[[currentParameter]][[idParent]]
        # Change the current parameter value of the model
        if (type %in% c("i", "r") &&
            !is.na(eliteConfigurations[idCurrentConfiguration,currentParameter]))
          probVector[2] <- eliteConfigurations[idCurrentConfiguration,currentParameter]
      }
      # cat("probVector: ", probVector)

      if (type == "c") {
        actualValue <- eliteConfigurations[idxConfiguration, currentParameter]
      
        if (is.na(actualValue)) {
          # cat ("NA found, don't change the prob vector")
        } else {
          possibleValues <- parameters$domain[[currentParameter]]
          # Decrease first all values in the vector:
          probVector <- probVector * (1 - ((indexIteration - 1) / nbIterations))
          # cat("new probVector after decrease: ", probVector)
          
          # Find the value that has been "chosen" to increase its probability.
          indexValue <- which (possibleValues == actualValue)
          probVector[indexValue] <- (probVector[indexValue]
                                      + ((indexIteration - 1) / nbIterations))
#                 cat("The value found for the configuration n.",
#                 idxConfiguration, "(ID=",
#                 idCurrentConfiguration, ") is the ", indexValue,
#                 "th.\n")

          # Prevent probabilities from growing too much.
          if (scenario$elitist) {
            probVector <- probVector / sum(probVector)
            probMax    <- 0.2^(1 / parameters$nbVariable)
            probVector <- pmin(probVector, probMax)
          }
          # Normalize probabilities.
          probVector <- probVector / sum(probVector)
          #print("newProbVector after increase: ")
          #print(newVector)  
        }
      } else {
        irace.assert(type %in% c("i", "r", "o"))
        probVector[1] <- probVector[1] * ((1 / nbNewConfigurations)^(1 / parameters$nbVariable))
      }
      newModel[[currentParameter]][[idCurrentConfiguration]] <- probVector
    }
  }
  return (newModel)
}

printModel <- function (model)
{
  cat("# Model:\n")
  print(model)
}

restartConfigurations <- function (configurations, restart_ids, model, parameters,
                                   nbConfigurations)
{
  model_ids <- names(model[[1L]])
  restart_ids <- as.character(sort.int(as.integer(restart_ids)))
  not_in <- restart_ids %not_in% model_ids
  configurations <- configurations[configurations[[".ID."]] %in% restart_ids, c(".ID.", ".PARENT.")]
  restart_ids[not_in] <- configurations[[".PARENT."]][order(as.integer(configurations[[".ID."]]))][not_in]
  restart_ids <- as.character(unique(restart_ids))
  restart_ids <- restart_ids[!is.na(restart_ids)]
  
  back_factor <- nbConfigurations^(2 / parameters$nbVariable)
  second_factor <- (1 / nbConfigurations)^(1 / parameters$nbVariable)
  namesParameters <- parameters$names[!parameters$isFixed]
  for (param in namesParameters) {
    model_param <- model[[param]]
    irace.assert (all(restart_ids %in% names(model_param)), {
      cat("Param:", param, "\n")
      print(restart_ids)
      print(model)
      print(configurations[, c(".ID.", ".PARENT.")])
    })
    type <- parameters$types[[param]]
    if (type == "c") {
      for (id in restart_ids) {
        probVector <- model_param[[id]]
        probVector <- 0.9 * probVector + 0.1 * max(probVector)
        model[[param]][[id]] <- probVector / sum(probVector)
      }
    } else {
      if (type == "i" || type == "r") {
        value <- init.model.numeric(param, parameters)
      } else {
        irace.assert(type == "o")
        value <- (length(parameters$domain[[param]]) - 1) / 2
      }
      for (id in restart_ids) {
        # Bring back the value 2 iterations or to the second iteration value.
        stdev <- model_param[[id]][1L]
        model[[param]][[id]][1L] <- min(stdev * back_factor,
                                        value * second_factor)
      }
    }
  }
  model
}

# Initialise model in case of numerical variables.
# it retuns an array size 2, first number indicates the
# standard deviation and second the last known value (initially NA)
init.model.numeric <- function(param, parameters)
{
  # Dependent parameters define the standard deviation as
  # a portion of the size of the domain interval. In this case,
  # 0.5 indicates half of the interval, equivalent  to
  # (domain[2] - domain[1]) * 0.5
  if (parameters$isDependent[[param]]) {
    return(0.5)
  }

  transf <- parameters$transform[[param]]
  if (transf == "log") {
    domain <- c(0,1)
  } else {
    domain <- parameters$domain[[param]]
  }
  value <- (domain[2] - domain[1]) / 2.0
  irace.assert(is.finite(value))
  return(value)
}
