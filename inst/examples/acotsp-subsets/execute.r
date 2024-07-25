#install.packages("irace")
library("irace")
scenario <- readScenario(filename = "scenario.txt", scenario = defaultScenario())

irace.main(scenario = scenario)
