#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2020-04-19 12:40:57 x86_64-w64-mingw32 

# Rattle version 5.3.0 user 'c.gendron1'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 

# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2020-04-19 12:44:44 x86_64-w64-mingw32 

# Load a dataset from file.

# fname         <- "file:///C:/Users/c.gendron1/Desktop/Project_Repository/DAT640/Data/iris.csv" 
fname <- here::here("Data","iris.csv")
crs$dataset <- read.csv(fname,
                        na.strings=c(".", "NA", "", "?"),
                        strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2020-04-19 12:44:44 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=150 train=105 validate=22 test=23

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
  crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
  crs$test

# The following variable selections have been noted.

crs$input     <- c("X", "Sepal.Length", "Sepal.Width",
                   "Petal.Length", "Petal.Width")

crs$numeric   <- c("X", "Sepal.Length", "Sepal.Width",
                   "Petal.Length", "Petal.Width")

crs$categoric <- NULL

crs$target    <- "Species"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2020-04-19 12:44:48 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=150 train=105 validate=22 test=23

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
  crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
  crs$test

# The following variable selections have been noted.

crs$input     <- c("Sepal.Length", "Sepal.Width", "Petal.Length",
                   "Petal.Width")

crs$numeric   <- c("Sepal.Length", "Sepal.Width", "Petal.Length",
                   "Petal.Width")

crs$categoric <- NULL

crs$target    <- "Species"
crs$risk      <- NULL
crs$ident     <- "X"
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2020-04-19 12:45:00 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

# The 'Hmisc' package provides the 'describe' function.

library(Hmisc, quietly=TRUE)

# Generate a description of the dataset.

describe(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

# The 'basicStats' package provides the 'fBasics' function.

library(fBasics, quietly=TRUE)

# Generate a description of the numeric data.

lapply(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)][,c(1:4)], basicStats)

# The 'mice' package provides the 'md.pattern' function.

library(mice, quietly=TRUE)

# Generate a summary of the missing values in the dataset.

md.pattern(crs$dataset[,c(crs$input, crs$target)])

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for Sepal.Length

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Species=as.factor(Species)) %>%
  dplyr::select(Sepal.Length, Species) %>%
  ggplot2::ggplot(ggplot2::aes(x=Sepal.Length)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Species, colour=Species), alpha=0.55) +
  ggplot2::xlab("Sepal.Length\n\nRattle 2020-Apr-19 13:29:56 c.gendron1") +
  ggplot2::ggtitle("Distribution of Sepal.Length (sample)\nby Species") +
  ggplot2::labs(fill="Species", y="Density")

# Use ggplot2 to generate histogram plot for Sepal.Width

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Species=as.factor(Species)) %>%
  dplyr::select(Sepal.Width, Species) %>%
  ggplot2::ggplot(ggplot2::aes(x=Sepal.Width)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Species, colour=Species), alpha=0.55) +
  ggplot2::xlab("Sepal.Width\n\nRattle 2020-Apr-19 13:29:56 c.gendron1") +
  ggplot2::ggtitle("Distribution of Sepal.Width (sample)\nby Species") +
  ggplot2::labs(fill="Species", y="Density")

# Use ggplot2 to generate histogram plot for Petal.Length

# Generate the plot.

p03 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Species=as.factor(Species)) %>%
  dplyr::select(Petal.Length, Species) %>%
  ggplot2::ggplot(ggplot2::aes(x=Petal.Length)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Species, colour=Species), alpha=0.55) +
  ggplot2::xlab("Petal.Length\n\nRattle 2020-Apr-19 13:29:56 c.gendron1") +
  ggplot2::ggtitle("Distribution of Petal.Length (sample)\nby Species") +
  ggplot2::labs(fill="Species", y="Density")

# Use ggplot2 to generate histogram plot for Petal.Width

# Generate the plot.

p04 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Species=as.factor(Species)) %>%
  dplyr::select(Petal.Width, Species) %>%
  ggplot2::ggplot(ggplot2::aes(x=Petal.Width)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Species, colour=Species), alpha=0.55) +
  ggplot2::xlab("Petal.Width\n\nRattle 2020-Apr-19 13:29:56 c.gendron1") +
  ggplot2::ggtitle("Distribution of Petal.Width (sample)\nby Species") +
  ggplot2::labs(fill="Species", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04)
