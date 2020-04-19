library(tidyverse)
library(tidymodels)
library(readr)
library(rattle)
library(here)

# data pull

# data("iris")
# write.csv(as_data_frame(iris),here::here("Data","iris.csv"))
iris <- read.csv(here::here("Data","iris.csv"))[,-1]
rattle()

# eda

names(iris)
dim(iris)
summary(iris)
