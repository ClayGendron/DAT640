library(tidyverse)
library(tidymodels)
library(readr)
library(rattle)
library(here)
library(ggvis)
library(ggraptR)

iris <- read.csv(here::here('Data','iris.csv'))

#Visual 1
ggplot(iris, aes(y=Sepal.Width, x=Sepal.Length)) + 
        geom_point(aes(colour=Species), stat="identity", position="jitter", alpha=0.5, size=4) + 
        theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + 
        scale_size(range=c(1, 4)) + 
        xlab("Sepal.Length") + 
        ylab("Sepal.Width")

#Visual 2

iris %>% ggvis(x=~Petal.Length) %>% 
  layer_densities(
    adjust = input_slider(.1, 2, value = 1, step = .1, label = "Bandwidth adjustment"),
    kernel = input_select(
      c("Gaussian" = "gaussian",
        "Epanechnikov" = "epanechnikov",
        "Rectangular" = "rectangular",
        "Triangular" = "triangular",
        "Biweight" = "biweight",
        "Cosine" = "cosine",
        "Optcosine" = "optcosine"),
      label = "Kernel")
  )
