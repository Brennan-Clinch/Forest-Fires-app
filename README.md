# Shiny app on forest fires

## Overview

This app will explore data on forest fires in Portugal in the northeast as well as use predictive analytics for predicting the burned area. Dataset is taken from the UCL Machine 
Learning repository. [The link to the dataset information is here](https://archive.ics.uci.edu/ml/datasets/Forest+Fires)

## Required packages

To use this app, the following packages are required:

`tidyverse` - R packages for data science in data manipulation and visualization
`shiny`- app framework
`readr` - read in data
`shinyWidgets`- additional functionality for Shiny
`caret`- machine learning
`DT`- additional functionality for Shiny
`rattle`-visualizing tree model

To install, I would run this code chunk:

```r
install.packages("tidyverse")
install.packages("readr")
install.packages("shiny")
install.packages("shinyWidgets")
install.packages("caret")
install.packages("DT")
install.packages("rattle")
```
