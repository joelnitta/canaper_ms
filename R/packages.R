library(canaper)
library(future)
library(tidyverse)
library(assertr)
library(glue)
library(conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
