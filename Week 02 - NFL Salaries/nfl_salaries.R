# ===== Introduction =====
# Project: Tidy Tuesday - Week 2 - NFL Salaries
# By: Matthew Hendrickson
# Twitter: @mjhendrickson
# GitHub: https://github.com/mjhendrickson/Tidy-Tuesday
# Original source links: http://www.spotrac.com/rankings/
  #https://fivethirtyeight.com/features/running-backs-are-finally-getting-paid-what-theyre-worth/

# ===== Install needed packages =====
install.packages("tidyverse")
install.packages("readxl")
install.packages("scales")
install.packages("viridis")
install.packages("here")
#install.packages("RColorBrewer")


# ===== Load needed packages =====
library(tidyverse)
library(readxl)
library(scales)
library(viridis)
library(here)
#library(RColorBrewer)


# ===== Load & review data =====
nfl_salaries <- read_excel(here("Week 02 - NFL Salaries", "nfl_salaries.xlsx"))
View(nfl_salaries)
glimpse(nfl_salaries)


# ===== Data prep =====


# ===== Create plots =====

