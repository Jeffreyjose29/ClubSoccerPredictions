##############################################
## Project: Soccer Club Projections
## Process: Gathering Data & Transformation
## Developer: Jeffrey Jose
## Date: 17th February 2023
##############################################

# Package names
packages <- c("dplyr", "ggplot2", "hrbrthemes")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

setwd("C:/Users/jeffr/OneDrive/Desktop/Github Activities/ClubSoccerPredictions/Data")

global_rankings <- read.csv("spi_global_rankings.csv", header = TRUE)
matches <- read.csv("spi_matches.csv", header = TRUE)
matches_latest <- read.csv("spi_matches_latest.csv", header = TRUE)

# Filtering to the EPL
global_rankings <- global_rankings %>% filter(league == "Barclays Premier League")
matches <- matches %>% filter(league_id == 2411)
matches_latest <- matches_latest %>% filter(league_id == 2411)




