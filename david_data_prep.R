# STAT 8330 project #3
# David Reynolds
# Data exploration

# Read in the data (just games, players, and plays for now)
setwd("~/Downloads/Documents/GitHub/Data-III-Project-3/Data")
games <- read.csv("games.csv")
players <- read.csv("players.csv")
plays <- read.csv("plays.csv")

# Check for NAs
apply(games, 2, function(x) any(is.na(x)))
apply(players, 2, function(x) any(is.na(x)))
apply(plays, 2, function(x) any(is.na(x)))

# See which rows have NAs
library(dplyr)
plays_na <- plays %>% 
  filter(is.na(defendersInTheBox) | is.na(numberOfPassRushers) | is.na(preSnapVisitorScore) | 
           is.na(preSnapHomeScore) | is.na(absoluteYardlineNumber))

# Replace NAs for defendersInTheBox
mean(plays$defendersInTheBox, na.rm = TRUE)
plays$defendersInTheBox[is.na(plays$defendersInTheBox)] <- 6

# Replace NAs for numberOfPassRushers
mean(plays$numberOfPassRushers, na.rm = TRUE)
plays$numberOfPassRushers[is.na(plays$numberOfPassRushers)] <- 4

# Replace NAs for preSnapVisitorScore

# Replace NAS for preSnapHomeScore

# Replace NAs for absoluteYardlineNumber
plays_yardline <- plays %>% 
  select(yardlineSide, yardlineNumber, absoluteYardlineNumber)

plays_na$absoluteYardlineNumber <- ifelse(is.na(plays_na$absoluteYardlineNumber) == TRUE, ifelse(plays_na$yardlineSide == plays_na$possessionTeam, 60 + (50 - plays_na$yardlineNumber), plays_na$yardlineNumber + 10), plays_na$absoluteYardlineNumber)
