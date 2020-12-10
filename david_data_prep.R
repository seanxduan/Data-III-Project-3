# STAT 8330 project #3
# David Reynolds
# Data prep

# Read in the data
setwd("~/Downloads/Documents/GitHub/Data-III-Project-3/Data")
games <- read.csv("games.csv")
players <- read.csv("players.csv")
plays <- read.csv("plays.csv")

## Data cleaning

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

# Replace NAs for absoluteYardlineNumber
plays_yardline <- plays %>% 
  select(yardlineSide, yardlineNumber, absoluteYardlineNumber)

plays$absoluteYardlineNumber <- ifelse(is.na(plays$absoluteYardlineNumber) == TRUE, ifelse(plays$yardlineSide == plays$possessionTeam, 60 + (50 - plays$yardlineNumber), plays$yardlineNumber + 10), plays$absoluteYardlineNumber)

# Remove rows with NAs for preSnapHomeScore and preSnapVisitorScore
plays <- plays %>% 
  filter(!is.na(preSnapHomeScore) & !is.na(preSnapVisitorScore) & gameClock != "")

# Remove isDefensivePI
plays$isDefensivePI <- NULL

# Fix gameClock
library(lubridate)
library(stringr)
time <- plays$gameClock
hat <- str_sub(time, end = -4)
res <- ms(hat)
time_s <- minutes(res)

time_s<-str_sub(time_s, end=-2)
time_s<-as.data.frame(time_s)
time_s<-as.numeric(unlist(time_s))

## Feature engineering

# Absolute value score differential
plays$score_diff <- abs(plays$preSnapHomeScore - plays$preSnapVisitorScore)

# Remove preSnapHomeScore and preSnapVisitorScore
plays$preSnapHomeScore <- NULL
plays$preSnapVisitorScore <- NULL

# Side binary
plays$side <- ifelse(plays$possessionTeam == plays$yardlineSide, "other", "own")
plays$side <- as.factor(plays$side)

# Fix variable types
