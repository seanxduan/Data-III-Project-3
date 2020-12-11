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
plays$absoluteYardlineNumber <- ifelse(is.na(plays$absoluteYardlineNumber) == TRUE, 
                                       ifelse(plays$yardlineSide == plays$possessionTeam, 60 + (50 - plays$yardlineNumber), plays$yardlineNumber + 10), plays$absoluteYardlineNumber)

# Remove rows with NAs for preSnapHomeScore and preSnapVisitorScore
plays <- plays %>% 
  filter(!is.na(preSnapHomeScore) & !is.na(preSnapVisitorScore) & gameClock != "")

# Remove punts
plays <- plays[!grepl("P", plays$personnelO),]

# Replace penaltyCodes blanks
plays$penaltyCodes[plays$penaltyCodes == ""] <- "None"

## Feature engineering

# Absolute value score differential
plays$score_diff <- abs(plays$preSnapHomeScore - plays$preSnapVisitorScore)

# Side binary
plays$side <- ifelse(plays$possessionTeam == plays$yardlineSide, "other", "own")
plays$side <- as.factor(plays$side)

# Time remaining in game
library(lubridate)
library(stringr)
times <- plays$gameClock
times <- str_sub(times, end = -4)
times <- as.period(ms(times), unit = "sec")
times <- str_sub(times, end = -2)
times <- as.numeric(times)/60
plays$time_remaining <- times
plays$quarter <- as.factor(plays$quarter)
plays$time_remaining <- ifelse(plays$quarter == "1", plays$time_remaining + 55,
                               ifelse(plays$quarter == "2", plays$time_remaining + 40,
                                      ifelse(plays$quarter == "3", plays$time_remaining + 25,
                                             ifelse(plays$quarter == "4", plays$time_remaining + 10, plays$time_remaining))))

# Close game binary
plays$close_game <- ifelse(plays$time_remaining <= 15 & plays$score_diff <= 7, "1", "0")

# Penalty binary
plays$penalty <- ifelse(plays$penaltyCodes == "None", 0, 1)

# Number of DBs
plays$dbs <- str_sub(plays$personnelD, 13, 13)

# EPA binary
plays$epa_bi <- ifelse(plays$epa >= 0, "1", "0")

## Prepare data for modeling

# Fix variable types
plays$quarter <- as.factor(plays$quarter)
plays$down <- as.factor(plays$down)
plays$playType <- as.factor(plays$playType)
plays$offenseFormation <- as.factor(plays$offenseFormation)
plays$personnelO <- as.factor(plays$personnelO)
plays$personnelD <- as.factor(plays$personnelD)
plays$typeDropback <- as.factor(plays$typeDropback)
plays$passResult <- as.factor(plays$passResult)
plays$close_game <- as.factor(plays$close_game)
plays$penalty <- as.factor(plays$penalty)
plays$dbs <- as.numeric(plays$dbs)
plays$epa_bi <- as.factor(plays$epa_bi)

# Select and rename relevant columns
cor(plays$defendersInTheBox, plays$numberOfPassRushers)
cor(plays$offensePlayResult, plays$playResult)
dat <- plays %>% 
  select(quarter, down, "yards_to_go" = yardsToGo, "play_type" = playType, 
         "offensive_formation" = offenseFormation, "offensive_personnel" = personnelO, 
         "defenders_in_box" = defendersInTheBox, "pass_rushers" = numberOfPassRushers, 
         "defensive_personnel" = personnelD, "dropback_type" = typeDropback, 
         "absolute_yardline" = absoluteYardlineNumber,  "play_result" = offensePlayResult, 
         score_diff, side, time_remaining, close_game, penalty, dbs, epa_bi, "pass_result" = passResult)

# Save dat
write.csv(x = dat, file = "david_modeling.csv")
