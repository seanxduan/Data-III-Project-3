# STAT 8330 project #3
# David Reynolds
# Data prep

# Read in the data (just games, players, and plays for now)
setwd("~/Downloads/Documents/GitHub/Data-III-Project-3/Data")
games <- read.csv("games.csv")
players <- read.csv("players.csv")
plays <- read.csv("plays.csv")

### Data cleaning

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

## Address remaining NAs (STILL NEED TO FIX)
apply(plays, 2, function(x) any(is.na(x)))

# Calculate pct of plays with NAs in which a DPI occurred
dpi <- plays %>% 
  filter(isDefensivePI == "TRUE")

dpi_na <- dpi %>% 
  filter(is.na(preSnapVisitorScore) & is.na(preSnapHomeScore) & gameClock == "")

nrow(dpi_na)/nrow(dpi) # 92% of rows with NAs have isDefensivePI == TRUE

# Create unique play ID and order plays by it
plays$gameId <- as.character(plays$gameId)
plays$playId <- as.character(plays$playId)
plays$Id <- paste0(plays$gameId, plays$playId)
plays$Id <- as.numeric(plays$Id)

plays <- plays %>% 
  arrange(Id)

### Feature engineering

# Side binary
plays$side <- ifelse(plays$possessionTeam == plays$yardlineSide, "other", "own")
plays$side <- as.factor(plays$side)

# 
