library(dplyr)
library(lubridate)
library(stringr)
library(lubridate)


#Getting Data
games = read.csv("c:/Users/JoeCo/Desktop/NFLData/games.csv")
players = read.csv("c:/Users/JoeCo/Desktop/NFLData/players.csv")
plays = read.csv("c:/Users/JoeCo/Desktop/NFLData/plays.csv")


#Weeks
week1 = read.csv("c:/Users/JoeCo/Desktop/NFLData/week1.csv")
week2 = read.csv("c:/Users/JoeCo/Desktop/NFLData/week2.csv")
week3 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week3.csv")
week4 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week4.csv")
week5 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week5.csv")
week6 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week6.csv")
week7 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week7.csv")
week8 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week8.csv")
week9 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week9.csv")
week10 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week10.csv")
week11 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week11.csv")
week12 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week12.csv")
week13 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week13.csv")
week14 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week14.csv")
week15 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week15.csv")
week16 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week16.csv")
week17 = read.csv("c:/Users/JoeCo/Desktop/NFLData/Week17.csv")

#Combining all data
FullData = rbind(week1, week2, week3, week4, week5, week6, week7, week8, week9, week10, week11, week12, week13,
                 week14, week15, week16, week17)


#Cleaning Height
players$height = as.character(players$height)

players$NewHeight = ifelse(substr(players$height, 2,2) == "-", (as.numeric(substr(players$height, 1, 1)) * 12) + 
                             as.numeric((substr(players$height, 3, ifelse(nchar(players$height) == 3, 3, 4)))), as.numeric(players$height))

#Adding Height to week data
FullData$Height = players[match(FullData$nflId, players$nflId), 9]



#Adding weight to week data
FullData$Weight = players[match(FullData$nflId, players$nflId), 3]


#Cleaing Age
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}


players$birthDate = as.character(players$birthDate)
players$birthDate = str_replace_all(players$birthDate, "/", "-")
players$NewBirthDate = ifelse(substr(players$birthDate, 3, 3) == "-", 
                              paste(substr(players$birthDate, 7, 10), "-", substr(players$birthDate, 1, 2), "-", 
                                    substr(players$birthDate, 4, 5), sep = ""), as.character(players$birthDate))

players$Age = as.numeric(round(age(players$NewBirthDate, floor = F), 3))

#Adding Age to week data 
FullData$Age = players[match(FullData$nflId, players$nflId), 8]

head(FullData)

#Adding new variable to group by
FullData$GamePlay = paste(FullData$gameId, FullData$playId, sep = "")
plays$GamePlay = paste(plays$gameId, plays$playId, sep = "")

#Need to determine if theyre offense of defense
Offense = c("FB", "HB", "K", "NT", "P", "QB", "RB", "TE", "WR", "LS", "NT")
players$Side = ifelse(is.element(players$position, Offense) == T, "Off", "Def")

#Add side to Full Data
FullData$Side = players[match(FullData$nflId, players$nflId), 11]


#Get averages on both sides of the ball
#One thing is that they don't have any offensive or defesive lineman listed on all of these plays
AveragesOff = data.frame(FullData %>%
  filter(Side == "Off") %>%
  group_by(GamePlay) %>%
  summarise(
    AvgWeightOff = mean(Weight),
    AvgHeightOff = mean(Height), 
    AvgAgeOff = mean(Age)
  ))
head(Averages)

AveragesDef = data.frame(FullData %>%
  filter(Side == "Def") %>%
  group_by(GamePlay) %>%
  summarise(
    AvgWeightDef = mean(Weight), 
    AvgHeightDef = mean(Height),
    AvgAgeDef = mean(Age)
  ))
head(AveragesDef)



#Add offensive Variables to Plays dataset
plays$AvgWeightOff = AveragesOff[match(plays$GamePlay, AveragesOff$GamePlay), 2]
plays$AvgHeightOff = AveragesOff[match(plays$GamePlay, AveragesOff$GamePlay), 3]
plays$AvgAgeOff = AveragesOff[match(plays$GamePlay, AveragesOff$GamePlay), 4]

#Add defensive variables to plays dataset
plays$AvgWeightDef = AveragesDef[match(plays$GamePlay, AveragesDef$GamePlay), 2]
plays$AvgHeightDef = AveragesDef[match(plays$GamePlay, AveragesDef$GamePlay), 3]
plays$AvgAgeDef = AveragesDef[match(plays$GamePlay, AveragesDef$GamePlay), 4]


head(plays)

#Fix the few missing rows

mean(plays$AvgWeightOff, na.rm = T)
plays$AvgWeightOff[is.na(plays$AvgWeightOff)] = 218.3606

mean(plays$AvgWeightDef, na.rm = T)
plays$AvgWeightDef[is.na(plays$AvgWeightDef)] = 214.1209

mean(plays$AvgHeightOff, na.rm = T)
plays$AvgHeightOff[is.na(plays$AvgHeightOff)] = 73.639

mean(plays$AvgHeightDef, na.rm = T)
plays$AvgHeightDef[is.na(plays$AvgHeightDef)] = 72.503

mean(plays$AvgAgeOff, na.rm = T)
plays$AvgAgeOff[is.na(plays$AvgAgeOff)] = 28.965

mean(plays$AvgAgeDef, na.rm = T)
plays$AvgAgeDef[is.na(plays$AvgAgeDef)] = 28.597





#David Data Prep

apply(games, 2, function(x) any(is.na(x)))
apply(players, 2, function(x) any(is.na(x)))
apply(plays, 2, function(x) any(is.na(x)))

# See which rows have NAs
library(dplyr)
plays_na <- plays %>% 
  filter(is.na(defendersInTheBox) | is.na(numberOfPassRushers) | is.na(preSnapVisitorScore) | 
           is.na(preSnapHomeScore) | is.na(absoluteYardlineNumber | is.na(AvgWeightOff) 
                                           | is.na(AvgWeightDef) | is.na(AvgAgeOff) | is.na(AvgAgeDef)
                                          | is.na(AvgHeightOff) | is.na(AvgHeightDef)))

# Replace NAs for defendersInTheBox
mean(plays$defendersInTheBox, na.rm = TRUE)
plays$defendersInTheBox[is.na(plays$defendersInTheBox)] <- 6

# Replace NAs for numberOfPassRushers
mean(plays$numberOfPassRushers, na.rm = TRUE)
plays$numberOfPassRushers[is.na(plays$numberOfPassRushers)] <- 4

# Replace NAs for absoluteYardlineNumber
plays$yardlineSide = as.character(plays$yardlineSide)
plays$possessionTeam = as.character(plays$possessionTeam)
plays$absoluteYardlineNumber <- ifelse(is.na(plays$absoluteYardlineNumber) == TRUE, 
                                       ifelse(plays$yardlineSide == plays$possessionTeam, 60 + (50 - plays$yardlineNumber), plays$yardlineNumber + 10), plays$absoluteYardlineNumber)


# Remove rows with NAs for preSnapHomeScore and preSnapVisitorScore
plays <- plays %>% 
  filter(!is.na(preSnapHomeScore) & !is.na(preSnapVisitorScore) & gameClock != "")

# Remove punts
plays <- plays[!grepl("P", plays$personnelO),]

# Replace penaltyCodes blanks
plays$penaltyCodes = as.character(plays$penaltyCodes)
plays$penaltyCodes = ifelse(is.na(plays$penaltyCodes) == T, "None", as.character(plays$penaltyCodes))
plays$penaltyCodes = as.factor(plays$penaltyCodes)

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

## More data prep

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
         score_diff, side, time_remaining, close_game, penalty, dbs, epa_bi, "pass_result" = passResult,
         AvgWeightOff, AvgWeightDef, AvgHeightOff, AvgHeightDef, AvgAgeOff, AvgAgeDef)


apply(dat, 2, function(x) any(is.na(x)))

train = sample(nrow(plays), .80*nrow(plays), replace = F)
NFL.train = plays[train, ]
NFL.test = plays[-train, ]

write.csv(NFL.train, "c:/Users/JoeCo/Desktop/NFL_Train.csv")
write.csv(NFL.test, "c:/Users/JoeCo/Desktop/NFL_Test.csv")




