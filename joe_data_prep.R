library(dplyr)
library(lubridate)
library(stringr)


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
