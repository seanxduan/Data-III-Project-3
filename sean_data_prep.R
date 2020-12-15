# STAT 8330 project #3
# Sean D
# Data prep

# Read in the data (just games, players, and plays for now)
games <- read.csv("nfl-big-data-bowl-2021/games.csv")
players <- read.csv("nfl-big-data-bowl-2021/players.csv")
plays <- read.csv("nfl-big-data-bowl-2021/plays.csv")

#this is the player/directional tracking data, can't do much w/ it as is
week1<-read.csv("nfl-big-data-bowl-2021/week1.csv")

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

## fixing presnap visitor score and presnap home score
table(plays$preSnapHomeScore)
noscore<-(is.na(plays$preSnapHomeScore))
plays_noscore<-(plays[noscore,])
#looking at our elements w/ noscore... we see a few things in common
#very common to have a penalty (over 99% of the cases have one!)
#no game-time or clock-time (wasn't sussed out in the earlier line of code by david?)


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

#quick search for outliers
summary(plays)
table(plays$quarter)
table(plays$yardsToGo)
hist(plays$playResult)
#Should we account for outliers in our playresult, yard to go, etc for us?

### Feature engineering

# Side binary
plays$side <- ifelse(plays$possessionTeam == plays$yardlineSide, "other", "own")
plays$side <- as.factor(plays$side)

#Garbage time feature engineering - ?
class(plays$gameClock)
table(plays$gameClock)
#we should only look @ the fourth quarter then?
#have gameclock and multiply by 15*quarter remaining

plays_4qtr <-plays[which(plays$quarter ==4),]
table(is.na(plays_4qtr))
plays_4qtr<-plays_4qtr[which(complete.cases(plays_4qtr)),]
#generate a ratio of times to score differential
library(lubridate)
library(stringr)
time<-plays_4qtr$gameClock


hat<-str_sub(time, end =-4)

res<-ms(hat)
time_s<-seconds(res)
time_s<-str_sub(time_s, end=-2)
time_s<-as.data.frame(time_s)
time_s<-as.numeric(unlist(time_s))
plays_4qtr$time_seconds<-time_s

str(plays_4qtr)

#doesn't seem like there is a big fx of raw time to epa, but we aren't accounting for score differential
plot(plays_4qtr$time_seconds, plays_4qtr$epa)

plays_4qtr$differential<-plays_4qtr$preSnapHomeScore-plays_4qtr$preSnapVisitorScore

#don't see anything here either
plot(plays_4qtr$differential, plays_4qtr$epa)

plays_4qtr$gtime<-(abs(plays_4qtr$differential)/plays_4qtr$time_seconds)

plot(plays_4qtr$gtime, plays_4qtr$epa)
#remove inf values
plays_4qtr<-plays_4qtr[is.finite(plays_4qtr$gtime), ]



#we see significance in our gtime variable!
#perhaps exile the outliers??
#consider nlin models?

#perhaps a ratio is no good, lets define a hard cutoff?
#greater than 1 score differential per 2 minutes remaining?
boxplot(plays_4qtr$gtime)
#3 cat- 0, nonzero, large
#deal w/ the zero inflation?

#consider a binary model as well?

boxplot(plays_4qtr$gtime)
summary(plays_4qtr$gtime)
#3 part variable, 0, up to the median, and above median?
plays_4qtr$mg<-cut(plays_4qtr$gtime, c(-1,0,0.027,21))
levels(plays_4qtr$mg)
levels(plays_4qtr$mg) <- c("zero","small","large")

#new model w/ the trinary output
m3_gtime<-lm(epa~mg, data = plays_4qtr)
summary(m3_gtime)

#lets try binary output
plays_4qtr$mg2<-cut(plays_4qtr$gtime, c(-1,0.026,21))
levels(plays_4qtr$mg2)
levels(plays_4qtr$mg2) <- c("small","large")

m4_gtime<-lm(epa~mg2, data = plays_4qtr)
summary(m4_gtime)
#we clearly see a difference here in our cutoff! perhaps choose diff cutoffs for the trinary?

#lets do some basic graphs work
gt_1<-ggplot(data = plays_4qtr, aes(x =, y= )
