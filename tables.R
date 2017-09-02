# load libraries
library("plyr")
library("dplyr")
library("ggplot2")
library("R.utils")
library(RCurl)

# load data 
git.repo <- "https://raw.github.com/rivergrove/Join-the-dark-side-of-chess/master/"
file <- "lichess_rivergrove_2017-04-03.csv"
user_name = "rivergrove"
csv.df <- read.csv(paste0(git.repo, file), header=FALSE)
  
# create games table 
start_row <- which(csv.df == "[Event Rated game]"|csv.df =="[Event Casual game]")
end_row <- c(start_row[-1]-1, nrow(csv.df))

for (i in 1:length(start_row)){
  
  if (exists("games")){
    temp <- data.frame(t.data.frame(csv.df[start_row[i]:end_row[i],]))
    
    if (ncol(games) != ncol(temp)) {
      # print rows of different games variants with data columns that do not align with games
      print(paste("column count does not align for row", i))
      
      if(exists("misformatted.games")){
        misformatted.games <- rbind.fill(misformatted.games,temp)
      }else{
        misformatted.games <- temp
      }
      
    }else{
      games <- rbind(games, temp)
    }
    
  }else{
    games <- data.frame(t.data.frame(csv.df[start_row[1]:end_row[1],]))
  }
}

# clean data
names(games) <- c("game_type",
                  "url",
                  "date",
                  "white",
                  "black",
                  "result",
                  "white_rating",
                  "black_rating",
                  "move_count",
                  "variation",
                  "time_control",
                  "opening_code",
                  "opening_name",
                  "termination_type",
                  "moves")

# clarify nomenclature
games <- as.data.frame(apply(games, 2, function(y) gsub("\\[|\\]","",y)))
games$game_type <- gsub("Event | game","",games$game_type)
games$url <- gsub("Site https://lichess.org/","",games$url)
games$date <- gsub("Date ","",games$date)
games$result <- gsub("Result 1-0","win",games$result)
games$result <- gsub("Result 0-1","loss",games$result)
games$result <- gsub("Result 1/2-1/2","draw",games$result)
games$move_count <- gsub("PlyCount ","",games$move_count)
games <- games[-which(games$move_count == 0),]
games$variation <- gsub("Variant ","",games$variation)
games$time_control <- gsub("TimeControl ","",games$time_control)
games$opening_code <- gsub("ECO ","",games$opening_code)
games$opening_name <- gsub("Opening ","",games$opening_name)
games$termination_type <- gsub("Termination ","",games$termination_type)

# opponent rating vs my rating
games$temp_white <- games$white %in% paste("White",user_name)
games$temp_black <- games$black %in% paste("Black",user_name)
temp <- which(games$temp_black == TRUE)
games$opp_name[temp] <- as.character(games$white[temp])
games$my_rating[temp] <- as.character(games$black_rating[temp])
games$opp_rating[temp] <- as.character(games$white_rating[temp])  
temp <- which(games$temp_white == TRUE) 
games$opp_name[temp] <- as.character(games$black[temp])
games$my_rating[temp] <- as.character(games$white_rating[temp])
games$opp_rating[temp] <- as.character(games$black_rating[temp])

# clarify nomenclature cont.
games$white <- games$temp_white
games$opp_name <- gsub("White |Black ", "", games$opp_name)
games$my_rating <- gsub("WhiteElo |BlackElo ","",games$my_rating)
games$opp_rating <- gsub("WhiteElo |BlackElo ","",games$opp_rating)

# gameplay timing
temp <- data.frame(do.call('rbind', strsplit(as.character(games$time_control),'+',fixed=TRUE)))
names(temp) <- c("seconds_per_side", "seconds_increment")
games <- cbind(games,temp)  

# set data types

  # date
  games$date <- as.Date(gsub("\\.","-",games$date),format="%Y-%m-%d")
  games <- games[order(games$date,decreasing = FALSE),]
  
  # numeric
  games$my_rating <- as.numeric(games$my_rating)
  games$opp_rating <- as.numeric(games$opp_rating)
  games$move_count <- as.numeric(games$move_count)
  games$seconds_per_side <- as.numeric(as.character(games$seconds_per_side))
  games$seconds_increment <- as.numeric(as.character(games$seconds_increment))
  
  # character
  games$moves <- as.character(games$moves)

# subsetting and ordering
games <- games[!(names(games) %in% c("temp_black",
                                     "temp_white", 
                                     "black",
                                     "white_rating",
                                     "black_rating",
                                     "time_control"))]

games <- games[,c(7,1,2,3,5,4,12,13,14,15,16,6,10,9,8,11)]
games$id <- seq.int(nrow(games))
games <- subset(games, select = c(17,1:16))
games$sec <- games$seconds_per_side + games$seconds_increment * games$move_count/2  

# remove non-standard variations if desired
games <- games[games$variation == "Standard",]

# create moves table

for (i in 1:nrow(games)){
  
  # temp table for each game.id
  move.string <- unlist(strsplit(games$moves[i]," "))
  move.string <- move.string[-length(move.string)]
  move.string <- move.string[rep(c(FALSE, TRUE, TRUE), len = length(move.string))]
  temp <- data.frame(move.string[rep(c(TRUE,FALSE), len = length(move.string))])
  names(temp) <- "white"
  
  if(length(move.string) %% 2 == 1) {
    move.string <- c(move.string,NA)
  }
  
  temp$black <- move.string[rep(c(FALSE,TRUE), len = length(move.string))]
  temp$game.id <- i
  temp$move.count <- seq(1:nrow(temp))
  
  if(exists("moves")){
    moves <- rbind(moves, temp)
  }else{
    moves <- temp
  }
  
}

# move id
moves$id <- seq(1:nrow(moves))
moves <- moves[,c(5,3,1,2,4)]

# remove unneccessary columns and variables
games$moves <- NULL

rm(list=setdiff(ls(),c("csv.df",
                       "misformatted.games",
                       "games",
                       "moves")))
