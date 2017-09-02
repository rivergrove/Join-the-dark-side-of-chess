# load libraries
library(RCurl)
library("plyr")
library("ggplot2")

# inputs
git.repo <- "https://raw.github.com/rivergrove/Join-the-dark-side-of-chess/master/"
file <- "lichess_rivergrove_2017-04-03.csv"
user_name = "rivergrove"

# create data frame
df <- read.csv(paste0(git.repo, file), header=FALSE)
start_row <- which(df == "[Event Rated game]"|df =="[Event Casual game]")

if(exists("tabl")) rm("tabl")
for (i in 2:length(start_row))
  {
    if (exists("tabl")){
      temp <- data.frame(t.data.frame(df[start_row[i-1]:(start_row[i]-1),1]))
        if (ncol(tabl) != ncol(temp)) {
          print(i-1)
          print("column count does not align")
          tabl <- rbind(tabl, temp[1:ncol(tabl)])
        }else{
          tabl <- rbind(tabl, temp)
        }
    }else{
      tabl <- data.frame(t.data.frame(df[1:start_row[i]-1,1]))
    }
  }

# investigate games with misaligned columns

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

foo <- start_row-start_row[-1]
foo2 <- foo == Mode(foo)
foo2[length(foo2)] <- TRUE
foo3 <- start_row[!foo2]
foo4 <- which(foo2==FALSE)+1
foo5 <- start_row[foo4]

for (i in 1:length(foo3)){
  if(exists("misformatted.games")){
    misformatted.games <- rbind(misformatted.games,data.frame(df[foo3[i]:(foo5[i]-1),]))
  }else{
    misformatted.games <- data.frame(df[foo3[i]:(foo5[i]-1),])
  }
}

# clean data table

  names(tabl) <- c("game_type", 
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
  
tabl <- as.data.frame(apply(tabl, 2, function(y) gsub("\\[|\\]","",y)))
tabl$game_type <- gsub("Event | game","",tabl$game_type)
tabl$url <- gsub("Site http://lichess.org/","",tabl$url)
tabl$date <- gsub("Date ","",tabl$date)
tabl$result <- gsub("Result 1-0","win",tabl$result)
  tabl$result <- gsub("Result 0-1","loss",tabl$result)
  tabl$result <- gsub("Result 1/2-1/2","draw",tabl$result)
tabl$move_count <- gsub("PlyCount ","",tabl$move_count)
  tabl <- tabl[-which(tabl$move_count == 0),]
tabl$variation <- gsub("Variant ","",tabl$variation)
tabl$time_control <- gsub("TimeControl ","",tabl$time_control)
tabl$opening_code <- gsub("ECO ","",tabl$opening_code)
tabl$opening_name <- gsub("Opening ","",tabl$opening_name)
tabl$termination_type <- gsub("Termination ","",tabl$termination_type)

tabl$temp_white <- tabl$white %in% paste("White",user_name)
  tabl$temp_black <- tabl$black %in% paste("Black",user_name)
temp <- which(tabl$temp_white == TRUE) 
  tabl$opp_name[temp] <- as.character(tabl$black[temp])
  tabl$my_rating[temp] <- as.character(tabl$white_rating[temp])
  tabl$opp_rating[temp] <- as.character(tabl$black_rating[temp])
temp <- which(tabl$temp_black == TRUE)
  tabl$opp_name[temp] <- as.character(tabl$white[temp])
  tabl$my_rating[temp] <- as.character(tabl$black_rating[temp])
  tabl$opp_rating[temp] <- as.character(tabl$white_rating[temp])
tabl$white <- tabl$temp_white
tabl$opp_name <- gsub("White |Black ", "", tabl$opp_name)
tabl <- tabl[!(names(tabl) %in% c("temp_black","temp_white", "black","white_rating","black_rating"))]
tabl <- tabl[,c(1,2,3,4,13,14,15,5,6,7,8,9,10,11,12)]
tabl$my_rating <- gsub("WhiteElo |BlackElo ","",tabl$my_rating)
tabl$opp_rating <- gsub("WhiteElo |BlackElo ","",tabl$opp_rating)

temp <- gsub("\\.","-",tabl$date)
tabl <- tabl[order(as.Date(temp, format="%Y-%m-%d"),decreasing = FALSE),]

tabl$date <- as.Date(gsub("\\.","-",tabl$date),format="%Y-%m-%d")
tabl$my_rating <- as.numeric(tabl$my_rating)
tabl$opp_rating <- as.numeric(tabl$opp_rating)
tabl$move_count <- as.numeric(tabl$move_count)

temp <- data.frame(do.call('rbind', strsplit(as.character(tabl$time_control),'+',fixed=TRUE)))
tabl <- cbind(tabl,temp)
names(tabl)[16] <- "seconds_per_side"
names(tabl)[17] <- "seconds_increment" 
tabl$time_control <- NULL
tabl$seconds_per_side <- as.numeric(as.character(tabl$seconds_per_side))
tabl$seconds_increment <- as.numeric(as.character(tabl$seconds_increment))
tabl <- tabl[,c(1,2,3,4,5,6,7,8,9,15,16,10,11,12,13,14)]
tabl$id <- seq.int(nrow(tabl))
tabl <- subset(tabl, select = c(17,1:16))

rm(list=setdiff(ls(),"tabl"))


  