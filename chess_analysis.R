# Write tables
  source("/Users/anthonyolund/Documents/r/chess/chess_tables.R")

# win, loss, draw
  
  # chi square test
  chi.test.games <- function(x){
    
    white.summary <- data.frame(count(x[x$white == TRUE, ], result))
    names(white.summary)[2] <- "freq"
    white.summary$percent <- round(white.summary$freq/sum(white.summary$freq),3)
    white.summary$color <- "white"
    
    black.summary <- data.frame(count(x[x$white == FALSE, ], result))
    names(black.summary)[2] <- "freq"
    black.summary$percent <- round(black.summary$freq/sum(black.summary$freq),3)
    black.summary$color <- "black"
    
    chi.df <- merge(white.summary[,1:2], black.summary[,1:2], by="result")
    names(chi.df)[2:3] <- c("white","black")
    color.p_value <- chisq.test(chi.df[,2:3])$p.value
    
    color.p_value
    
  }
  
  # pie graphs
  pie.games <- function(x){
    
    white.summary <- data.frame(count(x[x$white == TRUE, ], result))
    names(white.summary)[2] <- "freq"
    white.summary$percent <- round(white.summary$freq/sum(white.summary$freq),3)
    white.summary$color <- "white"
    
    black.summary <- data.frame(count(x[x$white == FALSE, ], result))
    names(black.summary)[2] <- "freq"
    black.summary$percent <- round(black.summary$freq/sum(black.summary$freq),3)
    black.summary$color <- "black"

    win.percent <- rbind(white.summary, black.summary)
    win.percent$color <- factor(win.percent$color, levels = c("white", "black"))
    win.percent <- win.percent %>% group_by(color) %>% mutate(pos = 1 - (cumsum(percent) - percent/2))
    win.percent$label <- paste0(win.percent$percent * 100, "%")
    win.percent[win.percent$result == "draw", 6] <- ""
    
    ggplot(data = win.percent, aes(x = "", y=percent, fill=result)) +
      geom_bar(width=10, stat = "identity") +
      theme_void() +
      geom_text(aes(x= factor(1), y=pos, label = label), size=3.5) +
      coord_polar("y") +
      scale_fill_manual(values = c("grey","pink","lightgreen")) +
      facet_grid(facets=. ~ color) +
      ggtitle("Win % By Color\n") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab('') + ylab('') +
      theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

  }
  
  # games
    chi.test.games(games)
    pie.games(games)
    nrow(games)
  
  # playing comp vs playing human
  
    # comp games
    comp.games <- games[grep("lichess AI", games$opp_name),]
    
      chi.test.games(comp.games)
      pie.games(comp.games)
      nrow(comp.games)
    
    # human games
    human.games <- games[grepl("lichess AI", games$opp_name) == FALSE,]
    
      chi.test.games(human.games)
      pie.games(human.games)
      nrow(human.games)
  
    # strong humans 
    strong.humans <- human.games[human.games$opp_rating > 2000 &
                                   is.na(human.games$opp_rating) == FALSE,]
    
      chi.test.games(strong.humans)
      pie.games(strong.humans)
      nrow(strong.humans)
  
    # weak humans
    weak.humans <- human.games[human.games$opp_rating < 1600 &
                                   is.na(human.games$opp_rating) == FALSE,]
    
      chi.test.games(weak.humans)
      pie.games(weak.humans)
      nrow(weak.humans)
  
# time
      
      win.by.min <- data.frame(count(subset(games, is.na(sec) == FALSE), round_any((sec/60), 1, f = ceiling), result))
      names(win.by.min)[1] <- "min"
      
      ggplot(win.by.min ,aes(x = min, y = n, fill = result)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("grey","pink","lightgreen")) +
        theme(axis.ticks = element_blank(),
              axis.text.y = element_blank())

      
     