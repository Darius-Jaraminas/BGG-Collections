rm(list = ls())

library("XML")
library("plyr")
library("dplyr")
library("httr")
library("reshape2")
library("gdata")

source("00_Functions.R")
url <- "https://www.boardgamegeek.com"

fnm.collections <- "input/collections.csv"
collections <- read.csv2(fnm.collections, check.names = FALSE, stringsAsFactors = FALSE)
collections <- collections[collections[, "own"] == 1, ]
all.games <- list()

for (i in 1:nrow(collections)){
  Sys.sleep(1)
  id <- as.numeric(collections[i, "id"])
  path <- paste0("xmlapi2/thing?id=", id, "&stats=1")
  game <- CallBGGAPI(url = url, path = path)
  game <- ProcessGame(l = game)
  game <- SummarisePolls(l = game)
  game <- cbind(game$info, game$statistics, game$polls.summary)
  cat(i, ":", game[, "name"], "\n")
  all.games[[i]] <- game
}
all.games <- bind_rows(all.games)

columns.drop <- c("thumbnail", "image", "description", "median")
all.games <- remove.vars(all.games, columns.drop, info = FALSE)

collections[, "own"] <- NULL
all.games2 <- merge(collections, all.games, all.y = TRUE, by = "id")

fnm.nick <- "input/Nick Mapping.csv"
nick.map <- read.csv2(fnm.nick, check.names = FALSE, stringsAsFactors = FALSE)
all.games2 <- merge(nick.map, all.games2, all.x = TRUE)

nm.from <- c("ownernickname", "ownername", "id", "itemtype", "yearpublished",
             "minplayers", "maxplayers", "playingtime", "minplaytime",
             "maxplaytime", "minage", "name", "usersrated", "average", "bayesaverage",
             "stddev", "owned", "trading", "wanting", "wishing", "numcomments",
             "numweights", "averageweight", "best.numplayers.community",
             "recommended.numplayers.community", "min.age.community",
             "average.min.age.community", "language.community")
nm.to <- c("Owner Nickname", "Owner Name", "Game ID", "Item Type", "Year Published",
           "Min Players", "Max Players", "Playing Time", "Min Play Time",
           "Max Play Time", "Min Age", "Game Name", "Users Rated", "Average Rating",
           "Bayes Average Rating", "Standard Deviation", "Users Own", "Users Trading",
           "Users Wanting", "Users Wishing", "Number of Comments",
           "Number of Complexity Weights", "Average Complexity Weight",
           "Community - Best Number of Players",
           "Community - Recommended Number of Players", "Community - Min Age",
           "Community - Average Min Age", "Community - Language Dependency")
all.games2 <- rename.vars(all.games2, nm.from, nm.to, info = FALSE)

# adjust max player count
fnm.adj <- paste0("input/Max Player Adjustment.csv")
adj <- read.csv(fnm.adj, check.names = FALSE, stringsAsFactors = FALSE)
all.games2 <- AdjustPlayerCount(dt = all.games2, adj = adj)

# add short long column
all.games2[, "Game Length"] <- "Short"
all.games2[as.numeric(all.games2[, "Playing Time"]) >= 90, "Game Length"] <- "Long"


name.order <- c("Owner Name", "Owner Nickname", "Game Name","Item Type", "Game Length",
                "Community - Best Number of Players",
                "Community - Recommended Number of Players",
                "Min Players", "Max Players",
                "Playing Time", "Min Play Time", "Max Play Time",
                "Min Age", "Community - Min Age", "Community - Average Min Age",
                "Average Complexity Weight", "Number of Complexity Weights",
                "Year Published", "Board Game Rank", "Average Rating",
                "Bayes Average Rating", "Users Rated",
                # "Rating Standard Deviation",
                "Users Own", "Users Trading",
                "Users Wanting", "Users Wishing", "Number of Comments",
                "Community - Language Dependency")
nm.rank <- grep("Rank", colnames(all.games2), value = TRUE)
nm.rank <- setdiff(nm.rank, "Board Game Rank")
nm.rating <- gsub("Rank", "Bayes Average", nm.rank)
all.games3 <- all.games2[, c(name.order)]

fnm.out <- "output/All Board Games (ze team).csv"
write.csv(all.games3, fnm.out, row.names = FALSE)
