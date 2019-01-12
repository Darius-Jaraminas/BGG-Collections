rm(list = ls())

library("XML")
library("plyr")
library("dplyr")
library("httr")
library("reshape2")
library("gdata")

source("00_Functions.R")

url <- "https://www.boardgamegeek.com"

# read name mapping
fnm.nick <- "input/Nick Mapping.csv"
nick.map <- read.csv2(fnm.nick, check.names = FALSE, stringsAsFactors = FALSE)

collections <- NULL

nicks <- nick.map[, "ownernickname"]
for (i in nicks){
  # board games
  path <- paste0("xmlapi2/collection?username=", i,
                 "&stats=1&excludesubtype=boardgameexpansion")
  list.games <- CallBGGAPI(url = url, path = path)
  df.games <- CollectionListToDF(l = list.games)
  df.games[, "itemtype"] <- "Board Game"
  # expansions
  path <- paste0("xmlapi2/collection?username=", i,
                 "&stats=1&subtype=boardgameexpansion")
  list.expansions <- CallBGGAPI(url = url, path = path)
  df.expansions <- CollectionListToDF(l = list.expansions)
  if (!is.null(df.expansions)){
    df.expansions[, "itemtype"] <- "Expansion"
  }
  # all collection
  df.collection <- bind_rows(df.games, df.expansions)
  # rename to nicer names
  nm.from <- c(".attrs.objectid", "status.own")
  nm.to <- c("id", "own")
  df.collection <- rename.vars(df.collection, nm.from, nm.to, info = FALSE)
  # add owner nick
  df.collection[, "ownernickname"] <- i
  # subset columns as other info will be pulled by game
  core.names <- c("ownernickname", "id", "itemtype", "own")
  df.collection <- df.collection[, core.names]
  collections <- rbind(collections, df.collection)
}

fnm.coll <- "input/collections.csv"
write.csv2(collections, fnm.coll, row.names = FALSE)


