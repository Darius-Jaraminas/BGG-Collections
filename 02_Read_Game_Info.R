# This file is for gettinh all data for ech game and formating output nicely

library("dplyr")
library("tidyr")
require("httr")
require("XML")

source("00_Functions.R")
url <- "https://www.boardgamegeek.com"

# read collections
fnm.collections <- "input/Collections.csv"
collections <- read.csv(fnm.collections, check.names = FALSE, stringsAsFactors = FALSE)
collections <- collections[collections[, "own"] == 1, ]
# read manual max player adjustments
fnm.adj <- paste0("input/Max Player Adjustment.csv")
adj <- read.csv(fnm.adj, check.names = FALSE, stringsAsFactors = FALSE)
# read meta file for columns names and order
fnm.meta <- "input/Meta.csv"
meta <- read.csv(fnm.meta, check.names = FALSE, stringsAsFactors = FALSE)

all.games <- list()
nicks <- unique(collections[, "ownernickname"])

# pull all games data for each nickname
for (i in 1:length(nicks)){
  cat(i, ":", nicks[i], "\n")
  id <- as.numeric(collections[collections[, "ownernickname"] == nicks[i], "id"])
  path <- paste0("xmlapi2/thing?id=", paste0(id, collapse = ","), "&stats=1")
  games <- CallBGGAPI(url = url, path = path)
  games <- games[names(games) %in% "item"]
  games <- lapply(games, ProcessGame)
  games <- lapply(games, SummarisePolls)
  games <- lapply(games, GatherGameInfo)
  all.games[[i]] <- bind_rows(games)
  Sys.sleep(10)
}
all.games <- bind_rows(all.games)

# add owner nick names
collections[, "own"] <- NULL
all.games[, "id"] <- as.numeric(all.games[, "id"])
all.games2 <- left_join(collections, all.games, by = "id")

# manually adjust max player count
all.games2 <- AdjustPlayerCount(dt = all.games2, adj = adj)

# add short long column
all.games2[, "gamelength"] <- "Short"
all.games2[as.numeric(all.games2[, "playingtime"]) >= 90, "gamelength"] <- "Long"

# rename variables, keep only output variables and order variables
all.games3 <- MetaPrep(all.games2, meta = meta)

# check output dir
if (!dir.exists("output")){
  dir.create("output")
}
# write output data
fnm.out <- "output/All Board Games.csv"
write.csv(all.games3, fnm.out, row.names = FALSE)
