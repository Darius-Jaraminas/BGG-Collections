CastRanks <- function(collection, nm.ranks){
  ranks <- collection[, nm.ranks]
  ranks[ranks[, nm.ranks[3]] == "Not Ranked", nm.ranks[3]] <- NA
  ranks[, nm.ranks[3]] <- as.numeric(ranks[, nm.ranks[3]])
  f <- as.formula(paste0(nm.ranks[1], "~", nm.ranks[2]))
  ranks <- dcast(ranks, f, value.var = nm.ranks[3], fun.aggregate = mean)
  ranks[apply(ranks, 2, is.nan)] <- NA
  return(ranks)
}

CallBGGAPI <- function(url, path){
  require("httr")
  require("XML")
  api.result <- GET(url = url, path = path)
  if (api.result$status_code == 202){
    Sys.sleep(10)
    api.result <- GET(url = url, path = path)
  }
  if (api.result$status_code %in% c(503, 429)){
    while(api.result$status_code %in% c(503, 429)){
      Sys.sleep(3)
      api.result <- GET(url = url, path = path)
    }
  }
  if (api.result$status_code != 200){
    stop(paste0("The API call did not return results! ",
                "Status code: ", api.result$status_code))
  }
  xml.content <- rawToChar(api.result$content)
  list.result <- xmlToList(xml.content)
  return(list.result)
}



CollectionListToDF <- function(l){
  if (l[[".attrs"]]["totalitems"] != 0){
    l[[".attrs"]] <- NULL
    df <- lapply(l, function(x){
      data.frame(t(unlist(x)), stringsAsFactors = FALSE)
    })
    df <- bind_rows(df)
  } else{
    df <- NULL
  }
  return(df)
}

ProcessGame <- function(l, drop = c("names", "links")){
  # put links in to nice data frame
  link <- lapply(l$item[names(l$item) == "link"], function(x){
    data.frame(t(x), stringsAsFactors = FALSE)
  })
  link <- bind_rows(link)
  l$item[names(l$item) == "link"] <- NULL
  l$item$link <- link
  if ("links" %in% drop){
    l$item$link <- NULL
    # message("links have been droped!")
  }
  # put names in to nice data frame
  name <- lapply(l$item[names(l$item) == "name"], function(x){
    data.frame(t(x), stringsAsFactors = FALSE)
  })
  name <- bind_rows(name)
  l$item[names(l$item) == "name"] <- NULL
  l$item$name <- name
  if ("names" %in% drop){
    if (!"value" %in% colnames(l$item$name)){
      browser()
    }
    l$item$name <- l$item$name[l$item$name[, "type"] == "primary", "value"]
    # message("names have been droped!")
  }
  # put polls in to nice data frame
  polls <- list()
  w.polls <- which(names(l$item) == "poll")
  
  for (i in w.polls){
    poll.i <- l$item[[i]]
    # exception for number of players as the poll is of different format
    is.nr.of.players <- poll.i[[".attrs"]]["title"] == "User Suggested Number of Players"
    if (is.nr.of.players){
      w.results <- which(names(poll.i) == "results")
      poll.df <- list()
      for (j in w.results){
        poll.df[[j]] <- ProcessNumberOfPlayersPollResult(r = poll.i[[j]])
      } 
      poll.df <- bind_rows(poll.df)
      # more compact form
      poll.df <- dcast(poll.df, numplayers ~ values, value.var = "numvotes")
      
      zero.votes <- poll.i[[".attrs"]]["totalvotes"] == 0
      if (!zero.votes){
        polls[[colnames(poll.df)[1]]] <- poll.df
      } else{
        polls[colnames(poll.df)[1]] <- list(NULL)
      }
    } else{
      poll.df <- ProcessOtherPollResult(r = poll.i)
      if (!is.null(poll.df)){
        polls[[colnames(poll.df)[1]]] <- poll.df
      }
    }
  }
  l$item[names(l$item) == "poll"] <- NULL
  l$item$polls <- polls
  # write id in to atomic vector
  l$item$id <- l$item[[".attrs"]]["id"]
  l$item$.attrs <- NULL
  # get rid of unnecessary list within list
  l <- l$item
  # pull together all atmoic values
  atomic <- unlist(lapply(l, is.atomic))
  atomic.df <- data.frame(l[atomic], check.names = FALSE, stringsAsFactors = FALSE)
  l[atomic] <- NULL
  l$info <- atomic.df
  # process stats
  l <- ProcessStats(l = l)
  return(l)
}

ProcessStats <- function(l){
  l$statistics <- l$statistics$ratings
  l$statistics$ranks <- lapply(l$statistics$ranks, function(x){
    data.frame(t(x), check.names = FALSE, stringsAsFactors = FALSE)
  })
  ranks <- bind_rows(l$statistics$ranks)
  nm.keep <- c("friendlyname", "value", "bayesaverage")
  ranks <- ranks[, nm.keep]
  
  ranks1 <- ranks[, nm.keep[2]]
  names(ranks1) <- ranks[, nm.keep[1]]
  
  ranks2 <- ranks[, nm.keep[3]]
  names(ranks2) <- gsub("Rank", "Bayes Average", ranks[, nm.keep[1]])
  
  atomic.st <- unlist(lapply(l$statistics, is.atomic))
  atomic.stats <- data.frame(l$statistics[atomic.st], t(ranks1), t(ranks2),
                             check.names = FALSE, stringsAsFactors = FALSE)
  l$statistics <- atomic.stats
  return(l)
}

ProcessNumberOfPlayersPollResult <- function(r){
  attribute <- r[[".attrs"]]
  r[[".attrs"]] <- NULL
  numvotes <- lapply(r, function(x){
    as.numeric(x["numvotes"])
  })
  numvotes <- unlist(numvotes)
  
  values <- lapply(r, function(x){
    x["value"]
  })
  values <- unlist(values)
  
  r.df <- data.frame(attribute, values, numvotes, stringsAsFactors = FALSE,
                     row.names = NULL)
  colnames(r.df)[1] <- names(attribute)
  return(r.df)
}

ProcessOtherPollResult <- function(r){
  total.votes <- as.numeric(r[[".attrs"]]["totalvotes"])
  if (total.votes > 0){
    attribute <- r[[".attrs"]]["name"]
    r[[".attrs"]] <- NULL
    r <- r$results
    numvotes <- lapply(r, function(x){
      as.numeric(x["numvotes"])
    })
    numvotes <- unlist(numvotes)
    
    values <- lapply(r, function(x){
      x["value"]
    })
    values <- unlist(values)
    
    r.df <- data.frame(values, numvotes, stringsAsFactors = FALSE,
                       row.names = NULL)
    colnames(r.df)[1] <- attribute
  } else{
    r.df <- NULL
  }
  return(r.df)
}

SummarisePolls <- function(l){
  # nr players
  numplayers <- l$polls$numplayers
  if (!is.null(numplayers)){
    # percentages for each number
    numplayers[, -1] <- round(numplayers[, -1] / rowSums(numplayers[, -1]) * 100, 2)
    # best
    best.numplayers.community <- numplayers[which.max(numplayers[, "Best"]), "numplayers"]
    # recommended
    numplayers[, "Best and Recommended"] <- numplayers[, "Best"] +
      numplayers[, "Recommended"]
    recommended.numplayers <- numplayers[which(numplayers[, "Best and Recommended"] >= 50),
                                         "numplayers"]
    has.plus <- grepl("+", recommended.numplayers, fixed = TRUE)
    if (any(has.plus)){
      plus <- recommended.numplayers[has.plus]
      plus <- gsub("+", "", plus, fixed = TRUE)
      more <- plus %in% recommended.numplayers
      recommended.numplayers <- recommended.numplayers[!has.plus]
    } else{
      more <- FALSE
    }
    recommended.numplayers <- sort(as.numeric(recommended.numplayers))
    no.gaps <- all(diff(recommended.numplayers) == 1)
    if (no.gaps & more){
      recommended.numplayers.community <- paste0(">=", min(recommended.numplayers))
    } else if (no.gaps & !more){
      nr.min <- min(recommended.numplayers)
      nr.max <- max(recommended.numplayers)
      if (nr.min < nr.max){
        recommended.numplayers.community <- paste0(" ", nr.min, " - ", nr.max)
      } else{
        recommended.numplayers.community <- paste0(nr.min)
      }
    } else{
      recommended.numplayers.community <- paste0(recommended.numplayers, collapse = ",")
    }
  } else{
    best.numplayers.community <- NA
    recommended.numplayers.community <- NA
  }
  
  # age
  suggestedplayerage <- l$polls$suggested_playerage
  if (!is.null(suggestedplayerage)){
    # most votes
    min.age.community <- suggestedplayerage[which.max(suggestedplayerage[, "numvotes"]),
                                            "suggested_playerage"]
    # average
    suggestedplayerage[suggestedplayerage[, "suggested_playerage"] == "21 and up",
                       "suggested_playerage"] <- "21"
    suggestedplayerage[, "suggested_playerage"] <- 
      as.numeric(suggestedplayerage[, "suggested_playerage"])
    suggestedplayerage[, "numvotes"] <- as.numeric(suggestedplayerage[, "numvotes"])
    w <- suggestedplayerage[, "numvotes"] / sum(suggestedplayerage[, "numvotes"])
    average.min.age.community <- sum(suggestedplayerage[, "suggested_playerage"] * w)
  } else{
    min.age.community <- NA
    average.min.age.community <- NA
  }
  
  # text
  languagedependence <- l$polls$language_dependence
  if (!is.null(languagedependence)){
    language.community <- languagedependence[which.max(languagedependence[, "numvotes"]),
                                             "language_dependence"]
  } else{
    language.community <- NA
  }
  
  #all in one
  stats <- data.frame(best.numplayers.community, recommended.numplayers.community, 
                      min.age.community , average.min.age.community,
                      language.community,
                      check.names = FALSE, stringsAsFactors = FALSE)
  l$polls.summary <- stats
  return(l)
}

AdjustPlayerCount <- function(dt, adj){
  for (i in 1:nrow(adj)){
    nm <- adj[i, "Name"]
    new.count <- adj[i, "Max Players"]
    dt[dt[, "Game Name"] == nm, "Max Players"] <- new.count
  }
  return(dt)
}

