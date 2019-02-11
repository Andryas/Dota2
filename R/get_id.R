#!/usr/bin/env Rscript

# Libraries
library(RDota2)

# Args
args <- commandArgs(TRUE)

# register key
key_actions(action = "register_key", value = args[1])

if (!dir.exists("~/Dota2/id")) dir.create("~/Dota2/id")

setwd("~/Dota2/id")

if (file.exists("id.RData")) {
    id <- unlist(readRDS("id.RData"))
} else {
    id <- c()
}

# Configs
heroesid <- get_heroes()$content$id
today_ <- Sys.Date()

while (TRUE) {
    newid <- try(
        lapply(heroesid, function(h) {
            newid <- get_match_history(min_players = 10,
                                       skill = 3,
                                       hero_id = h
                                       )
            newid <- sapply(newid$content$matches,
                            function(x) x$match_id)
            newid
        })
    )

    if (class(newid) == "try-error") next

    newid <- unique(unlist(do.call(c, newid)))

    id <- unique(unlist(c(id, newid)))

    if (Sys.Date() > today_) {
        saveRDS(unlist(id), paste0("id-", today_, "-.RData"))
        today_ <- Sys.Date()
        id <- c()
    } else {
        saveRDS(unlist(id), "id.RData")
    }

    Sys.sleep(180)
}
