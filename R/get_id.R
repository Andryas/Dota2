#!/usr/bin/env Rscript

# Libraries
library(RDota2)

args <- commandArgs(TRUE)

# register key
key_actions(action = "register_key",
            value = args[1]
            )

if (file.exists("id.RData")) {
    id <- readRDS("id.RData")
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

    newid <- unique(do.call(c, newid))

    id <- unique(c(id, newid))

    if (Sys.Date() > today_) {
        saveRDS(id, paste0("id-", today_, ".RData"))
        today_ <- Sys.Date()
        id <- c()
    } else {
        saveRDS(id, "id.RData")
    }

    Sys.sleep(180)
}
