#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------------
library(RDota2)

# ------------------------------------------------------------------------------
# Args
# ------------------------------------------------------------------------------
# 1: keyapi
# 2: local, if 1 (local) else 0
args <- commandArgs(TRUE)

if (is.null(args[1])) stop("An api key is required.")

# register key
key_actions(action = "register_key", value = args[1])

# if local set directory to ...
if (args[2] == 1) {
    setwd("~/DotA2/data/id")
    if (!dir.exists(".collecting")) dir.create(".collectig")
    setwd(".collecting")
}

if (any(grepl("id.RData", list.files()))) {
  id <- readRDS("id.RData")
} else {
  id <- c()
}

# Save the day
today_ <- Sys.Date()
i <- 1

while (TRUE) {
    # game_mode = Ranked Matchmaking (22), Captain's Mode (2),
    #             All Pick (1) e Compendium Matchmaking (14).
    # https://wiki.teamfortress.com/wiki/WebAPI/GetMatchHistory
    ## skill: bracket Very High
    ## min_players: minimum amount of players in a match
    ## game_mode: (1) All pick, (2) Captain's mode,
    ##            (14) Compendium Matchmaking and (22) Ranked Matchmaking
    new_id <- try(get_match_history(skill = 3,
                                    min_players = 10,
                                    game_mode = paste0(1, 2, 14, 22,
                                                       collapse = ";")))

    if (class(new_id) == "try-error") {
        next
    }

    ## -------------------------------------------------------------------------
    ## GetTopLiveGame
    ## -------------------------------------------------------------------------
    new_id_pro <- try(get_top_live_game(partner = 1)$content$game_list)
    new_id_pro2 <- try(get_top_live_game(partner = 2)$content$game_list)
    new_id_pro3 <- try(get_top_live_game(partner = 3)$content$game_list)

    if (class(new_id_pro) == "try-error") {
        new_id_pro <- NULL
    } else {
        new_id_pro  <- lapply(new_id_pro, function(x) x[["match_id"]])
    }

    if (class(new_id_pro2) == "try-error") {
        new_id_pro2 <- NULL
    } else {
        new_id_pro2  <- lapply(new_id_pro2, function(x) x[["match_id"]])
    }

    if (class(new_id_pro3) == "try-error") {
        new_id_pro3 <- NULL
    } else {
        new_id_pro3  <- lapply(new_id_pro3, function(x) x[["match_id"]])
    }
    ## End GetTopLiveGame  -----------------------------------------------------
    new_id <- lapply(new_id$content$matches, function(x) x$match_id)

    new_id2 <- unlist(c(new_id, new_id_pro, new_id_pro2, new_id_pro3))

    id <- unique(c(id, new_id2))

    ## The idea is to save the id's match of today with a name that doesnt
    ## hinder the collection of these IDS by the computer that will process them.
    if (i %% 2 == 0) {
        saveRDS(id, "id.RData")
        if (today_ < Sys.Date()) {
            ## Save the id
            file.rename("id.RData", paste0("id-", today_, "-.RData"))
            today_ <- Sys.Date()
            id <- c()
        }
        i <- i + 1
    }

    # Wait 120 seconds
    Sys.sleep(120)
    i <- i + 1
}
