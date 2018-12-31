#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------------
library(RDota2)
library(mongolite)
library(jsonlite)
library(lubridate)
library(dplyr)

# ------------------------------------------------------------------------------
# Args
# ------------------------------------------------------------------------------
# 1: keyapi
# 2: .RData of ID's is required
args <- commandArgs(TRUE)

# args <- c("1C0E621C134D76AFF6ECF401EE4681D0",
#           "/home/andryas/DotA2/data/id/id-2018-12-28-1.RData"
#           )

if (is.null(args[1])) stop("An api key is required.")
if (is.null(args[2])) print(".RData of ID's is required.")
if (!file.exists(args[2])) stop("This files doens't exist.")

# register key
key_actions(action = "register_key", value = args[1])

setwd("~/DotA2/data/id")

id <- readRDS(args[2])

# ------------------------------------------------------------------------------
# Log
# ------------------------------------------------------------------------------
log <- file(paste0(args[2], ".log"), open = "wt")
sink(log, type = "message")

# ------------------------------------------------------------------------------
# MongoDB
# ------------------------------------------------------------------------------
m <- mongo(collection = "match", db = "dota")   # Match
m2 <- mongo(collection = "player", db = "dota") # Player

# ------------------------------------------------------------------------------
# Clean ID's
# ------------------------------------------------------------------------------
# duration <= 900 & players != 10 & no information (BAD ID's)

if (length(
    list.files(
        pattern = paste0(gsub(".RData|.+/", "", args[2]), "bad_id"))) >= 1) {
    badid <- readRDS(paste0(gsub(".RData|.+/", "", args[2]), "bad_id.RData"))
} else {
    badid <- c()
}

# Storaged ID in MongoDB
si <- m$find(fields = '{"_id": true}')[[1]]
id <- id[!(id %in% si)]

# Remove bad ids
id <- id[!(id %in% badid)]

# ------------------------------------------------------------------------------
# Collect Details
# ------------------------------------------------------------------------------
while (length(id) > 0) {
    content <-  try(
        R.utils::withTimeout(
                     get_match_details(match_id = id[1]),
                     timeout = 10,
                     onTimeout = "silent"
                 ),
        silent = TRUE
    )

    if (class(content) == "try-error") {
        if (is.numeric(badid)) {
            badid <- c(badid, id[1])
            saveRDS(badid, paste0(gsub(".RData", "", args[2]), "bad_id.RData"))
            id <- id[-1]
            next
        }
    }

    if ("content" %in% names(content)) {
        if ("error" %in% names(content[["content"]])) {
            if (is.numeric(badid)) {
                badid <- c(badid, id[1])
                saveRDS(badid, paste0(gsub(".RData", "", args[2]), "bad_id.RData"))
                id <- id[-1]
                next
            }
        }
    }


    # https://partner.steamgames.com/doc/webapi_overview/responses
    # 200: success
    # 400: Bad request
    # 401: Unauthorized
    # 403: Forbidden
    # 404: Not found
    # 429: Too many requests
    # 500: Internet server error
    # 503: Service unvavailable

    if (content$response[["status_code"]] %in% c(503, 429, 500)) {
        Sys.sleep(20)
        next
    }

    if (content$response[["status_code"]] %in% c(400, 401, 403, 404)) {
        badid <- c(badid, id[1])
        if (is.numeric(badid)) {
            saveRDS(badid, paste0(gsub(".RData", "", args[2]), "bad_id.RData"))
        }
        id <- id[-1]
        next
    }

    ## (STOP) If the match lasted less than 900 seconds DELETE
    if (content$content$duration <= 900) {
        badid <- c(badid, id[1])
        if (is.numeric(badid)) {
            saveRDS(badid, paste0(gsub(".RData", "", args[2]),
                                  "bad_id.RData"))
        }
        id <- id[-1]
        next
    }

    players <- content$content$players

    players <-
        lapply(players, function(x)
            cbind.data.frame(x[-which(names(x) == "ability_upgrades")])) %>%
        bind_rows() %>%
        select(account_id, hero_id)

    ## (STOP) If have less than 10 players DELETE go to NEXT ID
    if (nrow(players) != 10) {
        badid <- c(badid, id[1])
        if (is.numeric(badid)) {
            saveRDS(badid, paste0(gsub(".RData", "", args[2]), "bad_id.RData"))
        }
        id <- id[-1]
        next
    }

    # For each player get the last matches played
    w <- 1
    while (w <= 10) {
        # cat(w, sep = "\n")
        info2 <- get_match_history(
            account_id = players$account_id[w],
            skill = 3,
            min_players = 10,
            # The same as the MATCH that is getting the data
            game_mode = content$content$game_mode
        )

        # while (info2$response[["status_code"]] != 200) {
        #     # ?get_match_history
        #     info2 <- get_match_history(
        #         account_id = players$account_id[w],
        #         skill = 3,
        #         min_players = 10,
        #         # The same as the MATCH that is getting the data
        #         game_mode = content$content$game_mode
        #     )
        #
        #     if (info2$response[["status_code"]] %in% c(429, 500, 503)) Sys.sleep(60)
        #     if (!(info2$response[["status_code"]] %in% c(200, 429, 500, 503, 1))) {
        #         info2$content[[1]] <- NULL
        #         break
        #     }
        # }

        if (!is.null(info2$content[[1]])) {

            info2 <-
                lapply(info2$content$matches, function(x)
                    cbind.data.frame(
                        match_id = x$match_id,
                        start_time = x$start_time
                    )) %>%
                bind_rows()

            # picks matches previous the current match
            # as.POSIXct(info2$start_time, origin = "1970-01-01")
            # as.POSIXct(content$content$start_time, origin = "1970-01-01")
            info2 <- info2 %>%
                filter(
                    start_time < content$content$start_time &
                    start_time >= as.integer(
                                      as.POSIXct(content$content$start_time,
                                                 origin = "1970-01-01") - base::months(3))
                ) %>%
                pull(match_id)

            # Stored Matches
            sm <- m2$find(
                         query = paste0('{"_id": ', players$account_id[w], '}'),
                         fields = paste0('{"details.match_id": true }')
                     )

            ## Just matches that aren't stored in MongoPlayer
            if (nrow(sm) != 0) {
                info2 <- info2[!(info2 %in% sm$details[[1]]$match_id)]
            }

            i <- 1
            N <- 50

            ## Coleta as últimas partidas (i) do jogador (w)
            while (N != 0 & i <= length(info2)) {

                content2 <- try(
                    R.utils::withTimeout(
                                 get_match_details(match_id = info2[i]),
                                 timeout = 5,
                                 onTimeout = "silent"
                             ),
                    silent = TRUE
                )

                if (class(content2) == "try-error") {
                    i <- i + 1
                    next
                }

                # while (content2$response[["status_code"]] !=  200) {
                #     content2 <- try(
                #         R.utils::withTimeout(
                #                      get_match_details(match_id = info2[i]),
                #                      timeout = 5,
                #                      onTimeout = "silent"
                #                  ),
                #         silent = TRUE
                #     )
                #
                #     if (class(content2) == "try-error") {
                #         content2 <- NULL
                #         break
                #     }
                #
                #     if (content2$response[["status_code"]] %in% c(429, 500, 503)) Sys.sleep(60)
                #     if (!(content2$response[["status_code"]] %in%  c(1, 200, 429, 500, 503))) {
                #         content2 <- NULL
                #         break
                #     }
                # }

                # the match needs to be longer than 900 seconds.
                if (!is.null(content2) &
                    !("error" %in% names(content2$content))) {
                    if (content2$content$duration >= 900 &
                        length(content2$content$players) == 10) {

                        # ------------------------------------------------------
                        # Update collect player with all players of this match
                        # ------------------------------------------------------

                        # for (A in 1:10) {
                        players2 <- content2$content$players
                        players2 <- lapply(players2, function(x) {
                            if ("ability_upgrades" %in% names(x)) {
                                players2_au <<- x$ability_upgrades
                                cbind.data.frame(x[-which(names(x) == "ability_upgrades")])
                            } else {
                                players2_au <<- NULL
                                cbind.data.frame(x)
                            }
                        }) %>%
                            bind_rows() %>%
                            mutate(position = 1:10)

                        players2 <- players2 %>%
                            filter(account_id == players$account_id[w]) %>%
                            # filter(account_id == players2$account_id[A] & position == A) %>%
                            mutate(
                                duration = content2$content$duration,
                                radiant_win = content2$content$radiant_win,
                                start_time = content2$content$start_time,
                                match_seq_num = content2$content$match_seq_num,
                                match_id = content2$content$match_id
                            )

                        if (is.null(players2_au)) {
                            players3 <- list("$addToSet" = list(details = as.list(players2)))
                        } else {
                            players3 <- list("$addToSet" = list(details = append(as.list(players2),
                                                                                 content2$content$players[[players2$position]])))
                        }

                        ## PRIVATE ID's
                        # if (players2$account_id %in% c(4294967295)) {
                        # next
                        # } else {
                        # Update account_id with the new informations about the player
                        m2$update(
                               query = paste0('{"_id":', players2$account_id, '}'),
                               update =  toJSON(players3, auto_unbox = TRUE),
                               upsert = TRUE
                           )
                        # }
                        # }

                        N <- N - 1
                        i <- i + 1
                    } else {
                        ## Next match of the player
                        i <- i + 1
                    }
                } else {
                    ## Next match of the player
                    i <- i + 1
                }
            }
            ## Next player
            w <- w + 1
        } else {
            ## Next player
            w <- w + 1
        }
    }

    # Add match in database
    m$insert(toJSON(append(
          list("_id" = content$content$match_id), content$content
      ), auto_unbox = TRUE),
      stop_on_error = TRUE)

    id <- id[-1]
}

# End log
sink(type = "message")
close(log)

file.remove(args[2])
file.remove(paste0(gsub(".RData", "", args[2]), "bad_id.RData"))
