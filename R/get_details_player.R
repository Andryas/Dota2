#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------------
library(RDota2)
library(mongolite)
library(jsonlite)
library(lubridate)
library(dplyr)
library(tcltk2)

# ------------------------------------------------------------------------------
# MongoDB
# ------------------------------------------------------------------------------
m <- mongo(collection = "match", db = "dota2")   # Match
m2 <- mongo(collection = "player", db = "dota2") # Player


# ------------------------------------------------------------------------------
# Init configs
# ------------------------------------------------------------------------------
# 1: number to a keyapi in DotA2/data/keyapi.RData
# 2: .RData with a list of match_id & account_id
args <- commandArgs(TRUE)

# register key
key_actions(action = "register_key",
            value = readRDS("~/DotA2/data/keyapi.RData")[as.integer(args[1])])

setwd("~/DotA2/data/id/")

ids <- readRDS(args[2])                #! IDs
index <- which(sapply(ids, nrow) == 0) #! Del null lists
ids <- ids[-index]
N <- length(ids)                       #! length to tcltk

ids_collected <- m$find(query = '{"_pi": 1}',
                        fields = '{"_id": true}')$`_id` #! from mongo match
index2 <- sapply(ids, function(x) unique(x$match_id)) %in% ids_collected
if (length(which(index2)) > 0) {
    i <- max(which(index2)) + 1 ## i
} else {
    i <- 1
}

# ------------------------------------------------------------------------------
# Collect Details Player
# ------------------------------------------------------------------------------
root <- tktoplevel()
tktitle(root) <- args[2]

l1 <- tk2label(root, args[2])
pb1 <- tk2progress(root, length = 300)
tkconfigure(pb1, value=0, maximum = length(ids))

l2 <- tk2label(root, "Collecting Player's Details")
pb2 <- tk2progress(root, length = 300)

tkpack(l1)
tkpack(pb1)
tkpack(l2)
tkpack(pb2)

for (i in i:N) {
    tkconfigure(l1, text = paste0(i, "/", N))
    tkconfigure(pb1, value = i)

    p <- ids[[i]]

    if (nrow(p) == 0) {
        next
    }

    for (j in 1:nrow(p)) {

        #! Historico anterior do jogador anterior a partida coletada
        history <-  try(
            R.utils::withTimeout(
                         get_match_history(account_id = p$account_id[j],
                                           start_at_match_id = p$match_id[j]),
                         timeout = 10,
                         onTimeout = "silent"
                     ),
            silent = TRUE
        )

        player_match_id <- sapply(history$content$matches, function(x) {
            x$match_id
        })

        #! Informação armazenada do jogador no MongoDB
        player_db <-  m2$find(
                             query = paste0('{"_id": ', p$account_id[j], '}'),
                             fields = paste0('{"details.match_id": true }')
                         )

        player_badid_db <- m2$find(
                                  query = paste0('{"_id": ', p$account_id[j], '}'),
                                  fields = paste0('{"badid": true }')
                              )

        #! Remove partidas já coletadas
        if (nrow(player_db) > 0) {
            player_match_id <- player_match_id[!(player_match_id %in%
                                                 player_db$details[[1]]$match_id)]
        }

        #! Remove partidas consideradas badid
        if (nrow(player_badid_db) > 0) {
            player_match_id <-
                player_match_id[!(player_match_id %in%
                                  player_badid_db$badid[[1]])]
        }

        ## Se não houver partidas historicas do jogador j
        if (length(player_match_id) == 0) {
            tkconfigure(pb2, value=0, maximum = 10)
            tkconfigure(l2, text = paste0("Jogador ", j, " de ", nrow(p)))
            tkconfigure(pb2, value = 10)
            next
        }

        tkconfigure(pb2, value=0, maximum = length(player_match_id))

        #! Coleta partidas anteriores do jogador j
        w <- 0
        while (length(player_match_id) > 0) {
            w <- w + 1
            tkconfigure(l2, text = paste0("Jogador ", j, " de ", nrow(p)))
            tkconfigure(pb2, value = w)

            content <-  try(
                R.utils::withTimeout(
                             get_match_details(match_id = player_match_id[1]),
                             timeout = 10,
                             onTimeout = "silent"
                         ),
                silent = TRUE
            )


            if ("content" %in% names(content)) {
                if ("error" %in% names(content[["content"]])) {
                    next
                }
            }

            # --------------------------------------------------------------------------
            # https://partner.steamgames.com/doc/webapi_overview/responses
            # 200: success
            # 400: Bad request
            # 401: Unauthorized
            # 403: Forbidden
            # 404: Not found
            # 429: Too many requests
            # 500: Internet server error
            # 503: Service unvavailable

            if (content$response[["status_code"]] %in% c(401, 403)) {
                cat(paste0("Access is denied. Retrying will not help.",
                           "Please verify your key= parameter.\n\n", args[1]),
                    file = paste0("log", args[2], ".log"))
                # file.remove(args[2])
                stop(paste0("Access is denied. Retrying will not help.",
                            "Please verify your key= parameter."))
            }

            if (content$response[["status_code"]] %in% c(503, 429, 500)) {
                Sys.sleep(60)
                next
            }


            if (content$response[["status_code"]] %in% c(400, 404)) {
                player_match_id <- player_match_id[-1]
                next
            }

            # --------------------------------------------------------------------------
            ## Lobby_type & game_mode rules
            if (!(content$content$lobby_type %in% c(0, 2, 5, 7)) |
                !(content$content$game_mode %in%  c(0, 1, 2, 14, 22))) {
                m2$update(
                       query = paste0('{"_id":', p$account_id[j], '}'),
                       update =  toJSON(
                           list("$addToSet" = list(badid = content$content$match_id)),
                           auto_unbox = TRUE
                       ),
                       upsert = TRUE
                   )
                player_match_id <- player_match_id[-1]
                next
            }

            ## If the match lasted less than 900 seconds DELETE
            if (content$content$duration <= 900) {
                m2$update(
                       query = paste0('{"_id":', p$account_id[j], '}'),
                       update =  toJSON(
                           list("$addToSet" = list(badid = content$content$match_id)),
                           auto_unbox = TRUE
                       ),
                       upsert = TRUE
                   )
                player_match_id <- player_match_id[-1]
                next
            }

            player <- content$content$players
            player <- lapply(player, function(x) {
                if ("ability_upgrades" %in% names(x)) {
                    players_au <<- x$ability_upgrades
                    cbind.data.frame(x[-which(names(x) == "ability_upgrades")])
                } else {
                    players_au <<- NULL
                    cbind.data.frame(x)
                }}) %>%
                bind_rows() %>%
                mutate(position = 1:10)

            player <- player %>%
                filter(account_id == p$account_id[j]) %>%
                mutate(
                    duration = content$content$duration,
                    radiant_win = content$content$radiant_win,
                    start_time = content$content$start_time,
                    match_seq_num = content$content$match_seq_num,
                    match_id = content$content$match_id
                )

            if (is.null(players_au)) {
                player <- list("$addToSet" = list(details = as.list(player)))
            } else {
                player <-
                    list("$addToSet" =
                             list(
                                 details = append(
                                     as.list(player),
                                     list(ability_upgrades =
                  content$content$players[[player$position]]$ability_upgrades))))
            }

            #! Add Jogador
            m2$update(
                   query = paste0('{"_id":', p$account_id[j], '}'),
                   update =  toJSON(player, auto_unbox = TRUE),
                   upsert = TRUE
               )

            player_match_id <- player_match_id[-1]
            tcl("update")
        }
    }


    m$update(paste0('{"_id":', p$match_id[1], '}'),
             '{"$set": {"_pi": 1}}')

    tcl("update")
}

tkdestroy(root)
