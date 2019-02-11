#!/usr/bin/env Rscript

# Libraries
library(RDota2)
library(mongolite)
library(jsonlite)

# Configs
args <- commandArgs(TRUE)
# args <- c(1)

# register key
key_actions(action = "register_key",
            value = readRDS("~/Dota2/keyapi.RData")[as.integer(args[1])])

# MongoDB
m <- mongo(collection = "match", db = "dota2")

setwd("~/Dota2/id")

file <- list.files(pattern = paste0("id-[0-9]{4}-[0-9]{2}-[0-9]{2}-",
                                    args[1], ".RData"))
id <- readRDS(file)
N <- length(id)

# BAD IDs
# duration <= 900 & players != 10 & no information (BAD ID's)
if (length(list.files(
    pattern = paste0(gsub(".RData|.+/", "", file), "bad_id"))) >= 1) {
    badid <- readRDS(paste0(gsub(".RData|.+/", "", file), "bad_id.RData"))
} else {
    badid <- data.frame(stringsAsFactors = FALSE)
}

# IDs in MongoDB
if (length(list.files(
    pattern = paste0(gsub(".RData|.+/", "", file), "storedid"))) >= 1) {
    storedid <- readRDS(paste0(gsub(".RData|.+/", "", file), "storedid.RData"))
} else {
    storedid <- c()
}

id <- id[!(id %in% storedid)]
id <- id[!(id %in% badid$match_id)]

# Collect Details Match ---------------------------------------------------
# pb <- tkProgressBar(title = file, min = 0, max = N, width = 300)

while (length(id) > 0) {
    # setTkProgressBar(pb, N - length(id),
    #                  label = paste0(round((N - length(id))/N * 100, 3), " %"))

    content <-  try(
        R.utils::withTimeout(
                     get_match_details(match_id = id[1]),
                      timeout = 10,
                     onTimeout = "silent"
                 ),
        silent = TRUE
    )

    # https://partner.steamgames.com/doc/webapi_overview/responses
    # 200: success
    # 400: Bad request
    # 401: Unauthorized
    # 403: Forbidden
    # 404: Not found
    # 429: Too many requests
    # 500: Internet server error
    # 503: Service unvavailable

    if ("content" %in% names(content)) {
        if ("error" %in% names(content[["content"]])) {
            badid <- rbind(badid, data.frame(match_id = id[1], type = "error"))
            saveRDS(badid, paste0(gsub(".RData", "", file), "bad_id.RData"))
            id <- id[-1]
            next
        }
    }

    if (content$response[["status_code"]] %in% c(401, 403)) {
        stop(paste0("Access is denied. Retrying will not help.",
                    "Please verify your key= parameter."))
    }

    if (content$response[["status_code"]] %in% c(503, 429, 500)) {
        Sys.sleep(20)
        next
    }

    if (content$response[["status_code"]] %in% c(400, 404)) {
        badid <- rbind(badid, data.frame(match_id = id[1], type = "bad_request"))
        saveRDS(badid, paste0(gsub(".RData", "", file), "bad_id.RData"))
        id <- id[-1]
        next
    }

    ## Lobby_type & game_mode rules
    # !(content$content$lobby_type %in% c(0, 2, 5, 7))
    # game_mode: All Pick & Ranked Matchmaking
    # if (!(content$content$game_mode %in%  c(1, 22))) {
    #     badid <- rbind(badid, data.frame(match_id = id[1], type = "lobbygame"))
    #     saveRDS(badid, paste0(gsub(".RData", "", file), "bad_id.RData"))
    #     id <- id[-1]
    #     next
    # }

    ## All account_id public
    account_id <- sapply(content$content$players, function(x) x$account_id)
    account_id <- account_id[!(account_id %in% c(4294967295))]
    # At least 9 players with publick account
    if (length(unique(account_id)) < 9) {
        badid <- rbind(badid,
                       data.frame(
                           match_id = id[1],
                           type = paste0("players", length(unique(account_id)))
                       ))
        saveRDS(badid, paste0(gsub(".RData", "", file), "bad_id.RData"))
        id <- id[-1]
        next
    }

    ## If the match lasted less than 900 seconds DELETE
    if (content$content$duration <= 900) {
        badid <- rbind(badid, data.frame(match_id = id[1], type = "duration"))
        saveRDS(badid, paste0(gsub(".RData", "", file), "bad_id.RData"))
        id <- id[-1]
        next
    }

    content$content$players_available <- length(unique(account_id))

    # Add match in database
    # _pi: player information: 0 FALSE 1 TRUE
    m$insert(
          toJSON(
              append(
                  list(
                      "_id" = content$content$match_id,
                      "_pi" = 0
                  ),
                  content$content
              ),
              auto_unbox = TRUE),
          stop_on_error = FALSE
      )

    storedid <- c(storedid, id[1])
    saveRDS(storedid, paste0(gsub(".RData|.+/", "", file), "storedid.RData"))

    id <- id[-1]
}
# close(pb)
file.remove(file)
