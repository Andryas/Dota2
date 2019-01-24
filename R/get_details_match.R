#!/usr/bin/env Rscript

# # temp -------------------------------------------------------------------------
# cmd <- readRDS("~/DotA2/data/cmd.RData")
# system(eval(parse(text = cmd[[1]])))
# system(eval(parse(text = cmd[[2]])))
# # ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------------
library(RDota2)
library(tcltk)
library(mongolite)
library(jsonlite)

# ------------------------------------------------------------------------------
# Configs
# ------------------------------------------------------------------------------
# 1: keyapi
args <- commandArgs(TRUE)

# args <- c(1, "id-2019-01-16-.RData")

# register key
key_actions(action = "register_key",
            value = readRDS("~/DotA2/data/keyapi.RData")[as.integer(args[1])])

setwd("~/DotA2/data/id")
file <- list.files(pattern = "id-[0-9]{4}-[0-9]{2}-[0-9]{2}-.RData")

# file <- list.files(pattern = "id-pro-[0-9]{4}-[0-9]{2}-[0-9]{2}-.RData")
# file <- lapply(file, readRDS)
# id <- unique(unlist(file))
# file <- "id-pro-2019-01-01--2019-01-18.RData"

if (length(file) == 0) break else file  <- file[1]
id <- readRDS(file)
id <- unlist(id)
N <- length(id)

# ------------------------------------------------------------------------------
# MongoDB
# ------------------------------------------------------------------------------
m <- mongo(collection = "match", db = "dota2")   # Match

# ------------------------------------------------------------------------------
# Clean ID's
# ------------------------------------------------------------------------------
# duration <= 900 & players != 10 & no information (BAD ID's)
# lobby_type not in (0, 2, 5, 7) & game_mode not in (0, 1, 2, 14, 22)
if (length(
    list.files(
        pattern = paste0(gsub(".RData|.+/", "", file), "bad_id"))) >= 1) {
    badid <- readRDS(paste0(gsub(".RData|.+/", "", file), "bad_id.RData"))
} else {
    badid <- data.frame(stringsAsFactors = FALSE)
}

# Storaged IDs in MongoDB
if (length(
    list.files(
        pattern = paste0(gsub(".RData|.+/", "", file), "storedid"))) >= 1) {
    storedid <- readRDS(paste0(gsub(".RData|.+/", "", file), "storedid.RData"))
} else {
    storedid <- c()
}

# si <- m$find(fields = '{"_id": true}')[[1]]
id <- id[!(id %in% storedid)]

# Remove bad IDs
if (nrow(badid) > 0) {
    id <- id[!(id %in% badid$match_id)]
}

# ------------------------------------------------------------------------------
# Collect Details Match
# ------------------------------------------------------------------------------

# tcltk
pb <- tkProgressBar(title = file, min = 0, max = N, width = 300)

while (length(id) > 0) {
    # cat(paste0(N - length(id), "/", N), sep = "\n")
    setTkProgressBar(pb, N - length(id),
                     label = paste0(round((N - length(id))/N * 100, 3), " %"))

    content <-  try(
        R.utils::withTimeout(
                     get_match_details(match_id = id[1]),
                     timeout = 10,
                     onTimeout = "silent"
                 ),
        silent = TRUE
    )

    if ("content" %in% names(content)) {
        if ("error" %in% names(content[["content"]])) {
            badid <- rbind(badid, data.frame(match_id = id[1], type = "error"))
            saveRDS(badid, paste0(gsub(".RData", "", file), "bad_id.RData"))
            id <- id[-1]
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
            file = paste0("log", file, ".log"))
        # file.remove(file)
        stop(paste0("Access is denied. Retrying will not help.",
                    "Please verify your key= parameter."))
    }

    if (content$response[["status_code"]] %in% c(503, 429, 500)) {
        Sys.sleep(20)
        next
    }

    if (content$response[["status_code"]] %in% c(400, 404)) {
        badid <- rbind(badid, data.frame(match_id = id[1],
                                         type = "bad_request"))
        saveRDS(badid, paste0(gsub(".RData", "", file), "bad_id.RData"))
        id <- id[-1]
        next
    }
    # --------------------------------------------------------------------------

    ## Lobby_type & game_mode rules
    if (!(content$content$lobby_type %in% c(0, 2, 5, 7)) |
        !(content$content$game_mode %in%  c(0, 1, 2, 14, 22))) {
        badid <- rbind(badid,
                       data.frame(match_id = id[1], type = "lobbygame"))
        saveRDS(badid, paste0(gsub(".RData", "", file),
                              "bad_id.RData"))
        id <- id[-1]
        next
    }

    ## All account_id public
    account_id <- sapply(content$content$players, function(x) x$account_id)
    account_id <- account_id[!(account_id %in% c(4294967295))]
    if (length(unique(account_id)) < 8) {
        badid <- rbind(badid,
                       data.frame(match_id = id[1],
                                  type = paste0("players",
                                                length(unique(account_id)))))
        saveRDS(badid, paste0(gsub(".RData", "", file),
                              "bad_id.RData"))
        id <- id[-1]
        next
    }

    ## If the match lasted less than 900 seconds DELETE
    if (content$content$duration <= 900) {
        badid <- rbind(badid,
                       data.frame(match_id = id[1], type = "duration"))
        saveRDS(badid, paste0(gsub(".RData", "", file),
                              "bad_id.RData"))
        id <- id[-1]
        next
    }

    content$content$players_available <- length(unique(account_id))


    # Add match in database
    # _pi: player information: 0 FALSE 1 TRUE
    m$insert(toJSON(append(
          list("_id" = content$content$match_id,
               "_pi" = 0), content$content
      ), auto_unbox = TRUE),
      stop_on_error = FALSE)

    storedid <- c(storedid, id[1])
    saveRDS(storedid,
            paste0(gsub(".RData|.+/", "", file), "storedid.RData"))

    id <- id[-1]
}
close(pb)
file.remove(file)
# file.remove(paste0(gsub(".RData", "", file), "bad_id.RData"))
