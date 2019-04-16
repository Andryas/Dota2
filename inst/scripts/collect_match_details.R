#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

key <- args[1]
document <- args[2]

m <- mongolite::mongo("collect_match_id", "dota")
configs <- m$find(paste0('{"_id": "', key,'"}'))
m$disconnect()

match_id <- configs$match_id[[1]]

if (length(match_id) == 0) stop("")

game_mode <- configs$game_mode[[1]]
lobby_type <- configs$lobby_type[[1]]
skill <- configs$skill[[1]]
duration <- configs$duration[[1]]
# public_account_id <- configs$public_account_id[[1]]


for (i in 1:length(match_id)) {
    
    content <- RDota2Plus::get_match_details_2(match_id = match_id[i], key = key)

    collection <- paste0("match_id_", skill)

    if (is.null(content)) {
        m <- mongolite::mongo("collect_match_id", "dota")
        query <- paste0('{"_id": "', key, '"}')
        update <- paste0('{"$pull": {"match_id": ', match_id[i], '}}')
        m$update(query = query, update = update)
        m$disconnect()
        next
    }

    ## Lobby_type & game_mode rules
    if (!(content$content$lobby_type %in% lobby_type) |
        !(content$content$game_mode %in%  game_mode)) {
        m <- mongolite::mongo("collect_match_id", "dota")
        query <- paste0('{"_id": "', key, '"}')
        update <- paste0('{"$pull": {"match_id": ', match_id[i], '}}')
        m$update(query = query, update = update)
        m$disconnect()
        next
    }

    ## All account_id public
    account_id <- sapply(content$content$players, function(x) x$account_id)
    account_id <- account_id[!(account_id %in% c(4294967295))] ## rmv private account_id

    # At least 'public_account_id' players with publick account
    # if (length(unique(account_id)) < public_account_id) {
    #     m <- mongolite::mongo("collect_match_id", "dota")
    #     query <- paste0('{"_id": "', key, '"}')
    #     update <- paste0('{"$pull": {"match_id": ', match_id[i], '}}')
    #     m$update(query = query, update = update)
    #     m$disconnect()
    #     next
    # }

    ## If the match lasted less than 900 seconds remove
    if (content$content$duration <= duration) {
        m <- mongolite::mongo("collect_match_id", "dota")
        query <- paste0('{"_id": "', key, '"}')
        update <- paste0('{"$pull": {"match_id": ', match_id[i], '}}')
        m$update(query = query, update = update)
        m$disconnect()
        next
    }

    content$content$public_account_id <- length(unique(account_id))

    # Add match in database
    # _pi: player information: Boolean
    m2 <- mongolite::mongo("match", "dota")
    m2$insert(
           jsonlite::toJSON(
                         append(
                             list(
                                 "_id" = match_id[i], 
                                 "_pi" = 0,
                                 "skill" = skill
                             ),
                             content$content
                         ),
                         auto_unbox = TRUE
                     ),
           stop_on_error = FALSE
       )
    m2$disconnect()

    ## delete match_id from collect_match_id
    m3 <- mongolite::mongo("collect_match_id", "dota")
    query <- paste0('{"_id": "', key, '"}')
    update <- paste0('{"$pull": {"match_id": ', match_id[i], '}}')
    m3$update(query = query, update = update)
    m3$disconnect()

    ## delete match_id from match_id_skill
    m3 <- mongolite::mongo(paste0("match_id_", skill), "dota")
    query <- paste0('{"_id": ', document, '}')
    update <- paste0('{"$pull": {"match_id": ', match_id[i], '}}')
    m3$update(query = query, update = update)
    m3$disconnect()
}

