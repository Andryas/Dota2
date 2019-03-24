collect_details_match <- function(match_id, key, public_account_id = 5,
                                  duration = 900,  game_mode = c(1, 22),
                                  lobby_type = 7, skill = 3, ids) {

    content <- get_match_details_2(match_id = match_id, key = key)
    
    if (is.null(content)) {
        m <- mongolite::mongo(paste0("ids", skill), "teste")
        query <- paste0('{"_id": ', ids, '}')
        update <- paste0('{"$pull": {"match_id": ', match_id, '}}')
        m$update(query = query, update = update)
        m$disconnect()
        return(0)
    }

    ## Lobby_type & game_mode rules
    if (!(content$content$lobby_type %in% lobby_type) |
        !(content$content$game_mode %in%  game_mode)) {
        m <- mongolite::mongo(paste0("ids", skill), "teste")
        query <- paste0('{"_id": ', ids, '}')
        update <- paste0('{"$pull": {"match_id": ', match_id, '}}')
        m$update(query = query, update = update)
        m$disconnect()
        return(0)
    }

    ## All account_id public
    account_id <- sapply(content$content$players, function(x) x$account_id)
    account_id <- account_id[!(account_id %in% c(4294967295))] ## rmv private account_id

    # At least 'public_account_id' players with publick account
    if (length(unique(account_id)) < public_account_id) {
        m <- mongolite::mongo(paste0("ids", skill), "teste")
        query <- paste0('{"_id": ', ids, '}')
        update <- paste0('{"$pull": {"match_id": ', match_id, '}}')
        m$update(query = query, update = update)
        m$disconnect()
        return(0)
    }

    ## If the match lasted less than 900 seconds remove
    if (content$content$duration <= duration) {
        m <- mongolite::mongo(paste0("ids", skill), "teste")
        query <- paste0('{"_id": ', ids, '}')
        update <- paste0('{"$pull": {"match_id": ', match_id, '}}')
        m$update(query = query, update = update)
        m$disconnect()
        return(0)
    }

    content$content$amount_public_players <- length(unique(account_id))
    
    # Add match in database
    # _pi: player information: Boolean
    m2 <- mongolite::mongo("match", "teste")
    m2$insert(
           jsonlite::toJSON(
               append(
                   list(
                       "_id" = content$content$match_id,
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

    ## delete match_id
    m <- mongolite::mongo(paste0("ids", skill), "teste")
    query <- paste0('{"_id": ', ids, '}')
    update <- paste0('{"$pull": {"match_id": ', match_id, '}}')
    m$update(query = query, update = update)
    m$disconnect()
    
    return(1)
}

