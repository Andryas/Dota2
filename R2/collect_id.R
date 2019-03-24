collect_id <- function(key, skill = 3, min_players = 10, returnID = FALSE) {
    heroesid <- RDota2::get_heroes(key = key)$content$id
    
    id <- lapply(heroesid, function(h) {
        out <- RDota2::get_match_history(
                           key = key, 
                           hero_id = h,
                           skill = skill,
                           min_players = min_players
                       )
        out <- sapply(out$content$matches, function(x) x$match_id)
        out
    })

    id <- unique(unlist(do.call(c, id)))

    m <- mongolite::mongo(paste0("ids", skill), "teste")

    query <- paste0('{"_id": ', as.integer(Sys.Date()), '}')
    update <- paste0('{ "$addToSet": { "match_id": { "$each": ', jsonlite::toJSON(id), '}}}')
    m$update(query = query, update = update, upsert = TRUE)
    if (isTRUE(returnID)) return(id)
}

