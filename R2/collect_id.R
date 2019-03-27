#' Collect a massive amount of matches id's.

collect_id <- function(key, skill = 3, game_mode = c(2, 22), lobby_type = c(7)) {
    if (length(skill) != 1) stop("Skill must be a number between 0-3")
    
    hero_id <- RDota2::get_heroes(key = key)$content$id
    
    grid <- expand.grid(hero_id = hero_id, game_mode = game_mode)

    id <- mapply(hero_id = grid$hero_id, game_mode = grid$game_mode,
                 function(hero_id, game_mode) {
                     
                     out <- RDota2::get_match_history(
                                        key = key, 
                                        hero_id = hero_id,
                                        skill = skill,
                                        game_mode = game_mode,
                                        min_players = 10
                                    )
                     
                     sapply(out$content$matches, function(x) {
                         f <- x$lobby_type %in% lobby_type
                         x$match_id[f]
                     })
                     
                 }, SIMPLIFY = FALSE)

    id <- unique(unlist(do.call(c, id)))

    if (is.null(id)) {
        return(0)
    } else {
        m <- mongolite::mongo(paste0("match_id_", skill), "teste")

        query <- paste0('{"_id": ', as.integer(Sys.Date()), '}')
        update <- paste0('{ "$addToSet": { "match_id": { "$each": ', jsonlite::toJSON(id), '}}}')
        m$update(query = query, update = update, upsert = TRUE)
        return(1)
    }
}

