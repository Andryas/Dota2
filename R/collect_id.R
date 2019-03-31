#' @title Collect a massive amount of matches id's.
#'
#' @description This function is used to collected the matches id of games that is happing in this
#'     exactly moment to later collect the details of the match after finishing.

collect_id <- function(key, skill = 3, lobby_type = c(7)) {
    if (length(skill) != 1) stop("Skill must be a number between 0-3")
    
    hero_id <- RDota2::get_heroes(key = key)$content$id
    
    id <- lapply(hero_id,  function(h) {
        out <- RDota2::get_match_history(
                           key = key, 
                           hero_id = h,
                           skill = skill,
                           min_players = 10
                       )
        
        sapply(out$content$matches, function(x) {
            f <- x$lobby_type %in% lobby_type
            x$match_id[f]
        })
        
    })

    id <- unique(unlist(do.call(c, id)))

    if (is.null(id)) {
        return(0)
    } else {
        m <- mongolite::mongo(paste0("match_id_", skill), "teste")

        query <- paste0('{"_id": ', as.integer(Sys.Date()), '}')
        update <- paste0('{ "$addToSet": { "match_id": { "$each": ',
                         jsonlite::toJSON(id), '}}}')
        m$update(query = query, update = update, upsert = TRUE)
        return(1)
    }
}

