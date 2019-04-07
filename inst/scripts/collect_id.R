#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

key <- args[1]
lobby_type <- args[2]
skill <- args[3]

today <- as.integer(Sys.Date())

while (TRUE) {
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

    if (!is.null(id)) {
        m <- mongolite::mongo(paste0("match_id_", skill), "dota")

        query <- paste0('{"_id": ', as.integer(Sys.Date()), '}')
        update <- paste0('{ "$addToSet": { "match_id": { "$each": ',
                         jsonlite::toJSON(id), '}}}')
        m$update(query = query, update = update, upsert = TRUE)
        m$disconnect()
        Sys.sleep(300)
    }

    if (as.integer(Sys.Date()) > today)  break

    
    
}

