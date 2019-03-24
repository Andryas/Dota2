config <- function(key = NULL, game_mode = c(1, 22), lobby_type = 7, skill = 3,
                   public_account_id = 5, duration = 900, min_players = 10) {
    if (!is.vector(key)) stop("key must be a vector of key api.")

    x <- lapply(key, function(key) {
        x <- try(RDota2::get_heroes(key = key), silent=TRUE)

        if (class(x) == "try-error") x <- list(response = list(status_code = 1))

        if (x[["response"]][["status_code"]] == 200) {
            return(c(goodid = key))
        } else {
            return(c(badid = key))
        }
    })
    
    x <- do.call(c, x)
    x <- as.vector(x[names(x) == "goodid"])
    
    m <- mongolite::mongo("config", "teste")
    query <- paste0('{"_id": "config"}')
    update <- paste0('{ "$addToSet": { "keyapi": { "$each": ', jsonlite::toJSON(key), '}}}')
    m$update(query = query, update = update, upsert = TRUE)

    args <- list(game_mode, lobby_type, skill, public_account_id, duration, min_players)
    nargs <- c("game_mode", "lobby_type", "skill", "public_account_id", "duration", "min_players")
    invisible(mapply(a = args, na = nargs, function(a, na) {
        update <- paste0('{ "$addToSet": { "', na, '": { "$each": ', jsonlite::toJSON(a), '}}}')
        m$update(query = query, update = update, upsert = TRUE)
    }, SIMPLIFY = FALSE))

    m$disconnect()
}
