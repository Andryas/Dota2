#' Settings to collect Dota2 matches.

config <- function(key = NULL, game_mode = c(2, 22), lobby_type = 7, skill = 3, duration = 900, 
                   public_account_id = 5) {
    # Check keys
    x <- lapply(key, function(key) {
        x <- try(RDota2::get_heroes(key = key), silent=TRUE)

        if (class(x) == "try-error") x <- list(response = list(status_code = 1))

        if (x[["response"]][["status_code"]] == 200) {
            return(c(good = key))
        } else {
            return(c(bad = key))
        }
    })
    
    x <- do.call(c, x)
    
    if (any(unique(names(x)) == "bad")) {
        stop("BAD KEYS: ", paste0(as.vector(x[names(x) == "bad"]),collapse=", "), 
            '\n Please make sure these keys are working or try again.')
    }
    x <- as.vector(x[names(x) == "good"])

    if (length(x) == 0) stop("All keys are bad. Please check the keys or try again.")
    
    m <- mongolite::mongo("config", "teste")
    query <- paste0('{"_id": "config"}')
    update <- paste0('{ "$addToSet": { "keyapi": { "$each": ', jsonlite::toJSON(key), '}}}')
    m$update(query = query, update = update, upsert = TRUE)

    args <- list(game_mode, lobby_type, skill, public_account_id, duration)
    nargs <- c("game_mode", "lobby_type", "skill", "public_account_id", "duration")
    invisible(mapply(a = args, na = nargs, function(a, na) {
        update <- paste0('{ "$addToSet": { "', na, '": { "$each": ', jsonlite::toJSON(a), '}}}')
        m$update(query = query, update = update, upsert = TRUE)
    }, SIMPLIFY = FALSE))

    # print(as.list(m$find()[1, ]))
    cat("\n\n\tYour settings for collecting Dota 2 matches have been recorded.",
        "\n\tIf you want to change something in the future just re-run this",
        "\n\tfunction with the settings you want.\n\n")
    
    m$disconnect()
}
