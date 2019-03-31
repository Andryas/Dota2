collect <- function(n_process = 4) {

    
    # config ---------------------------------------------------------------------------------------
    m <- mongolite::mongo("config", "teste")
    config <- m$find('{"_id": "config"}')
    m$disconnect()
    key <- config$keyapi[[1]]
    game_mode <- config$game_mode[[1]]
    lobby_type <- config$lobby_type[[1]]
    skill <- config$skill[[1]]
    public_account_id <- config$public_account_id[[1]]
    duration <- config$duration[[1]]
    min_players <- config$min_players[[1]]
    key <- if (length(key) > (n_process + 1)) {
               key[2:(n_process + 1)]
           } else {
               stop(paste0("n_process must be less than the number of registered keys.\n",
                           "n_process == (length(registered_key) - 1)"))
           }

    config <- lapply(key, function(x) list(key = x, game_mode = game_mode,
                                           lobby_type = lobby_type, skill = skill,
                                           public_account_id = public_account_id,
                                           duration = duration, min_players = min_players))
    

    # id -------------------------------------------------------------------------------------------
    ids <- as.integer(Sys.Date() - 1) ## collect details from last days id
    m <- mongolite::mongo(paste0("ids", skill), "teste")
    id <- m$find(paste0('{"_id": ', ids, '}'))
    if (nrow(id) > 0) {
        id <- id$match_id[[1]]
    } else {
        return(print("No ids available to collect."))
    }
    m$disconnect()

    
    # collect --------------------------------------------------------------------------------------
    num_cl <- parallel::detectCores()
    num_cl <- ifelse(n_process <= num_cl, n_process, 2)
    cl <- parallel::makeCluster(num_cl)
    source("~/Documentos/github/dota2/R2/get_match_details_2.R")
    source("~/Documentos/github/dota2/R2/collect_details_match.R")
    parallel::clusterExport(cl, list("get_match_details_2", "ids",
                                     "collect_details_match"), envir = environment())
    # parallel::clusterExport(cl, list("get_match_details_2", "collect_details_match", "ids"))
    ll <- data.frame(id = id, k = ceiling((1:length(id))/(length(id)/num_cl)))
    ll <- split(ll, ll$k)
    ll <- mapply(l = ll, config = config, function(l = ll, config = config) {
        append(list(id = l$id), list(config = config))
    }, SIMPLIFY = FALSE)
    
    out <- parallel::parLapply(cl, ll, function(l) {

        lapply(l$id, function(match_id) {
            collect_details_match(
                match_id = match_id,
                key = l$config$key,
                public_account_id = l$config$public_account_id,
                duration = l$config$duration,
                game_mode = l$config$game_mode,
                lobby_type = l$config$lobby_type,
                skill = l$config$skill,
                ids = ids
            )
        })
        
    })

    tb <- table(c(unlist(out)))
    return(tb)
    ## save mongodb
}
