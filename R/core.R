#' The engine to collect the Dota2 data.
#'
#' %makes documentation available but removes function from index
#' @keywords internal
#' @export

collect <- function(type = "collect_id") {
    m <- mongolite::mongo("config", "dota")
    config_info <- m$find('{"_id": "config"}')
    m$disconnect()
    
    key <- config_info$keyapi[[1]]
    game_mode <- config_info$game_mode[[1]]
    lobby_type <- config_info$lobby_type[[1]]
    skill <- config_info$skill[[1]]
    public_account_id <- config_info$public_account_id[[1]]
    duration <- config_info$duration[[1]]
    n_history_matches <- config_info$n_history_matches[[1]]
    
    if (type == "collect_id") {
        query <- paste0('{"_id": ', as.integer(Sys.Date() - 1), '}')
        m <- mongolite::mongo("track", "dota")

        if (nrow(m$find(query)) == 0) {
            m2 <- mongolite::mongo("players", "dota")
            m3 <- mongolite::mongo("match", "dota")
            m4 <- mongolite::mongo(paste0("match_id_", skill), "dota")
            n_player <- m2$info()$stats$count
            n_match <- m3$info()$stats$count
            n_id <- length(m4$find(paste0('{"_id": ', as.integer(Sys.Date() - 1) ,'}'))$match_id[[1]])
            if (is.null(n_player)) n_player <- 0
            if (is.null(n_match)) n_match <- 0
            if (is.null(n_id)) n_id <- 0
            info <- list("_id" = as.integer(Sys.Date() - 1),
                         date = Sys.Date() - 1,
                         player = n_player,
                         match = n_match,
                         match_id = n_id)
            m2$disconnect()
            m3$disconnect()
            m4$disconnect()
            m$insert(jsonlite::toJSON(info, auto_unbox = TRUE))
        }
        
        m$disconnect()
        
        script <- system.file(package = "RDota2Plus", "scripts", "collect_id.R")
        p <- processx::process$new(script, c(key[1], lobby_type, skill))

        ## Eval if the process is running in background
        today <- as.integer(Sys.Date())
        while (TRUE) {
            if (!p$is_alive()) {
                p <- processx::process$new(script, c(key[1], lobby_type, skill))
            }
            Sys.sleep(900)
            if (as.integer(Sys.Date()) > today) break
        }
    } else if (type == "collect_match_details") {
        if (length(key) == 1) key <- key[1] else key <- key[-1] 
        
        m <- mongolite::mongo(paste0("match_id_", skill), "dota")
        query <- paste0('{"_id": { "$lt": ', as.integer(Sys.Date()), '}}')
        fields <- '{"_id": 1, "match_id": 1}'
        df <- dplyr::as_tibble(m$find(query, fields ,'{"_id": -1}', limit = 7))
        m$disconnect()
        
        if (nrow(df) >  0) {

            document <- df[1, ]$`_id`[[1]]
            match_id <- df[1, ]$match_id[[1]]
            script <- system.file(package = "RDota2Plus", "scripts", "collect_match_details.R")

            ## One key serves to collect match_id
            match_id <- match_id[1:((length(match_id) %/% (length(key))) * (length(key)))]
            if (length(match_id) >  0) {

                match_id <- split(match_id, ceiling(1:length(match_id)/(length(match_id)/length(key))))
                
                lconfig <- list(game_mode = game_mode, lobby_type = lobby_type,
                                public_account_id = public_account_id, skill = skill,
                                duration = duration)
                
                lmatch_id <- lapply(match_id, function(x) list(match_id = x))
                lmatch_id <- lapply(lmatch_id, function(x) append(x, lconfig))
                lmatch_id <- lapply(lmatch_id, function(x) list("$set" = x))

                m <- mongolite::mongo("collect_match_id", "dota")

                ## Register match_id to collect
                for (i in 1:length(key)) {
                    lmatch_id_json <- jsonlite::toJSON(lmatch_id[[i]], auto_unbox = TRUE)
                    m$update(paste0('{"_id": "', key[i], '"}'), lmatch_id_json, upser = TRUE)
                }
                
                for (j in 1:length(key)) {
                    assign(paste0("p", j), processx::process$new(script, c(key[j], document)))
                }

                today <- Sys.time()
                while (TRUE) {
                    for (w in 1:length(key)) {
                        n <- m$find(paste0('{"_id": "', key[w], '"}'))$match_id[[1]]

                        if (length(n) == 0) {
                            key <- key[-w]
                            next
                        } 

                        condition <- eval(parse(text = paste0("p", w, "$is_alive()")))
                        
                        if (!isTRUE(condition)) {
                            assign(paste0("p", w), processx::process$new(script, c(key[w], document)))
                        } 
                    }
                    
                    if (length(key) == 0) break ## Add log finish and time

                    ## After two hour finish the process
                    if (as.integer(difftime(Sys.time(), today, units = "mins")) >= 120) {
                        for (w in 1:length(key)) {
                            eval(parse(text = paste0("p", w, "$kill()")))
                        }
                        
                        quit()
                    }

                    Sys.sleep(5)
                }

                m$disconnect()
            } else {
                ## add log no data to collect
            }
        } else {
            ## add log no data to collect
        }
        
    } else if (type == "collect_players_details") {
        if (length(key) == 1) key <- key[1] else key <- key[-1] 
        
        ## if TRUE then the match history player match wont collect
        if (n_history_matches >  0) {
            m <- mongolite::mongo("match", "dota")
            df <- dplyr::tibble()
            match_id <- "[]"
            while (nrow(df) < 30000) {
                query <- paste0('{"_pi": 0, "public_account_id": {"$gte": ',
                                public_account_id, '}, "match_id": {"$nin": ', match_id, '}}')
                new_df <- dplyr::as_tibble(m$find(query, limit = 5000))
                if (nrow(new_df) == 0) break
                df <- dplyr::bind_rows(df, new_df)
                match_id <- jsonlite::toJSON(unique(df$match_id))
            }
            m$disconnect()

            if (nrow(df) >  0) {
                df$players <- lapply(df$players, function(x) {
                    x <- dplyr::select(x, account_id, hero_id)
                    dplyr::as_tibble(x)
                })

                df <- split(df, ceiling(1:nrow(df)/(nrow(df)/length(key))))
                df <- lapply(df, function(x) tidyr::unnest(x, players))
                df <- lapply(df, function(x) dplyr::filter(x, account_id != 4294967295))
                df <- lapply(df, function(x) dplyr::select(x, match_id, start_time, hero_id, account_id))

                lconfig <- list(game_mode = game_mode, lobby_type = lobby_type,
                                public_account_id = public_account_id, skill = skill,
                                duration = duration, n_history_matches = n_history_matches)
                
                lp <- lapply(df, function(x) list(account_id = x))
                lp <- lapply(lp, function(x) append(x, lconfig))
                lp <- lapply(lp, function(x) list("$set" = x))
                
                m <- mongolite::mongo("collect_account_id", "dota")

                ## Register match_id to collect
                for (i in 1:length(key)) {
                    lp_json <- jsonlite::toJSON(lp[[i]], auto_unbox = TRUE)
                    m$update(paste0('{"_id": "', key[i], '"}'), lp_json, upser = TRUE)
                }

                script <- system.file(package = "RDota2Plus", "scripts", "collect_players_details.R")
                
                ## Start process
                for (j in 1:length(key)) {
                    assign(paste0("p", j), processx::process$new(script, key[j]))
                }

                today <- as.integer(Sys.Date())
                while (as.integer(Sys.Date()) == today) {
                    for (w in 1:length(key)) {
                        n <- m$find(paste0('{"_id": "', key[w], '"}'))$account_id[[1]]

                        if (length(n) == 0) {
                            key <- key[-w]
                            ## add time key removed
                            next
                        }

                        condition <- eval(parse(text = paste0("p", w, "$is_alive()")))
                        
                        if (!isTRUE(condition)) {
                            assign(paste0("p", w), processx::process$new(script, key[w]))
                        } 
                    }
                    
                    if (length(key) == 0) break ## add back no key
                    
                    if (as.integer(Sys.Date()) > today) {
                        for (w in 1:length(key)) {
                            eval(parse(text = paste0("p", w, "$kill()")))
                        }
                        ## backlog new day
                    }
                    
                    Sys.sleep(5)
                }
                
                m$disconnect()
                
            } else {
                ## no data to collect
            }
        } else {
            ## disable player history
        }
    } else {
        stop("type must be one of the follows args: 'collect_id'
             'collect_match_details' 'collect_players_details'")
    }
}
