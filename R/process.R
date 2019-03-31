
process <- function(limit = 1000, players_available = 10, parallel = FALSE) {
    ptm <- proc.time()
    `%>%` <-  magrittr::`%>%`

    m <- mongolite::mongo(collection = "match", db = "dota2")
    
    con <- DBI::dbConnect(RMySQL::MySQL(), dbname = "dota2")

    if (DBI::dbExistsTable(con, "partida")) {
        match_id <- DBI::dbGetQuery(con, "select match_id from dota2.partida;")$match_id
        match_id <- jsonlite::toJSON(match_id)
    } else {
        match_id <- "[]"        
    }

    if (is.numeric(players_available)) {
        players_available <- jsonlite::toJSON(players_available)           
    } else {
        players_available <- jsonlite::toJSON(1:10)           
    }

    query <- paste0('{"_pi": 1, "game_mode": 22, "players_available": {"$in":',
                    players_available,'}, "match_id":{ "$nin":', match_id,' }}')
    df <- m$find(query = query, limit = limit)
    df <- dplyr::as_tibble(df)

    player_info <- df$players
    player_info <- mapply(x = player_info, i = df$match_id, j = df$start_time, 
                          function(x, i, j) {
                              x$match_id <- i
                              x$start_time <- j
                              x$team <- rep(c("radiant","dire"), each = 5)
                              x
                          }, SIMPLIFY = FALSE)
    
    player_info <- plyr::rbind.fill(player_info)
    
    player_info <- player_info[, c("match_id", "account_id", "hero_id",
                                   "start_time", "team")] %>% dplyr::as_tibble()

    cols <- c("account_id", "kills", "deaths", "assists", "last_hits", "denies",
              "gold_per_min", "xp_per_min", "hero_damage", "tower_damage",
              "hero_healing", "gold", "gold_spent", "scaled_hero_damage",
              "scaled_tower_damage", "duration", "ability_upgrades_mean",
              "ability_upgrades_var", "win_player"
              )

    if (isTRUE(parallel)) {
        num_cl <- parallel::detectCores() - 1
        cl <- parallel::makeCluster(num_cl)
        player_info$k <- ceiling((1:limit)/(limit/num_cl))
        ll <- split(player_info, player_info$k)
        parallel::clusterExport(cl, list("cols", "%>%"), envir = environment())
        
        x <- parallel::parLapply(cl, ll, function(l) {
            x <- mapply(SIMPLIFY = FALSE,
                        account_id = l$account_id,
                        start_time = l$start_time,
                        match_id = l$match_id, 
                        function(account_id, start_time, match_id) {
                            m2 <- mongolite::mongo(collection = "player", db = "dota2")
                            
                            query <- paste0('{"_id":', account_id, ', "details.start_time": {"$lt": ',
                                            start_time,'}}')
                            
                            fields <- paste0('"details.', c(cols[-c(17:19)], "radiant_win", "position", 
                                                            "ability_upgrades", "start_time"), '": 1 ')
                            fields <- paste0(fields, collapse = ", ")
                            fields <- paste0("{", fields, "}")
                            
                            x <- m2$find(query, fields)$details[[1]] %>% dplyr::as_tibble()
                            
                            m2$disconnect()
                            
                            if (nrow(x) > 0 & is.data.frame(x)) {
                                if ((length(x$ability_upgrades) > 0) &
                                    is.list(x$ability_upgrades)) {

                                    ability_nrow <- sapply(lapply(x$ability_upgrades, nrow),
                                                           function(x) {
                                                               ifelse(is.null(x), 0, x)
                                                           })
                                    ## nrow>10
                                    if (sum((ability_nrow >= 10)) >= 10) {
                                        x <- x[sapply(x$ability_upgrades, is.data.frame), ]
                                        x <- x[sapply(x$ability_upgrades, nrow) >= 10, ]
                                    } 
                                    ## last 10 matches
                                    x <- x[order(x$start_time, decreasing = TRUE), ]
                                    x <- x[1:(ifelse(nrow(x) < 10, nrow(x), 10)), ]
                                    
                                    
                                    # cria variaveis summarisadas da variável ability_upgrades
                                    x$ability_upgrades_mean <- sapply(x$ability_upgrades, function(x) {
                                        if (is.data.frame(x)) {
                                            mean(c(x$time[1], diff(x$time)))
                                        } else {
                                            NA
                                        }
                                    })

                                    x$ability_upgrades_var <- sapply(x$ability_upgrades, function(x) {
                                        if (is.data.frame(x)) {
                                            var(c(x$time[1], diff(x$time)))
                                        } else {
                                            NA
                                        }
                                    })
                                    # Calcula se o jogador venceu a partida
                                    x$win_player <- as.integer((isTRUE(x$radiant_win) & x$position <= 5) |
                                                               (!isTRUE(x$radiant_win) & x$position > 5))

                                    x %>%
                                        dplyr::select(!!cols) %>%
                                        dplyr::mutate(match_id = match_id)
                                } else {
                                    x$ability_upgrades_mean <- NA
                                    x$ability_upgrades_var <- NA
                                    x$win_player <- as.integer((isTRUE(x$radiant_win) & x$position <= 5) |
                                                               (!isTRUE(x$radiant_win) & x$position > 5))


                                    x %>%
                                        dplyr::select(!!cols) %>%
                                        dplyr::mutate(match_id = match_id)
                                }
                            } else {
                                NULL
                            }
                        })
        })
        parallel::stopCluster(cl)
        gc(reset = TRUE)
        
    } else {
        x <- mapply(SIMPLIFY = FALSE,
                    account_id = player_info$account_id,
                    start_time = player_info$start_time,
                    match_id = player_info$match_id, 
                    function(account_id, start_time, match_id) {
                        m2 <- mongolite::mongo(collection = "player", db = "dota2")
                        query <- paste0('{"_id":', account_id, ', "details.start_time": {"$lt": ',
                                        start_time,'}}')
                        
                        fields <- paste0('"details.', c(cols[-c(17:19)], "radiant_win", "position", 
                                                        "ability_upgrades", "start_time"), '": 1 ')
                        fields <- paste0(fields, collapse = ", ")
                        fields <- paste0("{", fields, "}")
                        
                        x <- m2$find(query, fields)$details[[1]] %>%
                                                  dplyr::as_tibble()
                        m2$disconnect()
                        
                        if (nrow(x) > 0 & is.data.frame(x)) {
                            if ((length(x$ability_upgrades) > 0) &
                                is.list(x$ability_upgrades)) {

                                ability_nrow <- sapply(lapply(x$ability_upgrades, nrow),
                                                       function(x) {
                                                           ifelse(is.null(x), 0, x)
                                                       })
                                ## nrow>10
                                if (sum((ability_nrow >= 10)) >= 10) {
                                    x <- x[sapply(x$ability_upgrades, is.data.frame), ]
                                    x <- x[sapply(x$ability_upgrades, nrow) >= 10, ]
                                } 
                                ## last 10 matches
                                x <- x[order(x$start_time, decreasing = TRUE), ]
                                x <- x[1:(ifelse(nrow(x) < 10, nrow(x), 10)), ]
                                
                                
                                # cria variaveis summarisadas da variável ability_upgrades
                                x$ability_upgrades_mean <- sapply(x$ability_upgrades, function(x) {
                                    if (is.data.frame(x)) {
                                        mean(c(x$time[1], diff(x$time)))
                                    } else {
                                        NA
                                    }
                                })

                                x$ability_upgrades_var <- sapply(x$ability_upgrades, function(x) {
                                    if (is.data.frame(x)) {
                                        var(c(x$time[1], diff(x$time)))
                                    } else {
                                        NA
                                    }
                                })
                                # Calcula se o jogador venceu a partida
                                x$win_player <- as.integer((isTRUE(x$radiant_win) & x$position <= 5) |
                                                           (!isTRUE(x$radiant_win) & x$position > 5))

                                # i <<- i + 1
                                x %>%
                                    dplyr::select(!!cols) %>%
                                    dplyr::mutate(match_id = match_id)
                            } else {
                                x$ability_upgrades_mean <- NA
                                x$ability_upgrades_var <- NA
                                x$win_player <- as.integer((isTRUE(x$radiant_win) & x$position <= 5) |
                                                           (!isTRUE(x$radiant_win) & x$position > 5))

                                # i <<- i + 1

                                x %>%
                                    dplyr::select(!!cols) %>%
                                    dplyr::mutate(match_id = match_id)
                            }
                        } else {
                            # i <<- i + 1
                            NULL
                        }
                    })
    }


    x <- lapply(x, dplyr::bind_rows)
    x <- dplyr::as_tibble(dplyr::bind_rows(x))

    x <- x %>%
        dplyr::group_by(account_id, match_id) %>%
        dplyr::mutate(n =  dplyr::n())  %>%
        dplyr::summarise_all(.funs = dplyr::funs(mean, var)) %>%
        dplyr::left_join(player_info[, c("account_id", "hero_id", "team", "match_id", "start_time")],
                         by = c("account_id", "match_id")) %>% 
        dplyr::as_tibble() %>%
        dplyr::arrange(match_id, team) %>%
        dplyr::select(-n_var) %>%
        dplyr::rename(n = n_mean) %>%
        dplyr::select(match_id, account_id, start_time, hero_id, team, dplyr::everything())

    DBI::dbWriteTable(con, "jogador", x, row.names = FALSE, append = TRUE)
    info <- DBI::dbGetQuery(con, "show index from dota2.jogador where Key_name='PRIMARY'")
    if (nrow(info) == 0) {
        DBI::dbSendQuery(con, "alter table dota2.jogador add primary key (match_id,account_id)")
    }
    
    x2 <- df %>%
        dplyr::select(match_id, radiant_win, start_time, duration, pre_game_duration, match_seq_num,
                      lobby_type, game_mode, leagueid, flags, engine, radiant_score, dire_score,
                      players_available) %>%
        dplyr::mutate(radiant_win = as.integer(radiant_win))

    DBI::dbWriteTable(con, "partida", x2, row.names = FALSE, append = TRUE)
    info <- DBI::dbGetQuery(con, "show index from dota2.partida where Key_name='PRIMARY'")
    if (nrow(info) == 0) {
        DBI::dbSendQuery(con, "alter table dota2.partida add primary key (match_id)")
    }

    DBI::dbDisconnect(con)
    invisible(gc(reset = TRUE))
    print(proc.time() - ptm)
}
