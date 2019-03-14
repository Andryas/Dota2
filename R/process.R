source("packages.R")
setwd("~/Dota2/data")
options(dplyr.width = Inf)

## limit multipler 10
process <- function(limit = 10000) {
    m <- mongo(collection = "match", db = "dota2")
    m2 <- mongo(collection = "player", db = "dota2")

    files <- list.files(pattern = "md_.+.rds")
    if (length(files) == 0) match_id <- "[]" else match_id <- jsonlite::toJSON(do.call(c, lapply(files, readRDS)))

    df <- m$find(paste0('{"_pi": 1, "players_available": 10, "match_id":{ "$nin":', match_id,' }}'), limit = limit)

    df <- as_tibble(df)

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
                                   "start_time", "team")] %>% as_tibble()

    cols <- c("account_id", "kills", "deaths", "assists", "last_hits", "denies",
              "gold_per_min", "xp_per_min", "hero_damage", "tower_damage",
              "hero_healing", "gold", "gold_spent", "scaled_hero_damage",
              "scaled_tower_damage", "duration", "ability_upgrades_mean",
              "ability_upgrades_var", "win_player"
              )

    num_cl <- detectCores()                     ## get all cores
    cl <- makeCluster(num_cl)           
    player_info$k <- rep(1:4, each = limit/4)      ## split in four folds
    ll <- split(player_info, player_info$k)
    clusterExport(cl, list("m", "m2", "cols"), envir = environment())  ## export objetcs
    # clusterExport(cl, list("m", "m2", "cols"))  ## export objetcs
    clusterEvalQ(cl, library(mongolite))        ## call library

    x <- parLapply(cl, ll, function(l) {

        x <- mapply(SIMPLIFY = FALSE,
               account_id = l$account_id,
               start_time = l$start_time, 
               function(account_id, start_time) {
                   x <- m2$find(paste0('{"_id":', account_id, ', "details.start_time": {"$lt": ',
                                       start_time,'}}'))$details[[1]]
                   
                   if (is.null(x) | !all(c(cols[-c(17:19)], "ability_upgrades") %in% names(x))) {
                       NULL
                   } else {
                       x <- x[order(x$start_time, decreasing = TRUE), ][1:(ifelse(nrow(x) < 10, nrow(x), 10)), ]
                       
                       # cria variaveis summarisadas da variável ability_upgrades
                       x$ability_upgrades_mean <- sapply(x$ability_upgrades,
                                                         function(x) {
                                                             if (is.null(x) | length(x) == 0 | nrow(x) == 0) NA
                                                             else mean(c(x$time[1], diff(x$time)))
                                                         })

                       x$ability_upgrades_var <- sapply(x$ability_upgrades,
                                                        function(x) {
                                                            if (is.null(x) | length(x) == 0 | nrow(x) == 0) NA
                                                            else var(c(x$time[1], diff(x$time)))
                                                        })

                       # Calcula se o jogador venceu a partida
                       x$win_player <- as.integer((isTRUE(x$radiant_win) & x$position <= 5) |
                                                  (!isTRUE(x$radiant_win) & x$position > 5))
                       
                       x[, cols]
                   }
               })
        
    })
    
    stopCluster(cl)
    
    x <- lapply(x, bind_rows)
    x <- as_tibble(bind_rows(x))

    x2 <- x %>%
        group_by(account_id) %>%
        summarise_all(.funs = funs(mean, var)) %>%
        mutate(n =  as.integer(table(x$account_id))) %>%
        left_join(player_info[, c("account_id", "hero_id", "team", "match_id")],
                  by = "account_id") %>% 
        as_tibble() %>%
        arrange(match_id, team)

    uq_match_id <- unique(x2$match_id)

    files <- list.files()
    # files <- "data_1.rds"
    files <- str_extract(files, "(?<=data_).+(?=.rds)")

    i <- ifelse(length(files) == 0, 1, max(as.integer(files), na.rm = TRUE) + 1)
    saveRDS(x2, paste0("data_", i, ".rds"))
    saveRDS(uq_match_id, paste0("md_", i, ".rds"))
    m$disconnect()
    m2$disconnect()
}
