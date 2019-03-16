source("packages.R")
setwd("~/Dota2/data")
options(dplyr.width = Inf)


estimate_hero <- function(limit = Inf) {
    m <- mongo("match", "dota2")
    
    df_hero <- tibble()
    df_heroa <- tibble()
    N_total <- 0
    match_id <- NULL
    
    while (TRUE) {
        match_id_search <- ifelse(is.null(match_id), "[]", toJSON(match_id))

        df <- m$find(paste0('{"_pi": 1, "game_mode": 22,',
                            '"match_id":{ "$nin":', match_id_search,' }}'),
                     paste0('{"radiant_win": 1, "players.ability_upgrades": 1,',
                            '"match_id": 1,  "players.hero_id": 1}'),
                     limit = 10000
                     )

        if (nrow(df) == 0 | length(match_id) >= limit) {
            out <- list(N = N_total,
                        hero = df_hero,
                        hero_ability = df_heroa
                        )
            break
        } else {
            
            w <- df$radiant_win
            h <- lapply(df$players, function(x) x$hero_id)

            heroes <- mapply(w = w,
                             h = h,
                             function(w, h) {
                                 tibble(radiant_win = w,
                                        hero_id = h)
                             }, SIMPLIFY = FALSE) %>%
                bind_rows() %>%
                ungroup()

            heroes <- heroes %>%
                group_by(hero_id) %>%
                summarise(win = sum(radiant_win), n = n()) %>%
                ungroup()

            a <- lapply(df$players, function(x) x$ability_upgrades)

            # which(sapply(a, function(x) any(do.call(c, lapply(x, is.null)))))
            
            heroesa <- mapply(h = h,
                              a = a,
                              function(a, h) {
                                  mapply(h2 = h,
                                         a2 = a,
                                         function(a2, h2) {
                                             if (is.null(a2)) {
                                                 tibble()
                                             } else {
                                                 a2 %>%
                                                     mutate(hero_id = h2) %>%
                                                     as_tibble()
                                             }
                                         }, SIMPLIFY = FALSE) %>%
                                      bind_rows()
                              }, SIMPLIFY = FALSE) %>%
                bind_rows() %>%
                group_by(hero_id, level, ability) %>%
                summarise(n = n()) %>%
                ungroup()

            ## match_ids
            match_id <- c(match_id, df$match_id)

            ## total
            N_total <- N_total + nrow(df)

            ## heroes
            df_hero <- bind_rows(df_hero, heroes) %>%
                group_by(hero_id) %>%
                summarise(win = sum(win),  ## vitorias
                          n = sum(n)) %>%  ## total de partidas
                ungroup()

            ## abilidades
            df_heroa <- bind_rows(df_heroa, heroesa) %>%
                group_by(hero_id, level, ability) %>%
                summarise(n = sum(n)) %>%
                ungroup()
        }
    }

    m$disconnect()

    out$N <- out$N
    
    out$hero <- out$hero %>%
        group_by(hero_id) %>%
        summarise(winrate = win/n,
                  ingame = n/out$N,
                  n = n)

    out$hero_ability <- out$hero_ability %>%
        group_by(hero_id, level) %>%
        mutate(N = sum(n)) %>%
        ungroup() %>% 
        group_by(hero_id, level, ability) %>%
        summarise(prob_up = n/N,
                  n = n,
                  N = N
                  )
    return(out)
    
}

out <- estimate_hero(100000)

