setwd("~/Dota2/data")

library(mongolite)

# Mongo
m <- mongo(collection = "match", db = "dota2")
m2 <- mongo(collection = "player", db = "dota2")

# match_id <- if (exists("match_id.RData")) readRDS("match_id.RData") else 0

df <- m$find('{"_pi": 1}', limit = 10000)

player_info <- df$players

player_info <- mapply(x = player_info, i = df$match_id, j = df$start_time, 
                      function(x, i, j) {
                          x$match_id <- i
                          x$start_time <- j
                          x$team <- rep(c("radiant","dire"), each = 5)
                          x
                      })

player_info <- plyr::rbind.fill(player_info)

player_info <- player_info[, c("match_id", "account_id", "hero_id",
                               "start_time", "team")]

# ==============================================================================
cols <- c("account_id", "kills", "deaths", "assists", "last_hits", "denies",
          "gold_per_min", "xp_per_min", "hero_damage", "tower_damage",
          "hero_healing", "gold", "gold_spent", "scaled_hero_damage",
          "scaled_tower_damage", "duration", "ability_upgrades_mean",
          "ability_upgrades_var", "win_player"
          )

# sum(player_info$account_id == 4294967295) ## account_id privado
teste <- player_info[1:10, ]


# Últimas 10 partidas
x <- mapply(SIMPLIFY = FALSE,
       account_id = teste$account_id,
       start_time = teste$start_time,
       function(account_id, start_time) {
           ## ADD
           x <- m2$find(paste0('{"_id":', account_id, ', "details.start_time": {"$lt": ',
                               start_time,'}}'))$details[[1]]
           x <- x[order(x$start_time, decreasing = TRUE), ][1:(ifelse(nrow(x) < 10, nrow(x), 10)), ]

           # cria variaveis summarisadas da variável ability_upgrades
           x$ability_upgrades_mean <- sapply(x$ability_upgrades,
                                             function(x) {
                                                 if (is.null(x)) NULL
                                                 else mean(c(x$time[1], diff(x$time)))
                                             })

           x$ability_upgrades_var <- sapply(x$ability_upgrades,
                                            function(x) {
                                                if (is.null(x)) NULL
                                                else var(c(x$time[1], diff(x$time)))
                                            })

           # Calcula se o jogador venceu a partida
           x$win_player <- as.integer((isTRUE(x$radiant_win) & x$position <= 5) |
                                      (!isTRUE(x$radiant_win) & x$position > 5))
           
           x[, cols]
       })

x <- do.call(rbind, x)

# library(ggplot2)
# names(x)
# 
# x$account_id <- as.factor(x$account_id)
# x$win_player <- as.factor(x$win_player)
# ggplot(x, aes(x = last_hits, y = gold_spent,
#               group = win_player, color = win_player)) +
#     geom_point() + facet_wrap(~account_id)

# Calcula estatísticas do jogador para cada atributo média e variância.
x2 <- aggregate(x[, 2:ncol(x)], by = list(account_id = x$account_id),
                FUN = function(x) c(MEAN = mean(x, na.rm = TRUE),
                                    VAR = var(x, na.rm = TRUE)))
x2$n <- as.integer(table(x$account_id))
x2 <- merge(x2, teste[, c("account_id", "hero_id", "team")], by = "account_id")
x2 <- x2[order(x2$team, decreasing = TRUE), ]


# upate db.match com estatísticas de partidas passadas de cada jogador presente
# na partida




