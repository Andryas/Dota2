library(RMySQL)
library(mongolite)

# Mysql
con <- dbConnect(MySQL(),
                 db = "dota2", 
                 user = "Andryas"
                 )

# Mongo
m <- mongo(collection = "match", db = "dota2")


# Adicionar filtro com base nos dados que já estão armazenados
df <- m$find('{"_pi": 1}', limit = 25000)

# Seleciona variáveis ----------------------------------------------------------
cols <- c("match_id", "radiant_win", "duration", "start_time",
          "tower_status_radiant", "tower_status_dire",
          "barracks_status_radiant", "barracks_status_dire",
          "first_blood_time", "lobby_type", "game_mode",
          "radiant_score", "dire_score", "players_available")

# Informações da partida -------------------------------------------------------
match_info <- df[, cols]

match_info$radiant_win <- as.integer(match_info$radiant_win)

dbWriteTable(con, "match_info", match_info,
             append = TRUE, row.names = FALSE)

# Informações do jogador -------------------------------------------------------
player_info <- df$players

player_info <- mapply(x = player_info, i = df$match_id,
       function(x, i) {
           x$match_id <- i
           x
       })

player_info <- plyr::rbind.fill(player_info)

player_info <- player_info[, c("match_id", "account_id", "hero_id")]

dbWriteTable(con, "player_info", player_info,
             append = TRUE, row.names = FALSE)
