library(RDota2)
library(tidyverse)
library(mongolite)
library(DBI)

options(dplyr.width = Inf)

# KeyAPI
key_actions(action = "register_key",
            value = readRDS("~/DotA2/data/keyapi.RData")[7])

# ------------------------------------------------------------------------------
# Mongodb
# ------------------------------------------------------------------------------
m <- mongo(collection = "match", db = "dota2")
m2 <- mongo(collection = "player", db = "dota2")

# ------------------------------------------------------------------------------
# Mysql
# ------------------------------------------------------------------------------
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "dota2",
                 user = "andryas",
                 host = "127.0.0.1",
                 password = "211401ka")

if (dbExistsTable(con, "matches")) {
    matchidsql <- dbReadTable(con, "matches")$match_id
    matchidmongo <- m$find(fields = '{"match_id": true}')$match_id
    matchid <- matchidmongo[!(matchidmongo %in% matchidsql)]
} else {
    matchid <- m$find(fields = '{"match_id": true}')$match_id
}

# ------------------------------------------------------------------------------
# Configs
# ------------------------------------------------------------------------------
heroes_tb <- get_heroes()$content
# heroes <- heroes_tb$localized_name
heroes <- expand.grid(hero_id = heroes_tb$id,
                      team = c("team1", "team2"))

cols <- c("match_id", "_pi", "start_time", "radiant_win", "duration",
          "pre_game_duration", "first_blood_time", "lobby_type",
          "game_mode", "human_players", "radiant_score", "dire_score",
          "players_available"
          )


skip <- seq(from = 0, to = length(matchid), by = 25000)
skip2 <- lead(skip)
skip2 <- ifelse(is.na(skip2), length(matchid), skip2)
skip <- skip + 1


#-------------------------------------------------------------------------------
# MongoDB -> MySQL
# ------------------------------------------------------------------------------
for (i in 1:length(skip)) {
    df <- m$find(query = paste0('{"match_id": {"$in":  [',
                                paste0(matchid[skip[i]:skip2[i]],
                                       collapse = ","), ']}}',
                                collapse = "")) %>%
        as_tibble()

    df$radiant_win <- as.integer(df$radiant_win)

    df2 <- lapply(1:nrow(df), function(x) {
        match <- df[x, ] %>% select(cols)

        df[x, ]$players[[1]] %>%
            select(hero_id) %>%
            mutate(team = rep(c("team1", "team2"), each = 5),
                   i = 1) %>%
            merge(x = heroes,
                  y = .,
                  by = c("hero_id", "team"),
                  all = TRUE) %>%
            mutate(i = ifelse(is.na(i), 0, 1),
                   ff = paste0(team, "_", hero_id)) %>%
            arrange(team) %>%
            mutate(ff = factor(ff, unique(ff))) %>%
            select(ff, i) %>%
            spread(ff, i) %>%
            cbind(match, .)
    })

    df2 <- do.call(rbind, df2)

    dbWriteTable(con, "matches", df2, row.names = FALSE, append = TRUE)
}


PRIMARY <- dbGetQuery(con, "describe matches;")
if (PRIMARY[PRIMARY$Field == "match_id", "Key"] != "PRI") {
    dbSendQuery(con, "alter table matches add primary key (match_id)")
}
