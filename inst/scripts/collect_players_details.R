#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

key <- args[1]

m <- mongolite::mongo("collect_account_id", "dota")
configs <- dplyr::as_tibble(m$find(paste0('{"_id": "', key,'"}')))
m$disconnect()

p <- dplyr::as_tibble(configs$account_id[[1]])

if (nrow(p) == 0) stop("")

game_mode <- configs$game_mode[[1]]
lobby_type <- configs$lobby_type[[1]]
skill <- configs$skill[[1]]
duration <- configs$duration[[1]]
public_account_id <- configs$public_account_id[[1]]
n_history_matches <- configs$n_history_matches[[1]]

`%>%` <-  dplyr::`%>%`

for (j in 1:nrow(p)) {

    history <-  RDota2Plus::get_match_history_2(
        account_id = p$account_id[j],
        start_at_match_id = p$match_id[j],
        key = key
    )

    if (is.null(history)) next
    
    ## Get only players match with the lobby_type: ...
    h <- lapply(history$content$matches, function(x) {
        x[x$lobby_type %in% lobby_type]
    })

    h <- h[sapply(h, length) == 7]

    last_heroes_played <- sapply(h, function(x) {
        index <- sapply(x$players, function(y) {
            out <- y$account_id == p$account_id[j]
            if (length(out) > 0) {
                return(out)
            } else {
                return(FALSE)
            }
        })
        index <- which(index)

        if (length(index) > 0) {
            return(x$players[[index]]$hero_id)
        } else {
            return(NULL)
        }
    })

    # last heroes played
    last_heroes_played <- as.data.frame.table(table(last_heroes_played), stringsAsFactors = FALSE)
    last_heroes_played <- last_heroes_played[order(last_heroes_played$Freq, decreasing = TRUE), ]
    names(last_heroes_played) <- c("hero_id", "n")
    last_heroes_played$match_id <- unique(p$match_id[j])
    # last_heroes_played$start_time <- p$start_time[j]

    last_heroes_played <- list("$addToSet" = list(
                                   last_heroes_played = last_heroes_played
                               ))
    
    player_match_id <- sapply(history$content$matches, function(x) x$match_id)
    player_match_id <- unlist(player_match_id)

    mp <- mongolite::mongo("players", "dota")

    mp$update(
           query = paste0('{"_id":', p$account_id[j], '}'),
           update =  jsonlite::toJSON(last_heroes_played, auto_unbox = TRUE),
           upsert = TRUE
       )

    #! Informação armazenada do jogador no MongoDB
    player_db <-  mp$find(
                         query = paste0('{"_id": ', p$account_id[j], '}'),
                         fields = paste0('{"details.match_id": true }')
                     )

    player_badid_db <- mp$find(
                              query = paste0('{"_id": ', p$account_id[j], '}'),
                              fields = paste0('{"badid": true }')
                          )

    # Delete match already collected
    if (nrow(player_db) > 0) {
        player_match_id <- player_match_id[!(player_match_id %in%
                                             player_db$details[[1]]$match_id)]
    }

    # Delete badid matches
    if (nrow(player_badid_db) > 0) {
        player_match_id <- player_match_id[!(player_match_id %in%
                                             player_badid_db$badid[[1]])]
    }

    w <- 0
    while (length(player_match_id) > 0 & w <= n_history_matches) {
        w <- w + 1

        content <- RDota2Plus::get_match_details_2(key = key, match_id = player_match_id[w])

        
        if (is.null(content)) {
            mp$update(
                   query = paste0('{"_id":', p$account_id[j], '}'),
                   update =  jsonlite::toJSON(
                                           list("$addToSet" = list(badid = content$content$match_id)),
                                           auto_unbox = TRUE
                                       ),
                   upsert = TRUE
               )
            player_match_id <- player_match_id[-1]
            next
        }

        player <- content$content$players

        ## If the match lasted less than 900 seconds DELETE
        if (content$content$duration <= duration | length(player) != 10) {
            mp$update(
                   query = paste0('{"_id":', p$account_id[j], '}'),
                   update =  jsonlite::toJSON(
                                           list("$addToSet" = list(badid = content$content$match_id)),
                                           auto_unbox = TRUE
                                       ),
                   upsert = TRUE
               )
            player_match_id <- player_match_id[-1]
            next
        }

        ## Processing players p
        player <- lapply(player, function(x) {
            if ("ability_upgrades" %in% names(x)) {
                players_au <<- x$ability_upgrades
                cbind.data.frame(x[-which(names(x) == "ability_upgrades")])
            } else {
                players_au <<- NULL
                cbind.data.frame(x)
            }}) %>%
            dplyr::bind_rows() %>%
            dplyr::mutate(position = 1:10)

        player <- player %>%
            dplyr::filter(account_id == p$account_id[j]) %>%
            dplyr::mutate(
                       duration = content$content$duration,
                       radiant_win = content$content$radiant_win,
                       start_time = content$content$start_time,
                       match_seq_num = content$content$match_seq_num,
                       match_id = content$content$match_id
                   )

        if (is.null(players_au)) {
            player <- list("$addToSet" = list(details = as.list(player)))
        } else {
            player <-
                list("$addToSet" =
                         list(
                             details = append(
                                 as.list(player),
                                 list(ability_upgrades =
                                          content$content$players[[player$position]]$ability_upgrades))))
        }

        mp$update(
               query = paste0('{"_id":', p$account_id[j], '}'),
               update =  jsonlite::toJSON(player, auto_unbox = TRUE),
               upsert = TRUE
           )
        
        player_match_id <- player_match_id[-1]
    }
    
    mp$disconnect() ## disconnect from player's collection

    ## Remove player from collect_account_id
    mp <- mongolite::mongo("collect_account_id", "dota")
    match_id <- mp$find(paste0('{"_id": "', key, '"}'))
    query <- paste0('{"_id": "', key, '"}')
    update <- paste0('{"$pull": {"account_id": {"match_id": ', p$match_id[j], ',',
                     ' "account_id": ', p$account_id[j], '}}}')
    mp$update(query = query, update = update)
    match_id <- unique(mp$find(paste0('{"_id": "', key, '"}'))$account_id[[1]]$match_id)
    mp$disconnect()
    
    if (!(p$match_id[j] %in% match_id)) {
        m <- mongolite::mongo("match", "dota")
        m$update(paste0('{"_id":', p$match_id[j], '}'), '{"$set": {"_pi": 1}}')
        m$disconnect() 
    }
}




