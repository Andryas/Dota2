limit <- 10
`%>%` <-  magrittr::`%>%`

m <- mongolite::mongo("match", "teste")

query <- '{"_pi": 0}'
fields <- '{"players": 1, "start_time": 1}'

data <- m$find(query = query, fields = fields, limit = limit) %>%
    dplyr::as_tibble()

data$players <- lapply(data$players, function(x) {
    x %>%
        dplyr::select(account_id, hero_id) %>%
        dplyr::as_tibble()
})

data <- tidyr::unnest(data, players)
data <- data %>% dplyr::rename(id = `_id`)

# remove private account id
data <- data %>%
    dplyr::filter(account_id != 4294967295)

ids <- split(data, data$id)

for (i in i:nrow(data)) {
    p <- ids[[i]]

    for (j in 1:nrow(p)) {

        history <-  try(
            R.utils::withTimeout(
                         get_match_history(
                             account_id = p$account_id[j],
                             start_at_match_id = p$match_id[j]
                         ),
                         timeout = 10,
                         onTimeout = "silent"
                     ),
            silent = TRUE
        )

        # history2 <-  try(
        #     R.utils::withTimeout(
        #                  get_match_history(
        #                      account_id = p$account_id[j],
        #                      start_at_match_id = p$match_id[j],
        #                      hero_id = p$hero_id[j]
        #                  ),
        #                  timeout = 10,
        #                  onTimeout = "silent"
        #              ),
        #     silent = TRUE
        # )
        

        if (class(history) == "try-error") stop("Fail! Restarting... try-error")

        player_match_id <- sapply(history$content$matches,
                                  function(x) x$match_id)
        
        # player_match_id2 <- sapply(history2$content$matches,
        #                            function(x) x$match_id)
        
        player_match_id <- unlist(player_match_id)
        # player_match_id2 <- unlist(player_match_id2)

        # player_match_id <- unique(c(player_match_id2, player_match_id))

        #! Informação armazenada do jogador no MongoDB
        player_db <-  m2$find(
                             query = paste0('{"_id": ', p$account_id[j], '}'),
                             fields = paste0('{"details.match_id": true }')
                         )

        player_badid_db <- m2$find(
                                  query = paste0('{"_id": ', p$account_id[j], '}'),
                                  fields = paste0('{"badid": true }')
                              )

        #! Remove partidas já coletadas
        if (nrow(player_db) > 0) {
            player_match_id <- player_match_id[!(player_match_id %in%
                                                 player_db$details[[1]]$match_id)]
        }

        #! Remove partidas consideradas badid
        if (nrow(player_badid_db) > 0) {
            player_match_id <-
                player_match_id[!(player_match_id %in%
                                  player_badid_db$badid[[1]])]
        }

        ## Se não houver partidas historicas do jogador j
        if (length(player_match_id) == 0) {
            # tkconfigure(pb2, value=0, maximum = 10)
            # tkconfigure(l2, text = paste0("Jogador ", j, " de ", nrow(p)))
            # tkconfigure(pb2, value = 10)
            next
        }

        # tkconfigure(pb2, value=0, maximum = 10)

        #! Coleta partidas anteriores do jogador j
        w <- 0
        while (length(player_match_id) > 0 & w <= 10) {
            w <- w + 1
            # tkconfigure(l2, text = paste0("Jogador ", j, " de ", nrow(p)))
            # tkconfigure(pb2, value = w)

            content <-  try(
                R.utils::withTimeout(
                             get_match_details(match_id = player_match_id[1]),
                             timeout = 10,
                             onTimeout = "silent"
                         ),
                silent = TRUE
            )

            # https://partner.steamgames.com/doc/webapi_overview/responses
            # 200: success
            # 400: Bad request
            # 401: Unauthorized
            # 403: Forbidden
            # 404: Not found
            # 429: Too many requests
            # 500: Internet server error
            # 503: Service unvavailable

            if ("content" %in% names(content)) {
                if ("error" %in% names(content[["content"]])) {
                    m2$update(
                           query = paste0('{"_id":', p$account_id[j], '}'),
                           update =  toJSON(
                               list("$addToSet" = list(badid = content$content$match_id)),
                               auto_unbox = TRUE
                           ),
                           upsert = TRUE
                       )
                    player_match_id <- player_match_id[-1]
                    next
                }
            }

            if (content$response[["status_code"]] %in% c(401, 403)) {
                stop(paste0("Access is denied. Retrying will not help.",
                            "Please verify your key= parameter."))
            }

            if (content$response[["status_code"]] %in% c(503, 429, 500)) {
                Sys.sleep(60)
                next
            }

            if (content$response[["status_code"]] %in% c(400, 404)) {
                m2$update(
                       query = paste0('{"_id":', p$account_id[j], '}'),
                       update =  toJSON(
                           list("$addToSet" = list(badid = content$content$match_id)),
                           auto_unbox = TRUE
                       ),
                       upsert = TRUE
                   )
                player_match_id <- player_match_id[-1]
                next
            }

            ## Lobby_type & game_mode rules
            if (!(content$content$lobby_type == p$lobby_type) &
                !(content$content$game_mode ==  p$game_mode)) {
                # m2$update(
                #        query = paste0('{"_id":', p$account_id[j], '}'),
                #        update =  toJSON(
                #            list("$addToSet" = list(badid = content$content$match_id)),
                #            auto_unbox = TRUE
                #        ),
                #        upsert = TRUE
                #    )
                player_match_id <- player_match_id[-1]
                next
            }

            player <- content$content$players

            ## If the match lasted less than 900 seconds DELETE
            if (content$content$duration <= 900 | length(player) != 10) {
                m2$update(
                       query = paste0('{"_id":', p$account_id[j], '}'),
                       update =  toJSON(
                           list("$addToSet" = list(badid = content$content$match_id)),
                           auto_unbox = TRUE
                       ),
                       upsert = TRUE
                   )
                player_match_id <- player_match_id[-1]
                next
            }

            ## Processing players data

            player <- lapply(player, function(x) {
                if ("ability_upgrades" %in% names(x)) {
                    players_au <<- x$ability_upgrades
                    cbind.data.frame(x[-which(names(x) == "ability_upgrades")])
                } else {
                    players_au <<- NULL
                    cbind.data.frame(x)
                }}) %>%
                bind_rows() %>%
                mutate(position = 1:10)

            player <- player %>%
                filter(account_id == p$account_id[j]) %>%
                mutate(
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

            #! Add Player
            m2$update(
                   query = paste0('{"_id":', p$account_id[j], '}'),
                   update =  toJSON(player, auto_unbox = TRUE),
                   upsert = TRUE
               )

            player_match_id <- player_match_id[-1]
            # tcl("update")
        }
    }


    m$update(paste0('{"_id":', p$match_id[1], '}'), '{"$set": {"_pi": 1}}')

}
