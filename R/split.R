#!/usr/bin/env Rscript

args <- commandArgs(TRUE)

if (!(args[1] %in% c("match", "player"))) {
    # print(eval(parse(text = args[2])))
    stop("First arg must be 'match' or 'player'")
}

# Libraries
library(mongolite)

# MongoDB
m <- mongo(collection = "match", db = "dota2")   ## Match

setwd("~/DotA2/data/id")

if (args[1] == "player") {
    if (!any(grepl("players_available_-1", m$index()$name))) {
        ## Index the field start_time
        m$index(add = '{"players_available": -1"}')
    }

    id <- m$find(query = '{"_pi": 0, "game_mode": {"$in": [1, 22]}}',
                 fields = paste0('{"_id": true,  "start_time": true, ',
                                 '"players_available": true, "players": true}'),
                 sort = '{"players_available": -1}',
                 limit = 15000
                 )

    ids <- mapply(m = id$`_id`,
                  p = id$players,
                  s = id$start_time,
                  function(m, p, s) {
                      data.frame(match_id = m,
                                 account_id = p$account_id,
                                 hero_id = p$hero_id,
                                 start_time = s,
                                 stringsAsFactors = FALSE
                                 )
                  }, SIMPLIFY = FALSE)

    ids <- lapply(ids, function(x) {
        x[!(x$account_id %in% c(4294967295)), ]
    })

    ids <- split(ids, ceiling(seq_along(ids) / ceiling(length(ids)/5)))

    lapply(1:length(ids), function(x) saveRDS(ids[[x]], paste0("J", x, ".RData")))
    # lapply(ids, length)
}

if (args[1] == "match") {
    ## get_id.R file (AWS)
    cmd <- readRDS("~/DotA2/data/cmd.RData")
    system(eval(parse(text = cmd)))

    files <- list.files(pattern = "id-[0-9]{4}-[0-9]{2}-[0-9]{2}-.RData")

    if (length(files) == 0) stop("No Match files to process.")

    id <- unlist(readRDS(files[1]))

    id <- split(id, ceiling(seq_along(id) / ceiling(length(id)/5)))

    files2 <- sapply(1:length(id),
                    function(i) gsub(".RData", paste0(i, ".RData"), files))

    for (i in 1:length(files2)) {
        saveRDS(id[[i]], files2[i])
    }

    file.remove(files)
 }
