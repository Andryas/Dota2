#!/usr/bin/env Rscript

# Libraries
library(mongolite)

# MongoDB
m <- mongo(collection = "match", db = "dota2")   ## Match

setwd("~/DotA2/data/id")

if (!any(grepl("players_available_-1", m$index()$name))) {
    ## Index the field start_time
    m$index(add = '{"players_available": -1}')
}

id <- m$find(query = '{"_pi": 0}',
             fields = paste0('{"_id": true,  "players": true, ',
                             '"start_time": true, "players_available": true}'),
             sort = '{"players_available": -1}',
             limit = 5000
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
