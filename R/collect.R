#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------------
library(processx)
library(mongolite)

# ------------------------------------------------------------------------------
# Mongodb
# ------------------------------------------------------------------------------
m <- mongo(collection = "match", db = "dota")   # Match
m2 <- mongo(collection = "player", db = "dota") # Player

# ------------------------------------------------------------------------------
# Config files
# ------------------------------------------------------------------------------
keyapi <- readRDS("~/DotA2/data/keyapi.RData")
n <- length(keyapi)
path <- normalizePath("~/DotA2/R/get_details.R")
setwd("~/DotA2/data/id/")

## amazon
cmd <- readRDS("~/DotA2/data/cmd.RData")
system(eval(cmd[[1]])) # skill = 3
system(eval(cmd[[2]])) # Top Games ID's

topgameid <- normalizePath(
    list.files(pattern = paste0("id-pro-", Sys.Date() - 1, "-.RData$"))
)
file <- list.files(pattern = paste0("id-", Sys.Date() - 1, "-.RData$"))
id <- readRDS(file)
if (is.list(id)) id <- unlist(id)
id <- split(id, ceiling(seq_along(id) /
                        ceiling((length(id) / (n - length(topgameid))))))
invisible(
    lapply(1:(n - length(topgameid)), function(x) {
        saveRDS(id[[x]],  paste0(gsub(".RData", "", file), x, ".RData"))
    })
)
files <- normalizePath(
    list.files(pattern = paste0("id-", Sys.Date() - 1, "-[0-9]+.RData$"))
)
files <- c(files, topgameid)

# ------------------------------------------------------------------------------
# Creating Process to collect data
# ------------------------------------------------------------------------------
envp <- new.env()

for (i in 1:length(files)) {
    assign(paste0("p", i), process$new(path, c(keyapi[i], files[i])),
           envir = envp)
    Sys.sleep(4)
}

P <- ls(envp)
EVAL <- function(x, ...) {eval(parse(text = paste0("envp$", x, ...)))}

today  <- Sys.Date()

## Eval if the process is running in background
while (TRUE) {
    rmvP <- NULL

    if (length(P) == 0) break

    for (i in 1:length(P)) {
        if (!EVAL(P[i], "$is_alive()")) {
            if (file.exists(files[i])) {
                assign(P[i],
                       process$new(path, c(keyapi[i], files[i])),
                       envir = envp
                       )

                Sys.sleep(3)
            } else {
                rmvP <- c(rmvP, i)
            }
        }
    }

    if (!is.null(rmvP)) P <- P[-rmvP]

    if (today < Sys.Date()) {
        file.remove(list.files())
        break
    }

    Sys.sleep(60)
}
