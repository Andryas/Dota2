#!/usr/bin/env Rscript

# Libraries
library(processx)
library(mongolite)

# Mongodb
m <- mongo(collection = "match", db = "dota2")   # Match
m2 <- mongo(collection = "player", db = "dota2") # Player

# Configs
# 1: match or player
# 2: seq(1:5) (keyapi and files with IDs to collect)
args <- commandArgs(TRUE)

setwd("~/Dota2/id/")

match_script <- normalizePath("~/Documentos/projetos/dota2/R/get_details_match.R")
player_script <- normalizePath("~/Documentos/projetos/dota2/R/get_details_player.R")

if (!(args[1] %in% c("match", "player"))) {
    # print(eval(parse(text = args[2])))
    stop("First arg must be 'match' or 'player'")
}

# ------------------------------------------------------------------------------
# Creating Process to collect match
# ------------------------------------------------------------------------------
if (args[1] == "match") {
    envp <- new.env()

    files <- list.files(pattern = "id-[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]+.RData")
    files <- files[1:length(eval(parse(text = args[2])))]

    for (i in 1:length(files)) {
        assign(paste0("p", i), process$new(match_script, c(i, files[i])),
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
                           process$new(match_script, c(i, files[i])),
                           envir = envp
                           )

                    Sys.sleep(3)

                } else {
                    rmvP <- c(rmvP, i)
                }
            }
        }

        if (!is.null(rmvP)) {
            P <- P[-rmvP]
            files <- files[-rmvP]
        }

        if (Sys.Date() > today) stop("Another Day")

        Sys.sleep(60)
    }

    file.remove(list.files(pattern = paste0(gsub("id-|.RData", "", files), collapse = "|")))
}

# ------------------------------------------------------------------------------
# Creating Process to collect player
# ------------------------------------------------------------------------------
if (args[1] == "player") {

    envp <- new.env()

    for (i in eval(parse(text = args[2]))) {

        assign(paste0("p", i),
               process$new(player_script,
                           c(i, paste0("J", i, ".RData"))),
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
                if (file.exists(paste0("J", gsub("p", "", P[i]), ".RData"))) {
                    assign(P[i],
                           process$new(player_script,
                                       c(gsub("p", "", P[i]),
                                         paste0("J", gsub("p", "", P[i]), ".RData"))),
                           envir = envp)

                    Sys.sleep(3)
                } else {
                    rmvP <- c(rmvP, as.integer(gsub("p", "", P[i])))
                }
            }
        }

        if (!is.null(rmvP)) P <- P[-rmvP]

        if (today < Sys.Date()) stop("Another Day")

        Sys.sleep(60)
    }

    file.remove(list.files(pattern = paste0(gsub("id-|.RData", "", files), collapse = "|")))
}
