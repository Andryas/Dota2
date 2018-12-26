# ------------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------------
library(processx)
library(mongolite)

## keyapi's
keyapi <- readRDS("~/DotA2/data/keyapi.RData")
n <- length(keyapi)

# Script to collecting details
path <- normalizePath("~/DotA2/R/get_details.R")

setwd("~/DotA2/data/id/")

# ------------------------------------------------------------------------------
# Mongodb
# ------------------------------------------------------------------------------
m <- mongo(collection = "match", db = "dota")   # Match
m2 <- mongo(collection = "player", db = "dota") # Player


# ------------------------------------------------------------------------------
# Log daily data
# ------------------------------------------------------------------------------
if (file.exists("../stat.RData")) {
    stat2 <- readRDS("../stat.RData")

    if (stat2[nrow(stat2), ]$date < Sys.Date()) {
        totmatch <- m$count()
        totplayer <- m2$count()

        stat <- data.frame(date = Sys.Date(),
                           Imatch = totmatch,
                           Iplayer = totplayer)


        saveRDS(rbind(stat2, stat), "../stat.RData")
    }
}

# ------------------------------------------------------------------------------
# Config files
# ------------------------------------------------------------------------------
# hasFiles <- list.files(pattern = paste0("id-", Sys.Date() - 1, "-[0-9]+.RData$"))
## Clean the files


file <- list.files(pattern = paste0("id-", Sys.Date() - 1, "-.RData$"))
id <- readRDS(file)
if (is.list(id)) id <- unlist(id)
id <- split(id, ceiling(seq_along(id) / ceiling((length(id) / n))))
invisible(
    lapply(1:n, function(x) {
        saveRDS(id[[x]],  paste0(gsub(".RData", "", files), x, ".RData"))
    })
)

files <- normalizePath(
    list.files(pattern = "id-[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]+.RData$")
)

# ------------------------------------------------------------------------------
# Process of collecting data
# ------------------------------------------------------------------------------

# Create a new enviroment for the process
envp <- new.env()

# Create process in the new enviroment
for (i in 1:n) {
    assign(paste0("p", i), process$new(path, c(keyapi[i], files[i])),
           envir = envp)
    Sys.sleep(4)
}

# ls(envp)
# envp$p1$is_alive()
# envp$p2$is_alive()
# envp$p3$is_alive()
# envp$p4$is_alive()
# envp$p5$is_alive()
# envp$p6$is_alive()
# envp$p7$is_alive()
# envp$p8$is_alive()

# envp$p1$kill()
# envp$p2$kill()
# envp$p3$kill()
# envp$p4$kill()
# envp$p5$kill()
# envp$p6$kill()
# envp$p7$kill()
# envp$p8$kill()


P <- ls(envp)
# N <- as.integer(gsub("[A-z]+", "", P))
EVAL <- function(x, ...) {eval(parse(text = paste0("envp$", x, ...)))}
# sapply(ls(envp), function(x) EVAL(x, "$get_cmdline()[7]"))

while (TRUE) {
    for (i in 1:length(P)) {
        if (!EVAL(P[i], "$is_alive()")) {
            assign(paste0("p", i),
                   process$new(path, c(keyapi[i], files[i])),
                   envir = envp
                   )
            Sys.sleep(3)
        }
    }

    ## Put the process

    Sys.sleep(60)
}
