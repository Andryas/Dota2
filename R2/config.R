#' @title Settings to collect Dota2 matches.
#'
#' @description This function is used to define the settings that will be used to collect the data,
#'     such as type of game, skill etc ...
#' 
#' @param key The api key obtained from Steam. If you don't have one please visit
#'  \url{https://steamcommunity.com/dev} in order to do so.
#' 
#' @param game_mode The game mode can be a vector with one or more of these values:
#' \itemize{
#'   \item 0 - Unknown
#'   \item 1 - All Pick
#'   \item 2 - Captain's Mode
#'   \item 3 - Random Draft
#'   \item 4 - Single Draft
#'   \item 5 - All Random
#'   \item 6 - Intro
#'   \item 7 - Diretide
#'   \item 8 - Reverse Captain's Mode
#'   \item 9 - The Greeviling
#'   \item 10 - Tutorial
#'   \item 11 - Mid Only
#'   \item 12 - Least Played
#'   \item 13 - Limited Heroes
#'   \item 14 - Compendium Matchmaking
#'   \item 15 - Custom
#'   \item 16 - Captain's Draft
#'   \item 17 - Balanced Draft
#'   \item 18 - Ability Draft
#'   \item 19 - Event
#'   \item 20 - Random Death Match
#'   \item 21 - 1v1 Mid
#'   \item 22 - All Draft
#'   \item 23 - Turbo
#'   \item 24 - Mutation
#' }
#'
#' (Default: 2, 22)
#' 
#' @param lobby_type The lobby type can be a vector with one or more of these values:
#' \itemize{
#'   \item 0 - Normal
#'   \item 1 - Practice
#'   \item 2 - Tournament
#'   \item 3 - Tutorial
#'   \item 4 - Coop Bots
#'   \item 5 - Ranked Team MM
#'   \item 6 - Ranked Solo MM
#'   \item 7 - Ranked
#'   \item 8 - 1v1 Mid
#'   \item 9 - Battle Cup
#' }
#' 
#' (Default: 7)
#' 
#' @param skill The skill bracket can be a vector with one or more of these values (except 0):
#' \itemize{
#'   \item 0 - Any (If is set with this value, just it will be used)
#'   \item 1 - Normal
#'   \item 2 - High
#'   \item 3 - Very High
#' }
#'
#' (Default: 3)
#' 
#' @param duration The minimum duration of a match in seconds. (Default: 900)
#' @param public_account_id The minimum of public account's id. (Default: 5)
#'
#' @return A message saying that the settings have been logged.
#'
#' @examples
#'
#' # An available API key
#' config(key = 'xxxxx-xxxxx')
#' @export


config <- function(key = NULL, game_mode = c(2, 22), lobby_type = 7, skill = 3, duration = 900, 
                   public_account_id = 5) {
    # Check keys
    x <- lapply(key, function(key) {
        x <- try(RDota2::get_heroes(key = key), silent=TRUE)

        if (class(x) == "try-error") x <- list(response = list(status_code = 1))

        if (x[["response"]][["status_code"]] == 200) {
            return(c(good = key))
        } else {
            return(c(bad = key))
        }
    })
    
    x <- do.call(c, x)
    
    if (any(unique(names(x)) == "bad")) {
        stop("BAD KEYS: ", paste0(as.vector(x[names(x) == "bad"]),collapse=", "), 
            '\n Please make sure these keys are working or try again.')
    }
    x <- as.vector(x[names(x) == "good"])

    if (length(x) == 0) stop("All keys are bad. Please check the keys or try again.")
    
    m <- mongolite::mongo("config", "teste")
    query <- paste0('{"_id": "config"}')
    update <- paste0('{ "$addToSet": { "keyapi": { "$each": ', jsonlite::toJSON(key), '}}}')
    m$update(query = query, update = update, upsert = TRUE)

    args <- list(game_mode, lobby_type, skill, public_account_id, duration)
    nargs <- c("game_mode", "lobby_type", "skill", "public_account_id", "duration")
    invisible(mapply(a = args, na = nargs, function(a, na) {
        update <- paste0('{ "$addToSet": { "', na, '": { "$each": ', jsonlite::toJSON(a), '}}}')
        m$update(query = query, update = update, upsert = TRUE)
    }, SIMPLIFY = FALSE))

    # print(as.list(m$find()[1, ]))
    cat("\n\n\tYour settings for collecting Dota 2 matches have been recorded.",
        "\n\tIf you want to change something in the future just re-run this",
        "\n\tfunction with the settings you want.\n\n")
    
    m$disconnect()
}
