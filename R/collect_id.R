#' @title Collect a massive amount of matches id's.
#'
#' @description This function is used to collected the matches id of games that is happing in this
#'     exactly moment to later collect the details of the match after finishing.
#' 
#' @param key The api key obtained from Steam. If you don't have one please visit
#'  \url{https://steamcommunity.com/dev} in order to do so.
#' 
#' @param skill Skill bracket.
#' \itemize{
#' \item 0 - Any
#' \item 1 - Normal
#' \item 2 - High
#' \item 3 - Very High
#' }
#'
#' (Default: 3)
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
#' @return  0 if it failed to collect the data and return 1 if success to
#'     collect the data and stored it in the MongoDB.
#'
#' @seealso \code{\link[RDota2]{get_match_history}}

collect_id <- function(key, skill = 3, lobby_type = c(7)) {
    if (length(skill) != 1) stop("Skill must be a number between 0-3")
    
    hero_id <- RDota2::get_heroes(key = key)$content$id
    
    id <- lapply(hero_id,  function(h) {
        out <- RDota2::get_match_history(
                           key = key, 
                           hero_id = h,
                           skill = skill,
                           min_players = 10
                       )
        
        sapply(out$content$matches, function(x) {
            f <- x$lobby_type %in% lobby_type
            x$match_id[f]
        })
        
    })

    id <- unique(unlist(do.call(c, id)))

    if (is.null(id)) {
        return(0)
    } else {
        m <- mongolite::mongo(paste0("match_id_", skill), "teste")

        query <- paste0('{"_id": ', as.integer(Sys.Date()), '}')
        update <- paste0('{ "$addToSet": { "match_id": { "$each": ',
                         jsonlite::toJSON(id), '}}}')
        m$update(query = query, update = update, upsert = TRUE)
        return(1)
    }
}

