#' @title A list of matches
#'
#' @description This functions is a wrapper of RDota2::get_match_history. The purpose of this
#'     function is to try to collect the history of matches until your success or fail, without
#'     returning a error.
#'
#' @param ... Args from RDota2::get_match_details
#'
#' @seealso \code{\link[RDota2]{get_match_details}}

get_match_history_2 <- function(...) {
    while (TRUE) {
        if (isTRUE(curl::has_internet())) {
            content <-  R.utils::withTimeout(
                                     RDota2::get_match_history(...), 
                                     timeout = 3,
                                     onTimeout = "silent"
                                 )
            if ("content" %in% names(history)) {
                if ("error" %in% names(history[["content"]])) {
                    content <- NULL
                    break
                } 
            }
            
            if (history$response[["status_code"]] %in% c(429, 503, 500, 777)) {
                Sys.sleep(30)
            } else if (history$response[["status_code"]] %in% c(400, 404, 401, 403)) {
                content <- NULL
                break
            } else {
                break
            }
        } else {
            Sys.sleep(2)
        }
    }

    return(content)
}
