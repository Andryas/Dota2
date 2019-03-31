#' @title A list of matches
#'
#' @description This function is a wrapper of RDota2::get_match_history. The purpose of this
#'     function is to try to collect the history of matches until your success or fail, without
#'     returning a error.
#'
#' 
get_match_history_2 <- function(...) {
    while (TRUE) {
        if (isTRUE(curl::has_internet())) {
            content <-  R.utils::withTimeout(
                                     RDota2::get_match_history(...), 
                                     timeout = 3,
                                     onTimeout = "silent"
                                 )
            
            if ("content" %in% names(content)) {
                if ("error" %in% names(content[["content"]])) {
                    content <- NULL
                    break
                } 
            }
            
            if (content$response[["status_code"]] %in% c(429, 503, 500, 777)) {
                Sys.sleep(30)
            } else if (content$response[["status_code"]] %in% c(400, 404, 401, 403)) {
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
