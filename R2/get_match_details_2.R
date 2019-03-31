#' @title Get the information about a specific match
#'
#' @description This function is a wrapper of RDota2::get_match_details. The purpose of this
#'     function is to try to collect the details matches until your success or fail, without
#'     returning a error.
#'
#' @param ... Args from RDota2::get_match_details
#'
#' @seealso \code{\link[RDota2]{get_match_details}}

get_match_details_2 <- function(...) {
    while (TRUE) {
        if (isTRUE(curl::has_internet())) {
            content <-  R.utils::withTimeout(
                                     RDota2::get_match_details(...),
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
                Sys.sleep(20)
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
