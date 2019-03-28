#' @title Get the information about a specific match
#'
#' @description This functions is a wrapper of RDota2::get_match_details. The advantage of this
#'     function is that will be trying to collect the data until a sucess or a fail.
#'
#' @param ... Args from RDota2::get_match_details
#'
#' @seealso \code{\link[RDota2]{get_match_details}}

get_match_details_2 <- function(...) {
    while (TRUE) {
        content <-  R.utils::withTimeout(
                                 RDota2::get_match_details(...),
                                 timeout = 3,
                                 onTimeout = "error"
                             )
        
        if ("content" %in% names(content)) {
            if ("error" %in% names(content[["content"]])) {
                content <- NULL
                break
            } ## rmv
        }
        
        if (content$response[["status_code"]] %in% c(429, 503, 500, 777)) {
            Sys.sleep(20)
        } else if (content$response[["status_code"]] %in% c(400, 404, 401, 403)) {
            content <- NULL
            break
        } else {
            break
        }
    }

    return(content)
}
