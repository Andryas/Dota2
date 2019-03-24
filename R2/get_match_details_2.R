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
        } else if (content$response[["status_code"]] %in% c(400, 404)) {
            content <- NULL
            break
        } else if (content$response[["status_code"]] %in% c(401, 403)) {
            content <- "keyapi"
            break
        } else {
            break
        }
    }

    return(content)
}
