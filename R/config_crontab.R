#' @title Schedule R scripts/processes with the cron scheduler.
#'
#' @description This function config the scripts to collect Dota2 data.
#' collect_id: everyday 00:10
#' collect_match_details: everyday 00:15
#' collect_playres_details: everyday 02:20
#'
#'
#' @export

config_crontab <- function() {
    if (length(setdiff("cronR", rownames(installed.packages()))) > 0) {
        install.packages(setdiff("cronR", rownames(installed.packages())))  
    }

    cmd_collect_id <- "Rscript -e \"RDota2Plus::collect('collect_id')\" &"
    cmd_collect_match_details <- "Rscript -e \"RDota2Plus::collect('collect_match_details')\" &"
    cmd_collect_players_details <- "Rscript -e \"RDota2Plus::collect('collect_players_details')\" &"

    cronR::cron_add(cmd_collect_id, id = "collect_id", at = "00:10")
    cronR::cron_add(cmd_collect_id, id = "collect_match_details", at = "00:15")
    cronR::cron_add(cmd_collect_id, id = "collect_players_details", at = "02:20")
    cat("\t\tDone\t\t")
}

