if (!("pacman" %in% installed.packages()[, "Package"])) install.packages("pacman")
pacman::p_load(mongolite, stringr, dplyr, tidyr, parallel, plyr, jsonlite,
               rvest, httr, RDota2, processx, R.utils, lubridate,  parallel)
