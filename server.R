shinyServer(function(input, output, session) {
    # --------------------------------------------------------------------------
    # Database
    # --------------------------------------------------------------------------
    source(file.path("server", "database.R"), local = TRUE)$value

})
