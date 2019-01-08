# --------------------------------------------------------------------------
# Database
# --------------------------------------------------------------------------
# Informações do banco de dados
output$Nmatches <- renderInfoBox({
    infoBox("Matches", value = m$info()$stats$count, color = "red", width = 6)
})

output$Nplayers <- renderInfoBox({
    infoBox("Players", value = m2$info()$stats$count, color = "red", width = 6)
})

# Configurações
output$keyapi_output <- renderUI({
    if (file.exists("data/keyapi.RData")) {
        keyapi <- readRDS("data/keyapi.RData")
        if (length(keyapi) == 0) keyapi <- NULL
    }

    if (!is.null(keyapi)) {
        lapply(1:length(keyapi), function(x) {
            textInput(paste0("keyapi", x + 50),
                      "",
                      value = keyapi[x],
                      placeholder = "Nova chave da API Steam"
                      )
        })
    } else {
        NULL
    }
})

observeEvent(input$new_keyapi, {
    insertUI(
        selector = "#play_collect_details",
        where = "afterEnd",
        ui = textInput(paste0("keyapi", input$new_keyapi),
                       "", placeholder = "Nova chave para a API da steam")
    )
})

observeEvent(input$save_database, {
    x <- reactiveValuesToList(input)
    print(x)
    x <- x[grepl("keyapi[0-9]+", names(x))]
    x <- lapply(x, function(y) {
        if (nchar(y) > 0) {
            y
        } else {
            NULL
        }
    })
    x <- as.character(unlist(x))
    saveRDS(x, "data/keyapi.RData")
    # saveRDS(input$cmd_get_id, "data/cmd.RData")
})

# Dicionário
output$dicionario <- renderDT({readRDS("data/dic.RData")})
