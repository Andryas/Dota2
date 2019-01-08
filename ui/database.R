# ------------------------------------------------------------------------------
# Database
# ------------------------------------------------------------------------------
fluidRow(
    column(6, align = "center",
           box(width = 12,
               title = "Database",
               solidHeader = TRUE,
               status = "danger",
               infoBoxOutput("Nmatches", width = 6),
               infoBoxOutput("Nplayers", width = 6),
               fluidRow(
                   box(title = "Configurações de coleta",
                       width = 12, solidHeader = TRUE,
                       status = "danger",
                       column(4,
                              bsButton("save_database", "",
                                       icon = icon("save"),
                                       width = "80%"),
                              bsTooltip("save_database",
                                        "Salvar",
                                        "right",
                                        options = list(container = "body"))
                              ),
                       column(4,
                              bsButton("new_keyapi", "",
                                       icon = icon("plus"),
                                       width = "80%"),
                              bsTooltip("new_keyapi",
                                        "Add uma nova chave da API steam",
                                        "right",
                                        options = list(container = "body"))
                              ),
                       column(4,
                              bsButton("play_collect_details", "",
                                       icon = icon("play"),
                                       width = "80%"),
                              bsTooltip("play_collect_details",
                                        "Começar o processo de coleta de dados.",
                                        "right",
                                        options = list(container = "body")),
                              hidden(
                                  actionButton("stop_collect_details", "",
                                               icon = icon("stop"),
                                               width = "80%"),
                                  bsTooltip("stop_collect_details",
                                            "Parar o processo de coleta de dados",
                                            "right",
                                            options = list(container = "body"))
                              )
                              ),
                       # div(style = "margin-top: 10%;",
                       #     textInput("cmd_get_id",
                       #               "Comando para salvar os IDs.",
                       #               "", width = "100%")
                       #     ),
                       div(style = "margin-top: 10%;",
                           tags$b("Chaves da API STEAM"),
                           uiOutput("keyapi_output")
                           )
                       )
               ))
           ),
    column(6, align = "center",
           box(title = "Dicionário", width = 12,
               solidHeader = TRUE, status = "danger",
               DTOutput("dicionario")
               ))
)
