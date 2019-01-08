dashboardPagePlus(
    title = "DotA2",
    skin = "red",
    dashboardHeaderPlus(
        title = "DotA2",
        enable_rightsidebar = FALSE
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Bet", tabName = "bet", icon = icon("trophy")),
            menuItem("Model", tabName = "model", icon = icon("project-diagram")),
            menuItem("Analytics", tabName = "analytics", icon = icon("chart-pie")),
            menuItem("Database", tabName = "database", icon = icon("database"),
                     selected = TRUE)
        )
    ),
    dashboardBody(
        useShinyjs(),
        useShinyalert(),
        tabItems(
            # ------------------------------------------------------------------
            # BET
            # ------------------------------------------------------------------
            tabItem("bet",
                    column(6, align = "center",
                           h1("Torneios")),
                    column(6, align = "center",
                           h1("Partidas avulsas"))
                    ),

            # ------------------------------------------------------------------
            # Database
            # ------------------------------------------------------------------
            tabItem("database",
                    source(file.path("ui", "database.R"), local = TRUE)$value
                    )

        ),
        rightSidebar()
    )
)
