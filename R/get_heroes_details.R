if (!("pacman" %in% installed.packages()[, "Package"])) install.packages("pacman")
pacman::p_load(rvest, httr, tidyr, dplyr, stringr, RDota2)

get_heroes_details <- function(key) {

    heroes <- get_heroes(key = key)$content
    heroes$name <- gsub("npc_dota_hero_", "", heroes$name, perl = TRUE)

    myhtmlparse <- function(url) {
        GET(url) %>% 
            content('text') %>% 
            xml2::read_html()
    }

    scrap <- function(sel_hero) {
        url <- "http://www.dota2.com/heroes/"
        h <- myhtmlparse(url)
        
        # ==============================================================================
        # Informações sobre o Herói
        # ==============================================================================
        link <- h %>%
            html_nodes(xpath = paste0("//a[@id='link_", sel_hero, "']")) %>% 
            html_attr("href")

        h2 <- myhtmlparse(link)

        hero <- str_extract(sel_hero, "(?<=_).+")

        # Principais funções -----------------------------------------------------------
        bioroles <- h2 %>%
            html_nodes(xpath = "//p[@id='heroBioRoles']") %>%
            html_text() 

        # Atributos --------------------------------------------------------------------
        int <- h2 %>%
            html_nodes(xpath = "//div[@id='overview_IntVal']") %>%
            html_text()
            # str_extract("(?<=\\+\\s).+")

        agi <- h2 %>%
            html_nodes(xpath = "//div[@id='overview_AgiVal']") %>%
            html_text()
            # str_extract("(?<=\\+\\s).+")

        str <- h2 %>%
            html_nodes(xpath = "//div[@id='overview_StrVal']") %>%
            html_text()
            # str_extract("(?<=\\+\\s).+")

        move <- h2 %>%
            html_nodes(xpath = "//div[@id='overview_SpeedVal']") %>%
            html_text()

        tb <- h2 %>%
            html_nodes(xpath = "//div[@id='statsRight']//div[@class='statRowCol2W']") %>%
            html_text()

        alcvis <- tb[1]  # visão dia/noite
        alcatq <- tb[2]  # alcance do ataque
        velproj <- tb[3] # velocidade projetil

        # lvls 1 - 15 - 25
        atr <- h2 %>%
            html_nodes(xpath = "//div[@id='statsLeft']") %>%
            html_text() %>%
            str_replace_all("[\n+\t+]", "") %>%
            str_replace_all("\r", ";") %>%
            str_replace_all("\\s+", "") %>%
            str_replace_all(";;", ";") %>%
            strsplit(";") %>%
            .[[1]]

        atr <- if(any(atr == "")) atr[-which(atr == "")] else atr

        atr <- as_tibble(matrix(atr, ncol = 4, byrow = TRUE)) %>%
            gather(type, value, -V4) %>%
            spread(V4, value) %>%
            select(-type) %>%
            select(Level, Damage, Armor, HitPoints, Mana)


        ## Nome das magias
        magias <- h2 %>%
            html_nodes(xpath = "//div[@class='abilityHeaderRowDescription']/h2") %>%
            html_text() %>%
            str_replace_all("\r|\n|\t", "")

        ## Descrição
        desc <- h2 %>%
            html_nodes(xpath = "//div[@class='abilityHeaderRowDescription']/p") %>%
            html_text() %>%
            str_replace_all("\r|\n|\t", "")

        ability_desc <- tibble(ability = magias,
                               desc = desc)

        # ability1 <- h2 %>%
        #     html_node(xpath = "//div[@class='abilityFooterBox']") %>%
        #     html_node(xpath = "div[@class='abilityFooterBoxLeft']") %>%
        #     html_text() %>% 
        #     str_replace_all("\r|\n|\t", " ") %>%
        #     str_extract_all("[A-Z|\\s]+(?=:)") %>%
        #     do.call(c, .) %>%
        #     trimws()
        # 
        # ability2 <- h2 %>%
        #     html_node(xpath = "//div[@class='abilityFooterBox']") %>%
        #     html_node(xpath = "div[@class='abilityFooterBoxLeft']//span") %>%
        #     html_text()
        # 
        # ability3 <- h2 %>%
        #     html_nodes(xpath = "//div[@class='abilityFooterBox']") %>%
        #     html_nodes(xpath = "div[@class='abilityFooterBoxRight']") %>%
        #     html_text() %>% 
        #     str_replace_all("\r|\n|\t", " ") %>%
        #     str_extract_all("[A-Z|\\s]+(?=:)") %>%
        #     do.call(c, .) %>%
        #     trimws()
        # 
        # ability4 <- h2 %>%
        #     html_nodes(xpath = "//div[@class='abilityFooterBox']") %>%
        #     html_nodes(xpath = "div[@class='abilityFooterBoxRight']//span[@class='attribVal']") %>%
        #     html_text()
        # 
        # ability_1 <- c(ability1, ability3)
        # ability_2 <- c(ability2, ability4)
        # 
        # cbind(ability_1, ability_2)
        # 
        # ability <- tibble(ability = rep(magias, each = 4),
        #                   type = ability_1,
        #                   value = ability_2)

        atr$hero <- sel_hero
        ability_desc$hero <- sel_hero
        # ability$hero <- hero 

        atr <- nest(atr, -hero, .key = "atr")
        ability_desc <- nest(ability_desc, -hero, .key = "ability_desc")
        # ability <- nest(ability, -hero, .key = "ability")

        ## Scrap final
        scrap <- tibble(hero = sel_hero,
                        bioroles = bioroles,
                        int = int,
                        agi = agi,
                        str = str,
                        move = move,
                        alcvis = alcvis, # alcance da visao dia/noite
                        alcatq = alcatq,  # alcance do ataque
                        velproj = velproj,
                        )

        scrap <- left_join(scrap, atr, by = "hero")
        scrap <- left_join(scrap, ability_desc, by = "hero")
        # scrap <- left_join(scrap, ability, by = "hero")


        return(scrap)
    }

    x <- lapply(heroes$name, scrap)
    bind_rows(x)
}

# x <- get_heroes_details(key = readRDS("~/Dota2/keyapi.RData")[1])
# saveRDS(x, "~/Dota2/heroes.RData")
