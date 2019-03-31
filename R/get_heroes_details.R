#' @title Details about each hero
#'
#' @description Details about each hero such as STR, AGI, INT, Range of day vision or night etc...
#'
#' @param key The api key obtained from Steam. If you don't have one please visit
#'  \url{https://steamcommunity.com/dev} in order to do so.
#'
#' @return A data frame where each row contains a hero and his/her attributes.
#'
#' @examples
#' df <- get_hero_detais(key = 'xxxxx-xxxxx')
#' head(df)
#' 
#' @export

get_heroes_details <- function(key) {
    `%>%` <-  dplyr::`%>%`
    
    heroes <- RDota2::get_heroes(key = key)$content
    heroes$name <- gsub("npc_dota_hero_", "", heroes$name, perl = TRUE)

    myhtmlparse <- function(url) {
        httr::GET(url) %>% 
            httr::content('text') %>% 
            xml2::read_html()
    }

    # core function to get the information about the hero
    scrap <- function(sel_hero) {
        url <- "http://www.dota2.com/heroes/"
        h <- myhtmlparse(url)
        
        # ==============================================================================
        # Informações sobre o Herói
        # ==============================================================================
        link <- h %>%
            rvest::html_nodes(xpath = paste0("//a[@id='link_", sel_hero, "']")) %>% 
            rvest::html_attr("href")

        h2 <- myhtmlparse(link)

        hero <- stringr::str_extract(sel_hero, "(?<=_).+")

        # Principais funções -----------------------------------------------------------
        bioroles <- h2 %>%
            rvest::html_nodes(xpath = "//p[@id='heroBioRoles']") %>%
            rvest::html_text() 

        # Atributos --------------------------------------------------------------------
        int <- h2 %>%
            rvest::html_nodes(xpath = "//div[@id='overview_IntVal']") %>%
            rvest::html_text()
            # str_extract("(?<=\\+\\s).+")

        agi <- h2 %>%
            rvest::html_nodes(xpath = "//div[@id='overview_AgiVal']") %>%
            rvest::html_text()
            # str_extract("(?<=\\+\\s).+")

        str <- h2 %>%
            rvest::html_nodes(xpath = "//div[@id='overview_StrVal']") %>%
            rvest::html_text()
            # str_extract("(?<=\\+\\s).+")

        move <- h2 %>%
            rvest::html_nodes(xpath = "//div[@id='overview_SpeedVal']") %>%
            rvest::html_text()

        tb <- h2 %>%
            rvest::html_nodes(xpath = "//div[@id='statsRight']//div[@class='statRowCol2W']") %>%
            rvest::html_text()

        alcvis <- tb[1]  # visão dia/noite
        alcatq <- tb[2]  # alcance do ataque
        velproj <- tb[3] # velocidade projetil

        # lvls 1 - 15 - 25
        atr <- h2 %>%
            rvest::html_nodes(xpath = "//div[@id='statsLeft']") %>%
            rvest::html_text() %>%
            stringr::str_replace_all("[\n+\t+]", "") %>%
            stringr::str_replace_all("\r", ";") %>%
            stringr::str_replace_all("\\s+", "") %>%
            stringr::str_replace_all(";;", ";") %>%
            strsplit(";") %>%
            .[[1]]

        atr <- if(any(atr == "")) atr[-which(atr == "")] else atr

        atr <- dplyr::as_tibble(matrix(atr, ncol = 4, byrow = TRUE)) %>%
            tidyr::gather(type, value, -V4) %>%
            tidyr::spread(V4, value) %>%
            dplyr::select(-type) %>%
            dplyr::select(Level, Damage, Armor, HitPoints, Mana)


        ## Nome das magias
        magias <- h2 %>%
            rvest::html_nodes(xpath = "//div[@class='abilityHeaderRowDescription']/h2") %>%
            rvest::html_text() %>%
            stringr::str_replace_all("\r|\n|\t", "")

        ## Descrição
        desc <- h2 %>%
            rvest::html_nodes(xpath = "//div[@class='abilityHeaderRowDescription']/p") %>%
            rvest::html_text() %>%
            stringr::str_replace_all("\r|\n|\t", "")

        ability_desc <- dplyr::tibble(ability = magias,
                                      desc = desc)

        # ability1 <- h2 %>%
        #     html_node(xpath = "//div[@class='abilityFooterBox']") %>%
        #     html_node(xpath = "div[@class='abilityFooterBoxLeft']") %>%
        #     rvest::html_text() %>% 
        #     stringr::str_replace_all("\r|\n|\t", " ") %>%
        #     str_extract_all("[A-Z|\\s]+(?=:)") %>%
        #     do.call(c, .) %>%
        #     trimws()
        # 
        # ability2 <- h2 %>%
        #     html_node(xpath = "//div[@class='abilityFooterBox']") %>%
        #     html_node(xpath = "div[@class='abilityFooterBoxLeft']//span") %>%
        #     rvest::html_text()
        # 
        # ability3 <- h2 %>%
        #     rvest::html_nodes(xpath = "//div[@class='abilityFooterBox']") %>%
        #     rvest::html_nodes(xpath = "div[@class='abilityFooterBoxRight']") %>%
        #     rvest::html_text() %>% 
        #     stringr::str_replace_all("\r|\n|\t", " ") %>%
        #     str_extract_all("[A-Z|\\s]+(?=:)") %>%
        #     do.call(c, .) %>%
        #     trimws()
        # 
        # ability4 <- h2 %>%
        #     rvest::html_nodes(xpath = "//div[@class='abilityFooterBox']") %>%
        #     rvest::html_nodes(xpath = "div[@class='abilityFooterBoxRight']//span[@class='attribVal']") %>%
        #     rvest::html_text()
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

        
        atr <- tidyr::nest(atr, -hero, .key = "atr")
        ability_desc <- tidyr::nest(ability_desc, -hero, .key = "ability_desc")
        # ability <- nest(ability, -hero, .key = "ability")

        ## Scrap final
        scrap <- dplyr::tibble(hero = sel_hero,
                               bioroles = bioroles,
                               int = int,
                               agi = agi,
                               str = str,
                               move = move,
                               alcvis = alcvis, # alcance da visao dia/noite
                               alcatq = alcatq,  # alcance do ataque
                               velproj = velproj,
                               )

        scrap <- dplyr::left_join(scrap, atr, by = "hero")
        scrap <- dplyr::left_join(scrap, ability_desc, by = "hero")
        # scrap <- left_join(scrap, ability, by = "hero")


        return(scrap)
    }

    x <- lapply(heroes$name, scrap)
    x <- dplyr::bind_rows(x)
    x <- dplyr::left_join(x, heroes %>%
                             dplyr::rename(hero = "name") %>%
                             dplyr::select(hero, id), by = "hero")
    
    split_atr <- function(x, atr) {
        ATR <- x %>% dplyr::select(!!atr) %>% dplyr::pull()
        out <- lapply(ATR, function(x) {
            r <- trimws(unlist(strsplit(x, "\\+")))
            m <- matrix(r, ncol = 2)
            m <- as.data.frame(m)
            colnames(m) <- c(atr, paste0("tx", atr))
            m
        })
        out <- do.call(rbind, out)
        out[,colnames(out)] <- sapply(out, function(x) as.numeric(as.character(x)))
        x <- x %>% dplyr::select(-!!atr)
        out2 <- cbind(x, out)
        out2 %>% 
            dplyr::as_tibble()
    }

    # improve the atributtes of the hero
    x <- split_atr(x, "int")
    x <- split_atr(x, "agi")
    x <- split_atr(x, "str")

    # improve atr column
    x$atr <- lapply(x$atr, function(A) {
        lapply(strsplit(A$Damage,"-"), function(x) {
            x <- as.integer(x)
            m <- matrix(x, ncol = 2)
            m <- as.data.frame(m)
            colnames(m) <- c("Dmg_min", "Dmg_max")
            m
        }) %>% 
            dplyr::bind_rows() %>% 
            cbind(A, .) %>% 
            dplyr::select(-Damage) %>% 
            dplyr::mutate(HitPoints = as.integer(gsub(',','',HitPoints)),
                   Mana = as.integer(gsub(',','',Mana)),
                   Armor = as.integer(Armor))
    })

    # Split vision in day and night
    x <- lapply(lapply(strsplit(x$alcvis, "/"), trimws), function(x) {
        x <- as.integer(x)
        m <- matrix(x, ncol = 2)
        m <- as.data.frame(m)
        colnames(m) <- c("Vision_day","Vision_night")
        m
    }) %>% 
        do.call(rbind, .) %>% 
        dplyr::bind_cols(x, .) %>% 
    dplyr::select(-alcvis)
    
    # Create a vector to the mainly roles of the hero
    bioroles <- lapply(strsplit(x$bioroles, "-"), trimws)
    bioroles <- unique(do.call(c, bioroles))

    class_role <- function(role) {
        r <- role
        r <- trimws(unlist(strsplit(r, "-")))
        m <- matrix(as.integer(bioroles %in% r),
                    ncol = length(bioroles))
        m <- as.data.frame(m)
        colnames(m) <- bioroles
        m
    }

    x$bioroles <- lapply(x$bioroles, class_role)

    x$alcatq <- as.integer(x$alcatq)
    x$move <- as.integer(x$move)
    
    out <- x %>%
        dplyr::select(id, hero, dplyr::everything())

    return(out)
}
