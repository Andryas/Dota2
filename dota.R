library(tidyverse)

options(dplyr.width = Inf)

x <- readRDS("heroes.RData")
x <- as_tibble(x)

split_atr <- function(x, atr) {
  ATR <- x %>% select(!!atr) %>% pull()
  out <- lapply(ATR, function(x) {
    r <- trimws(unlist(strsplit(x, "\\+")))
    m <- matrix(r, ncol = 2)
    m <- as.data.frame(m)
    colnames(m) <- c(atr, paste0("tx", atr))
    m
  })
  out <- do.call(rbind, out)
  out[,colnames(out)] <- sapply(out, function(x) as.numeric(as.character(x)))
  x <- x %>% select(-!!atr)
  out2 <- cbind(x, out)
  out2 %>% 
    as_tibble()
}

# Quebra atributos em (atr inicial) e (progressão do atr)
x <- split_atr(x, "int")
x <- split_atr(x, "agi")
x <- split_atr(x, "str")

# Trata coluna de atributos e status
x$atr <- lapply(x$atr, function(A) {
  lapply(strsplit(A$Damage,"-"), function(x) {
    x <- as.integer(x)
    m <- matrix(x, ncol = 2)
    m <- as.data.frame(m)
    colnames(m) <- c("Dmg_min", "Dmg_max")
    m
  }) %>% 
    bind_rows() %>% 
    cbind(A, .) %>% 
    select(-Damage) %>% 
    mutate(HitPoints = as.integer(gsub(',','',HitPoints)),
           Mana = as.integer(gsub(',','',Mana)),
           Armor = as.integer(Armor))
})

# Separa alcance da visão em alcance da visão de dia e noite
x <- lapply(lapply(strsplit(x$alcvis, "/"), trimws), function(x) {
  x <- as.integer(x)
  m <- matrix(x, ncol = 2)
  m <- as.data.frame(m)
  colnames(m) <- c("Vision_day","Vision_night")
  m
}) %>% 
  do.call(rbind, .) %>% 
  bind_cols(x, .) %>% 
  select(-alcvis)

x$alcatq <- as.integer(x$alcatq)
x$move <- as.integer(x$move)

# Similaridade dos heroes por função que desempenham ----------------------
x2 <- x %>% select(hero, bioroles)

bioroles <- lapply(strsplit(x2$bioroles, "-"), trimws)
bioroles <- unique(do.call(c, bioroles))

class_role <- function(role) {
  r <- role
  r <- trimws(unlist(strsplit(r, "-")))
  return(as.integer(bioroles %in% r))
}

m <- do.call(rbind, lapply(x2$bioroles, class_role))
m <- as.data.frame(m)
colnames(m) <- bioroles
x2 <- cbind(x2 %>% select(-bioroles), m) %>% as_tibble()

library(cluster)
library(factoextra)

x3 <- column_to_rownames(x2, "hero")

distance <- get_dist(x3)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


# Similaridade dos heroes por atributo e status ---------------------------
x4 <- x %>% select(hero, move, int, txint, agi, txagi, str, txstr, alcatq)
x4 <- column_to_rownames(x4, "hero")
x4 <- scale(x4)
distance2 <- get_dist(x4)
fviz_dist(distance2, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))



# Dendogramas -------------------------------------------------------------
library(vegan)

dendo <- vegdist(x3, method = "jaccard")
clust1 <- hclust(dendo, method="average")

groups1 <- cutree(clust1, k = 4)
sort(groups1)


dendo2 <- vegdist(x4, method = "manhattan")
plot(hclust(dendo2, method="centroid"), hang=-1)
clust2 <- hclust(dendo2, method = "average")
groups2 <- cutree(clust2, k = 3)
sort(groups2)
