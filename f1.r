# bibliotecas necessarias

library(XML)
library(RCurl)
library(stringr)
library(ggplot2)

# url de interesse

url <- "https://en.wikipedia.org/wiki/List_of_Formula_One_drivers"

# baixar os dados localmente e extrair as tabelas da pagina

tabs <- getURL(url)

# obter os nomes dos pilotos corretamente

myFun <- function(x){
  if(length(y <- getNodeSet(x, ".//a")) > 0){
    # return data.frame
    title <- xpathSApply(x, ".//a", fun=xmlGetAttr, name="title")
    href  <- xpathSApply(x, ".//a", fun=xmlGetAttr, name="href")
    value <- xpathSApply(x, ".//a", fun=xmlValue)
    return(paste(value, collapse = ","))
  }
  xmlValue(x, encoding = "UTF-8")
}

pilotos.nomes <- readHTMLTable(tabs, stringsAsFactors=FALSE, elFun=myFun)[[3]][, 1]
pilotos.nomes <- pilotos.nomes[-length(pilotos.nomes)]

# criar o data frame pilotos, apenas com a tabela que interessa

tabs    <- getURL(url)
pilotos <- readHTMLTable(tabs, stringsAsFactors=FALSE)
pilotos <- pilotos[[3]]
pilotos <- pilotos[-dim(pilotos)[1], ] # retira a ultima linha da tabela

dim(pilotos)
names(pilotos)

head(pilotos)

# limpar a tabela

pilotos$Championships <- as.numeric(as.character(substring(str_replace(pilotos$Championships, "\\[.\\]", ""), 1, 1)))

for (j in 5:10){
  pilotos[, j] <- as.numeric(as.character(str_replace(pilotos[, j], "\\[.*\\]", "")))
}

pilotos.numeros <- pilotos[, c("Championships", "Entries", "Starts", "Poles", "Wins", "Podiums", "Fastest laps")]

rownames(pilotos.numeros) <- pilotos.nomes

# estatisticas sobre as colunas da tabela

summary(pilotos.numeros)

# clusterizacao feita por pilotos, tentando encontrar os mais parecidos entre si

# normalizacao das variaveis - sugestão de Gustavo Fontoura

pilotos.numeros.norm                <- pilotos.numeros
pilotos.numeros.norm$Championships  <- pilotos.numeros.norm$Championships/sum(pilotos.numeros$Championships)
pilotos.numeros.norm$Entries        <- pilotos.numeros.norm$Entries/sum(pilotos.numeros$Entries)
pilotos.numeros.norm$Starts         <- pilotos.numeros.norm$Starts/sum(pilotos.numeros$Starts)
pilotos.numeros.norm$Poles          <- pilotos.numeros.norm$Poles/sum(pilotos.numeros$Poles)
pilotos.numeros.norm$Wins           <- pilotos.numeros.norm$Wins/sum(pilotos.numeros$Wins)
pilotos.numeros.norm$Podiums        <- pilotos.numeros.norm$Podiums/sum(pilotos.numeros$Podiums)
pilotos.numeros.norm$`Fastest laps` <- pilotos.numeros.norm$`Fastest laps`/sum(pilotos.numeros$`Fastest laps`)

# calculo das distancias entre os pilotos

pilotos.dist <- dist(pilotos.numeros.norm, method="manhattan")

pilotos.cluster <- hclust(pilotos.dist, method="average")

plot(pilotos.cluster)

# pca

pca <- prcomp(pilotos.numeros, center=TRUE, scale.=TRUE)
summary(pca)

# graficos

pca.plot <- as.data.frame(pca$x)

# 3 clusters - melhor resultado

ggplot(pca.plot, aes(x=pca.plot[, 1], y=pca.plot[, 2], color=as.factor(cutree(pilotos.cluster, k=3)))) + geom_point(size=2) + xlab("Componente Principal 1") + ylab("Componente Principal 2") + scale_color_discrete(name ="Grupos de Pilotos", labels=c("Pilotos Normais", "Pilotos Bons", "Michael Schumacher"))

# identificacao dos pilotos

rownames(pilotos.numeros[cutree(pilotos.cluster, k=3)==1, ])
rownames(pilotos.numeros[cutree(pilotos.cluster, k=3)==2, ])
rownames(pilotos.numeros[cutree(pilotos.cluster, k=3)==3, ])
