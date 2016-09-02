# bibliotecas necessarias

library(XML)
library(RCurl)
library(stringr)
library(ggplot2)
library(stats)

# url de interesse

url <- "https://en.wikipedia.org/wiki/List_of_Formula_One_drivers"

# baixar os dados localmente e extrair as tabelas da pagina

tabs <- getURL(url)
tabs <- readHTMLTable(tabs, stringsAsFactors=FALSE)

# criar o data frame pilotos, apenas com a tabela que interessa

pilotos <- tabs[[3]]
tail(pilotos)
pilotos <- pilotos[-dim(pilotos)[1], ] # retira a ultima linha da tabela

dim(pilotos)
names(pilotos)

pilotos.bk <- pilotos

# limpar a tabela

pilotos$Championships <- as.numeric(as.character(substring(str_replace(pilotos$Championships, "\\[.\\]", ""), 1, 1)))

for (j in 5:10){
  pilotos[, j] <- as.numeric(as.character(str_replace(pilotos[, j], "\\[.*\\]", "")))
}

pilotos.numeros <- pilotos[, c("Championships", "Entries", "Starts", "Poles", "Wins", "Podiums", "Fastest laps")]

# corrige os nomes dos pilotos

rownames(pilotos.numeros) <- gsub("^.+?,\\s*(\\w+)\\1(.+?)$", "\\1\\2", pilotos$Name)

# estatisticas sobre as colunas da tabela

summary(pilotos.numeros)

# correlacao entre pole positions e vitorias

cor.test(pilotos.numeros$Poles, pilotos.numeros$Wins, method="spearman")
ggplot(pilotos.numeros, aes(x=Poles, y=Wins)) + geom_point(size=2, alpha=0.25) + xlab("Pole Positions") + ylab("Vit�rias")

# clusterizacao feita por pilotos, tentando encontrar os mais parecidos entre si

# normalizacao das variaveis - sugestão de Gustavo Fontoura
pilotos.numeros.norm <- pilotos.numeros
pilotos.numeros.norm$Championships <- pilotos.numeros.norm$Championships/sum(pilotos.numeros$Championships)
pilotos.numeros.norm$Entries <- pilotos.numeros.norm$Entries/sum(pilotos.numeros$Entries)
pilotos.numeros.norm$Starts <- pilotos.numeros.norm$Starts/sum(pilotos.numeros$Starts)
pilotos.numeros.norm$Poles <- pilotos.numeros.norm$Poles/sum(pilotos.numeros$Poles)
pilotos.numeros.norm$Wins <- pilotos.numeros.norm$Wins/sum(pilotos.numeros$Wins)
pilotos.numeros.norm$Podiums <- pilotos.numeros.norm$Podiums/sum(pilotos.numeros$Podiums)
pilotos.numeros.norm$`Fastest laps` <- pilotos.numeros.norm$`Fastest laps`/sum(pilotos.numeros$`Fastest laps`)

pilotos.dist <- dist(pilotos.numeros.norm, method="manhattan")

pilotos.cluster <- hclust(pilotos.dist,method = "average")
pp <- as.dendrogram(pilotos.cluster)
plot(pp)

# pca

pca <- prcomp(pilotos.numeros, center=TRUE, scale.=TRUE)
summary(pca)

# graficos

# 5 clusters

plot(pca$x[, 1], pca$x[, 2], col=as.numeric(cutree(pilotos.cluster, k=5)), xlab="PC 1", ylab="PC 2")

# 4 clusters

plot(pca$x[, 1], pca$x[, 2], col=as.numeric(cutree(pilotos.cluster, k=4)), xlab="PC 1", ylab="PC 2")

# 3 clusters

plot(pca$x[, 1], pca$x[, 2], col=as.numeric(cutree(pilotos.cluster, k=3)), xlab="PC 1", ylab="PC 2")

# identificacao dos pilotos

pilotos.numeros[cutree(pilotos.cluster, k=3)==1, ]
pilotos.numeros[cutree(pilotos.cluster, k=3)==2, ]
pilotos.numeros[cutree(pilotos.cluster, k=3)==3, ]
