########################################
#
#   CHAMANDO BIBLIOTECAS IMPORTANTES
#
########################################
install.packages("tidyverse")
install.packages("dendextend")
install.packages("gridExtra")
install.packages("readtextgrid")
library(tidyverse) #pacote para manipulacao de dados
library(cluster) #algoritmo de cluster
library(dendextend) #compara dendogramas
library(factoextra) #algoritmo de cluster e visualizacao
library(fpc) #algoritmo de cluster e visualizacao
library(gridExtra) #para a funcao grid arrange
library(readxl)
library(magrittr)#Para transformar linhas em colunas


########################################
#
#         CLUSTER HIERARQUICO 
#
########################################

#LEITURA DOS DADOS
BVFR_Analytics <- read_excel("C:/Users/bcorrea/Downloads/TCC-20221010T234045Z-001/TCC/dados/Base_Analytics_BVFR_2.xlsx")
View(BVFR_Analytics)
BVFR_Analytics_2 <- select(BVFR_Analytics,  everything(), -Data) # todas menos data
view(BVFR_Analytics_2)

#transformar o nome dos datas em linhas
BVFR_Analytics_3 <- data.frame(BVFR_Analytics_2)
print(BVFR_Analytics_2)
print(BVFR_Analytics_3)
library(magrittr)
BVFR_Analytics_3 <- BVFR_Analytics_3 %>% data.frame() %>% set_rownames(.$Recuperação.Nb2O5)
view(BVFR_Analytics_3)

#explorando base de dados

view(BVFR_Analytics_3)
glimpse(BVFR_Analytics_3)
dim(BVFR_Analytics_3)
names(BVFR_Analytics_3)

#Padronizar variaveis usando comando scale
BVFR_Analytics_3.padronizado <- scale(BVFR_Analytics_3)
view(BVFR_Analytics_3.padronizado)

#CALCULANDO MATRIZ DE DISTANCIAS
d <- dist(BVFR_Analytics_3.padronizado, method = "euclidean")


#DEFININDO O CLUSTER A PARTIR DO METODO ESCOLHIDO
#metodos disponiveis "average", "single", "complete" e "ward.D"
hc1 <- hclust(d, method = "single" )
hc2 <- hclust(d, method = "complete" )
hc3 <- hclust(d, method = "average" )
hc4 <- hclust(d, method = "ward.D" )

#DESENHANDO O DENDOGRAMA
plot(hc1, cex = 0.6, hang = -1)
plot(hc2, cex = 0.6, hang = -1)
plot(hc3, cex = 0.6, hang = -1)
plot(hc4, cex = 0.6, hang = -1)

# DENDOGRAMA PARA 10 GRUPOS
rect.hclust(hc4, k = 10)

#COMPARANDO DENDOGRAMAS
#comparando o metodo average com ward
dend3 <- as.dendrogram(hc3)
dend4 <- as.dendrogram(hc4)
dend_list <- dendlist(dend3, dend4) 
#EMARANHADO, quanto menor, mais iguais os dendogramas sao
tanglegram(dend3, dend4, main = paste("Emaranhado =", round(entanglement(dend_list),2)))
#agora comparando o metodo single com complete
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
dend_list2 <- dendlist(dend1, dend2) 
#EMARANHADO, quanto menor, mais iguais os dendogramas sao
tanglegram(dend1, dend2, main = paste("Emaranhado =", round(entanglement(dend_list2),2)))

#VERIFICANDO ELBOW 
fviz_nbclust(BVFR_Analytics_3.padronizado, FUN = hcut, method = "wss")

#criando 5 grupos de variáveis pelo metodo ward
grupo_var5 <- cutree(hc4, k = 5)
table(grupo_var5)

#transformando em data frame a saida do cluster
grupo_var2 <- data.frame(grupo_var5)
view(grupo_var2)

#juntando com a base original
Base_BVFR_fim <- cbind(BVFR_Analytics_3, grupo_var2)
view(Base_BVFR_fim)

# entendendo os clusters
#FAZENDO ANALISE DESCRITIVA
#MEDIAS das variaveis por grupo
mediagrupo <- Base_BVFR_fim %>% 
  group_by(grupo_var5) %>% 
  summarise(n = n(),
           Recuperação.Nb2O5 = mean(Recuperação.Nb2O5), 
           Disponibilidade.Física = mean(Disponibilidade.Física),
           Utilização.Física = mean(Utilização.Física),
           Moagem...Alimentação = mean(Moagem...Alimentação),
           Deslamagem...Pressão.3.2 = mean(Deslamagem...Pressão.3.2),
           Deslamagem...Pressão.Alim..Ciclone = mean(Deslamagem...Pressão.Alim..Ciclone),
           Deslamagem...Alim..Flot..Carbonatos = mean(Deslamagem...Alim..Flot..Carbonatos),
           Flot..Carbonatos...Alimentação.C520.CI01 = mean(Flot..Carbonatos...Alimentação.C520.CI01),
           Flot..Carbonatos...Alimentação.C520.TQ01 = mean(Flot..Carbonatos...Alimentação.C520.TQ01),
           Flot..Carbonatos...Alimentação.CF01 = mean( Flot..Carbonatos...Alimentação.CF01),
           Flot..Carbonatos...Alimentação.FTS = mean(Flot..Carbonatos...Alimentação.FTS),
           Flot..Carbonatos...Dosagem.Coletor.CF01 = mean(Flot..Carbonatos...Dosagem.Coletor.CF01),
           Flot..Carbonatos...Dosagem.Coletor.Total = mean(Flot..Carbonatos...Dosagem.Coletor.Total),
           Flot..Carbonatos...Dosagem.Depressor.C520.TQ01 = mean(Flot..Carbonatos...Dosagem.Depressor.C520.TQ01),
           Flot..Carbonatos...Dosagem.NaOH.C520.TQ01 = mean(Flot..Carbonatos...Dosagem.NaOH.C520.TQ01),
           Flot..Carbonatos...Dosagem.Stargil = mean(Flot..Carbonatos...Dosagem.Stargil),
           Flot..Carbonatos...Pressão.CI01 = mean(Flot..Carbonatos...Pressão.CI01),
           Flot..Carbonatos...Pressão.CI02 = mean(Flot..Carbonatos...Pressão.CI02),
           Flot..Carbonatos...Pressão.Desag.CI01 = mean(Flot..Carbonatos...Pressão.Desag.CI01),
           Flot..Carbonatos...Vazão.Afundado = mean(Flot..Carbonatos...Vazão.Afundado),
           Flot..Carbonatos...Vazão.Alimentação = mean(Flot..Carbonatos...Vazão.Alimentação),
           Flot..Carbonatos...Vazão.Ar.C520.CF03 = mean(Flot..Carbonatos...Vazão.Ar.C520.CF03),
           Flot..Carbonatos...Vazão.Ar.C520.CF04 = mean(Flot..Carbonatos...Vazão.Ar.C520.CF04),
           Flot..Carbonatos...Vazão.Ar.C520.CF05 = mean(Flot..Carbonatos...Vazão.Ar.C520.CF05),
           Flot..Carbonatos...Vazão.Ar.C520.CF06 = mean(Flot..Carbonatos...Vazão.Ar.C520.CF06),
           Flot..Carbonatos...Vazão.Ar.C520.CF07 = mean(Flot..Carbonatos...Vazão.Ar.C520.CF07),
           Flot..Carbonatos...Vazão.Ar.C520.CF08 = mean(Flot..Carbonatos...Vazão.Ar.C520.CF08),
           Flot..Carbonatos...Vazão.Coletor.C520.CF01 = mean(Flot..Carbonatos...Vazão.Coletor.C520.CF01),
           Flot..Carbonatos...Vazão.Coletor.C520.CF03 = mean(Flot..Carbonatos...Vazão.Coletor.C520.CF03),
           Flot..Carbonatos...Vazão.Coletor.Total = mean(Flot..Carbonatos...Vazão.Coletor.Total),
           Flot..Carbonatos...Vazão.Depressor.C520.TQ01 = mean(Flot..Carbonatos...Vazão.Depressor.C520.TQ01),
           Flot..Carbonatos...Vazão.NaOH.C520.TQ01 = mean(Flot..Carbonatos...Vazão.NaOH.C520.TQ01),
           Flot..Carbonatos...Vazão.Overflow = mean(Flot..Carbonatos...Vazão.Overflow),
           Flot..Carbonatos...Vazão.Stargil = mean(Flot..Carbonatos...Vazão.Stargil),
           Flot..Carbonatos...Vazão.Underflow = mean(Flot..Carbonatos...Vazão.Underflow),
           Flot..Sílica.Alimentação.C530.CI01 = mean(Flot..Sílica.Alimentação.C530.CI01),
           Flot..Sílica.Alimentação.CF01 = mean(Flot..Sílica.Alimentação.CF01),
           Flot..Sílica.Alimentação.FTC = mean(Flot..Sílica.Alimentação.FTC),
           Flot..Sílica.Dosagem.Coletor.CF01 = mean(Flot..Sílica.Dosagem.Coletor.CF01),
           Flot..Sílica.Dosagem.Coletor.Total = mean(Flot..Sílica.Dosagem.Coletor.Total),
           Flot..Sílica.Pressao.Desag. = mean(Flot..Sílica.Pressao.Desag.),
           Flot..Sílica.Rougher.CF03...Vazão.Ar = mean(Flot..Sílica.Rougher.CF03...Vazão.Ar),
           Flot..Sílica.Vazão.Alimentação = mean(Flot..Sílica.Vazão.Alimentação),
           Flot..Sílica.Vazão.Ar.C530.CF01 = mean(Flot..Sílica.Vazão.Ar.C530.CF01),
           Flot..Sílica.Vazão.Ar.C530.CF04 = mean(Flot..Sílica.Vazão.Ar.C530.CF04),
           Flot..Sílica.Vazão.Coletor.C530.CF01 = mean(Flot..Sílica.Vazão.Coletor.C530.CF01),
           Flot..Sílica.Vazão.Coletor.C530.CF05 = mean(Flot..Sílica.Vazão.Coletor.C530.CF05),
           Flot..Sílica.Vazão.NaOH.C530.TQ01 = mean(Flot..Sílica.Vazão.NaOH.C530.TQ01),
           Flot..Sílica.Vazão.Total.Coletor = mean(Flot..Sílica.Vazão.Total.Coletor),
           Flot..Sílica.Vazão.Underflow = mean(Flot..Sílica.Vazão.Underflow),
           Sep..Magnética.Alimentação = mean(Sep..Magnética.Alimentação),
           Sep..Magnética.Alimentação.Nova = mean(Sep..Magnética.Alimentação.Nova),
           Sep..Magnética.Pressão.CI01 = mean(Sep..Magnética.Pressão.CI01),
           Sep..Magnética.Vazão.Alimentação = mean(Sep..Magnética.Vazão.Alimentação),
           Sep..Magnética.Vazão.Magnético = mean(Sep..Magnética.Vazão.Magnético),
           Sep..Magnética.Vazão.Nâo.Magnético = mean(Sep..Magnética.Vazão.Nâo.Magnético),
           Flot..Nióbio...Alimentação.CF01 = mean(Flot..Nióbio...Alimentação.CF01),
           Flot..Nióbio...Dosagem.Acetadiamin = mean(Flot..Nióbio...Dosagem.Acetadiamin),
           Flot..Nióbio...Dosagem.Coletor.Total = mean(Flot..Nióbio...Dosagem.Coletor.Total),
           Flot..Nióbio...Vazão.Acetadiamin.Genagem = mean(Flot..Nióbio...Vazão.Acetadiamin.Genagem),
           Flot..Nióbio...Vazão.Alimentação.Rougher = mean(Flot..Nióbio...Vazão.Alimentação.Rougher),
           Flot..Nióbio...Vazão.Ar.C550.CF01 = mean(Flot..Nióbio...Vazão.Ar.C550.CF01),
           Flot..Nióbio...Vazão.Ar.C550.CF02 = mean(Flot..Nióbio...Vazão.Ar.C550.CF02),
           Flot..Nióbio...Vazão.Ar.C550.CF03 = mean(Flot..Nióbio...Vazão.Ar.C550.CF03),
           Flot..Nióbio...Vazão.Ar.C550.CF05 = mean(Flot..Nióbio...Vazão.Ar.C550.CF05),
           Flot..Nióbio...Vazão.Ar.C550.CF08 = mean(Flot..Nióbio...Vazão.Ar.C550.CF08),
           Flot..Nióbio...Vazão.BB01 = mean(Flot..Nióbio...Vazão.BB01),
           Flot..Nióbio...Vazão.BB02 = mean(Flot..Nióbio...Vazão.BB02),
           Flot..Nióbio...Vazão.Carga.Circulante = mean(Flot..Nióbio...Vazão.Carga.Circulante),
           Flot..Nióbio...Vazão.Coletor.C550.CF01 = mean(Flot..Nióbio...Vazão.Coletor.C550.CF01),
           Flot..Nióbio...Vazão.Coletor.C550.CF03 = mean(Flot..Nióbio...Vazão.Coletor.C550.CF03),
           Flot..Nióbio...Vazão.Concentrado = mean(Flot..Nióbio...Vazão.Concentrado),
           Flot..Nióbio...Vazão.Concentrado.Final = mean(Flot..Nióbio...Vazão.Concentrado.Final),
           Flot..Nióbio...Vazão.Não.Magnético = mean(Flot..Nióbio...Vazão.Não.Magnético),
           Flot..Nióbio...Vazão.Rejeito = mean(Flot..Nióbio...Vazão.Rejeito)
           )
mediagrupo

########################################
#
#    CLUSTER NAO HIERARQUICO 
#
########################################

#AGRUPANDO DADOS PELO METODO NAO HIERARQUICO

#Rodar o modelo
BVFR_Analytics.k6 <- kmeans(BVFR_Analytics_3.padronizado, centers = 6)  

#Visualizar os clusters
fviz_cluster(BVFR_Analytics.k6, data = BVFR_Analytics_3.padronizado, main = "Cluster K6")

#Criar clusters
BVFR_Analytics.k3 <- kmeans(BVFR_Analytics_3.padronizado, centers = 3)
BVFR_Analytics.k5 <- kmeans(BVFR_Analytics_3.padronizado, centers = 5)
BVFR_Analytics.k6 <- kmeans(BVFR_Analytics_3.padronizado, centers = 6)
BVFR_Analytics.k10 <- kmeans(BVFR_Analytics_3.padronizado, centers = 10)

#Criar graficos
G1 <- fviz_cluster(BVFR_Analytics.k3, geom = "point", data = BVFR_Analytics_3.padronizado) + ggtitle("k = 3")
G2 <- fviz_cluster(BVFR_Analytics.k5, geom = "point", data = BVFR_Analytics_3.padronizado) + ggtitle("k = 5")
G3 <- fviz_cluster(BVFR_Analytics.k6, geom = "point", data = BVFR_Analytics_3.padronizado) + ggtitle("k = 6")
G4 <- fviz_cluster(BVFR_Analytics.k10, geom = "point", data = BVFR_Analytics_3.padronizado) + ggtitle("k = 10")

#Imprimir graficos na mesma tela
grid.arrange(G1, G2, G3, G4, nrow = 2)

#VERIFICANDO ELBOW 
fviz_nbclust(BVFR_Analytics_3.padronizado, kmeans, method = "wss")

#juntando dados
BVFR_Analitics_fit <- data.frame(BVFR_Analytics.k5$cluster)
BVFR_Analytics_5 <- read_excel("C:/Users/bcorrea/Downloads/TCC-20221010T234045Z-001/TCC/dados/Base_Analytics_BVFR_5.xlsx")

#Agrupar cluster e base
BVFR_AnalyticsFinal <-  cbind(BVFR_Analytics_5, BVFR_Analitics_fit$BVFR_Analytics.k5.cluster)
colnames(BVFR_AnalyticsFinal)[77] <- "cluster_5"
view(BVFR_AnalyticsFinal)





