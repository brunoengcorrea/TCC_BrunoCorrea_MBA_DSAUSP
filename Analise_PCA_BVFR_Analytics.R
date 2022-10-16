
# Instalação e Carregamento de Todos os Pacotes ---------------------------

pacotes <- c("plotly","tidyverse","knitr","kableExtra","car","rgl","gridExtra",
             "PerformanceAnalytics","reshape2","rayshader","psych","ggrepel",
             "factoextra","sp","tmap","magick","gridExtra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



# Proposta de elaboração de um ranking e plotagem espacial com base na analise de cluster anterior----------------

# Apresentando a base de dados:
BVFR_Analytics %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)


# Analisando as correlações entre variáveis da base de dados nova_base_BVFR_Analytics
chart.Correlation(BVFR_Analytics[, 2:76], histogram = TRUE, pch = "+")

# Salvando a Matriz de Correlações -----------------------------------
rho_BVFR_Analytics <- cor(BVFR_Analytics[,2:76])

# Construindo um mapa de calor a partir das correlações
rho_BVFR_Analytics %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))


# O teste de efericidade de Bartlett --------------------------------------
cortest.bartlett(R = rho_BVFR_Analytics)

# O algoritmo prcomp(), do pacote psych, EXIGE que a a matriz de dados fornecida
# a ele já esteja padronizada pelo procedimento zscores:
BVFR_Analytics_Pad <- data.frame(BVFR_Analytics)
print(BVFR_Analytics)
print(BVFR_Analytics_Pad)
library(magrittr)
BVFR_Analytics_Pad <- BVFR_Analytics_Pad %>% data.frame() %>% set_rownames(.$Data)
BVFR_Analytics_Pad <- select(BVFR_Analytics_Pad, everything(), -Data)
view(BVFR_Analytics_Pad)
BVFR_Analytics_std <- scale(BVFR_Analytics_Pad) %>% data.frame()
view(BVFR_Analytics_std)


# Rodando a PCA
afpc_BVFR_Analytics <- prcomp(BVFR_Analytics_std)
summary(afpc_BVFR_Analytics)


# Sumarizando pontos importantes:
data.frame(eigenvalue = afpc_BVFR_Analytics$sdev ^ 2,
           var_compartilhada = summary(afpc_BVFR_Analytics)$importance[2,],
           var_cumulativa = summary(afpc_BVFR_Analytics)$importance[3,]) -> relatorio

relatorio %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Visualizando os pesos que cada variável tem em cada componente principal 
# obtido pela PCA
ggplotly(
  data.frame(afpc_BVFR_Analytics$rotation) %>%
    mutate(var = names(BVFR_Analytics[2:76])) %>%
    melt(id.vars = "var") %>%
    mutate(var = factor(var)) %>%
    ggplot(aes(x = "", y = value, fill = var)) +
    geom_bar(stat = "identity", color = "black") +
    facet_wrap(~variable) +
    labs(x = NULL, y = NULL, fill = "") +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
)

# Scree Plot - apenas ignorar os warnings
ggplotly(
  fviz_eig(X = afpc_BVFR_Analytics,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod4")
)

#Extraindo as Cargas Fatoriais
k <- sum((afpc_BVFR_Analytics$sdev ^ 2) > 1) 
cargas_fatoriais <- afpc_BVFR_Analytics$rotation[, 1:k] %*% diag(afpc_BVFR_Analytics$sdev[1:k])

# Visualizando as cargas fatoriais
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Visualizando as Comunalidades
data.frame(rowSums(cargas_fatoriais ^ 2)) %>%
  rename(comunalidades = 1) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Relatório das cargas fatoriais e das comunalidades
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Plotagem das Cargas Fatoriais
data.frame(cargas_fatoriais) %>%
  ggplot(aes(x = X1, y = X2)) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = "F1",
       y = "F2") +
  theme_bw()

# Scores Fatoriais
scores_fatoriais <- t(afpc_BVFR_Analytics$rotation)/afpc_BVFR_Analytics$sdev 
colnames(scores_fatoriais) <- colnames(BVFR_Analytics_std)

scores_fatoriais

scores_fatoriais %>%
  t() %>%
  data.frame() %>%
  rename(PC1 = 1,
         PC2 = 2) %>%
  select(PC1, PC2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)


# Proposta da construção de um ranking ------------------------------------

#Assumindo-se apenas o F1 como indicador, calculam-se os scores fatoriais:
score_D1 <- scores_fatoriais[1,]
score_D1

#Estabelecendo o ranking dos indicadores assumido
F1 <- t(apply(BVFR_Analytics_std, 1, function(x) x * score_D1))

F1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Na construção de rankings no R, devemos efetuar a multiplicação por -1, 
#visto que os scores fatoriais das observações mais fortes são, por padrão, 
#apresentados acompanhados do sinal de menos.
F1 <- data.frame(F1) %>%
  mutate(fator1 = rowSums(.) * -1)

F1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Importando a coluna referente ao fator F1:
BVFR_Analytics["fator1"] <- F1$fator1

#Criando um ranking pela soma ponderada dos fatores por sua variância
#compartilhada
BVFR_Analytics %>%
  mutate(pontuacao = fator1 * 
           relatorio$var_compartilhada[1]) -> BVFR_Analytics

#Visualizando o ranking final
BVFR_Analytics %>%
  arrange(desc(pontuacao)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
