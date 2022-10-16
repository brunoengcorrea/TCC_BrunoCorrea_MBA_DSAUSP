################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra","rgl","car",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Algoritmo para determinação dos erros-padrão das variâncias no componente de
#efeitos aleatórios
#ATENÇÃO: A função abaixo é plenamente funcional para modelos do tipo HLM2
#e HLM3, desde que estimados pelo pacote nlme

stderr_nlme <- function(model){
  if(base::class(model) != "lme"){
    base::message("Use a lme object model from nlme package")
    stop()}
  resume <- base::summary(model)
  if(base::length(base::names(model$groups))==1){
    m.type <- "HLM2"
  } else if(base::length(base::names(model$groups))==2){
    m.type <- "HLM3"
  }
  if(m.type == "HLM2"){
    vcov_matrix <- model$apVar
    logs_sd_re <- base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re)==2){
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE Components`=base::c("Var(v0j)","Var(e)"),
                                  `Variance Estimatives`= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                                  base::exp(logs_sd_re[[2]])^2),
                                  `Std Err.`=base::c(stderr_tau00,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
    else{
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau01 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(v0j)","Var(v1j)","Var(e)"),
                                  Estimatives= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                       base::exp(logs_sd_re[[2]])^2,
                                                       base::exp(logs_sd_re[[4]])^2),
                                  Std_Err=base::c(stderr_tau00,
                                                  stderr_tau01,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                            base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                                                               base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
  if(m.type == "HLM3"){
    vcov_matrix <- model$apVar
    logs_sd_re <-  base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re) == 3){
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x3)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(t00k)","Var(v0jk)","Var(e)"),
                                  Estimatives=base::c(base::exp(logs_sd_re)[[2]]^2,
                                                      base::exp(logs_sd_re)[[1]]^2,
                                                      base::exp(logs_sd_re)[[3]]^2),
                                  Std_Err=base::c(stderr_tau_u000,
                                                  stderr_tau_r000,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    } 
    else{
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_r100 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u100 <- msm::deltamethod(~exp(x5)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x7)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE_Components`=base::c("Var(t00k)","Var(t10k)",
                                                          "Var(v0jk)","Var(v1jk)",
                                                          "Var(e)"),
                                  `Variance Estimatives`=base::c(base::exp(logs_sd_re)[[4]]^2,
                                                                 base::exp(logs_sd_re)[[5]]^2,
                                                                 base::exp(logs_sd_re)[[1]]^2,
                                                                 base::exp(logs_sd_re)[[2]]^2,
                                                                 base::exp(logs_sd_re)[[7]]^2),
                                  `Std Err.`=base::c(stderr_tau_u000,
                                                     stderr_tau_u100,
                                                     stderr_tau_r000,
                                                     stderr_tau_r100,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                            base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                                                               base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
}


################################################################################
#                      DESCRIÇÃO E EXPLORAÇÃO DO DATASET                       #
################################################################################

#Carregamento da base de dados
BVFR_Analytics <- read_excel("C:/Users/bcorrea/Downloads/TCC-20221010T234045Z-001/TCC/dados/Base_Analytics_BVFR_2.xlsx")

#Visualização da base de dados formada na analise de cluster
BVFR_AnalyticsFinal %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 11)

#Estatísticas descritivas
summary(BVFR_AnalyticsFinal)

#Estudo sobre a clusterização
BVFR_AnalyticsFinal %>% 
  group_by(cluster_5) %>%
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

#Desempenho médio da recuperação por cluster
BVFR_AnalyticsFinal %>%
  group_by(cluster_5) %>%
  summarise(`Recuperação média` = mean(Recuperação, na.rm = T)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 18)

#Exploração visual do desempenho médio
BVFR_AnalyticsFinal %>%
  group_by(cluster_5) %>%
  mutate(Recuperação_media = mean(Recuperação, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point(aes(x = grupo_var5, y = Recuperação),color = "orange", alpha = 0.5, size = 2) +
  geom_line(aes(x = grupo_var5, y = Recuperação_media, 
                group = 1, color = "Recuperação do cluster"), size = 1.5) +
  scale_colour_viridis_d() +
  labs(x = "Cluster",
       y = "Recuperação cluster") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (Recuperação), com histograma
ggplotly(
  ggplot(BVFR_AnalyticsFinal, aes(x = Recuperação)) +
    geom_density(aes(x = Recuperação), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                   bins = 30) +
    theme_classic()
)


################################################################################
#                         ESTIMAÇÃO DO MODELO NULO HLM2                        #
################################################################################

#Estimação do modelo nulo (função lme do pacote nlme)
modelo_nulo_hlm2 <- lme(fixed = Recuperação ~ 1, 
                        random = ~ 1 | cluster_5,
                        data = BVFR_AnalyticsFinal,
                        method = "REML")

#Parâmetros do modelo
summary(modelo_nulo_hlm2)

#Verificando a funcionalidade da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm2)


################################################################################
#                    COMPARAÇÃO DO HLM2 NULO COM UM OLS NULO                   #
################################################################################
#Para estimarmos o modelo OLS nulo, podemos comandar o seguinte
modelo_ols_nulo <- lm(formula = Recuperação ~ 1, 
                      data = BVFR_AnalyticsFinal)

#Parâmetros do modelo OLS nulo
summary(modelo_ols_nulo)

#Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_ols_nulo, modelo_nulo_hlm2)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


################################################################################
#            ESTIMAÇÃO DO MODELO COM INTERCEPTOS ALEATÓRIOS HLM2               #
################################################################################

#Estimação do modelo com Interceptos Aleatórios
modelo_intercept_hlm2 <- lme(fixed = Recuperação ~ Carbonatos_1,
                             random = ~ 1 | cluster_5,
                             data = BVFR_AnalyticsFinal,
                             method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_hlm2)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


################################################################################
#      ESTIMAÇÃO DO MODELO COM INTERCEPTOS E INCLINAÇÕES ALEATÓRIOS HLM2       #
################################################################################

#Estimação do modelo com Interceptos e Inclinações Aleatórios
modelo_intercept_inclin_hlm2 <- lme(fixed = Recuperação ~ Carbonatos_1,
                                    random = ~ Carbonatos_1 | cluster_5,
                                    data = BVFR_AnalyticsFinal,
                                    method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_inclin_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_inclin_hlm2)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

################################################################################
#                 COMPARAÇÃO COM UM MODELO OLS COM DUMMIES                     #
################################################################################

#Procedimento n-1 dummies para o contexto
BVFR_AnalyticsFinal_dummies <- dummy_cols(.data = BVFR_AnalyticsFinal,
                                       select_columns = "cluster_5",
                                       remove_first_dummy = TRUE,
                                       remove_selected_columns = TRUE)
view(BVFR_AnalyticsFinal_dummies)

#Visualizando as dummies na nova base de dados 'estudante_escola_dummies'
BVFR_AnalyticsFinal_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 11)

#Modelo OLS com dummies
modelo_ols_dummies <- lm(formula = Recuperação ~ Data +
                           Disponibilidade + 
                           Utilização + 
                           Moagem_2 + 
                           Deslamagem_1 + 
                           Deslamagem_2 + 
                           Deslamagem_3 + 
                           Carbonatos_1 + 
                           Carbonatos_2 + 
                           Carbonatos_3 + 
                           Carbonatos_4 + 
                           Carbonatos_5 + 
                           Carbonatos_6 + 
                           Carbonatos_7 + 
                           Carbonatos_8 + 
                           Carbonatos_9 + 
                           Carbonatos_10 + 
                           Carbonatos_11 + 
                           Carbonatos_12 + 
                           Carbonatos_13 + 
                           Carbonatos_14 + 
                           Carbonatos_15 + 
                           Carbonatos_16 + 
                           Carbonatos_17 + 
                           Carbonatos_18 + 
                           Carbonatos_19 +
                           Carbonatos_20 +
                           Carbonatos_21 +
                           Carbonatos_22 +
                           Carbonatos_23 +
                           Carbonatos_24 + 
                           Carbonatos_25 + 
                           Carbonatos_26 + 
                           Carbonatos_27 + 
                           Carbonatos_28 +
                           Silica_1 +
                           Silica_2 +
                           Silica_3 +
                           Silica_4 +
                           Silica_5 +
                           Silica_6 +
                           Silica_7 +
                           Silica_8 +
                           Silica_9 +
                           Silica_10 +
                           Silica_11 +
                           Silica_12 +
                           Silica_13 +
                           Silica_14 +
                           Silica_15 +
                           Magnetico_1 +
                           Magnetico_2 +
                           Magnetico_3 +
                           Magnetico_4 +
                           Magnetico_5 +
                           Magnetico_6 +
                           Niobio_1 +
                           Niobio_2 +
                           Niobio_3 +
                           Niobio_4 +
                           Niobio_5 +
                           Niobio_6 +
                           Niobio_7 +
                           Niobio_8 +
                           Niobio_9 +
                           Niobio_10 +
                           Niobio_11 +
                           Niobio_12 +
                           Niobio_13 +
                           Niobio_14 +
                           Niobio_15 +
                           Niobio_16 +
                           Niobio_17 +
                           Niobio_18 +
                           Niobio_19 +
                           cluster_5_2 +
                           cluster_5_3 +
                           cluster_5_4 +
                           cluster_5_5,
                         
                           data = BVFR_AnalyticsFinal_dummies)

#Parâmetros
summary(modelo_ols_dummies)

#Procedimento stepwise
modelo_ols_dummies_step <- step(object = modelo_ols_dummies,
                                step = qchisq(p = 0.05, df = 1,
                                              lower.tail = FALSE))

#Parâmetros do modelo OLS estimado com dummies por cluster a partir do
#procedimento Stepwise
summary(modelo_ols_dummies_step)

#Comparando os LL dos modelos HLM2 Final, OLs e OLS com Dummies e Stepwise
data.frame(OLS_Dummies = logLik(modelo_ols_dummies),
           OLS_Dummies_Step = logLik(modelo_ols_dummies_step),
           modelo_intercept_inclin_hlm2 = logLik(modelo_intercept_inclin_hlm2)) %>%
  rename(`OLS_Dummies` = 1,
         `OLS com Dummies e Stepwise` = 2,
         `modelo_intercept_inclin_hlm2` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                  values = c("darkorchid","maroon1","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#LR Test
lrtest(modelo_ols_dummies_step, modelo_intercept_inclin_hlm2)

#Comparação entre os parãmetros dos modelos (atente-se para a quantidade de
#parâmetros estimados em cada um deles!)
export_summs(modelo_ols_dummies_step, modelo_intercept_inclin_hlm2,
             model.names = c("OLS com Dummies", "HLM2 intercept_inclin"))


#Comparando a aderência dos fitted values dos modelos HLM2 Final, OLS e
#OLS com Dummies e Stepwise
#Gerando os fitted values do modelo OLS com Dummies e Stepwise
estudante_escola$ols_step_fitted <- modelo_ols_dummies_step$fitted.values

#Gráfico para a comparação entre os fitted values dos modelos HLM2 Final, OLs e
#OLS com Dummies e Procedimento Stepwise
estudante_escola %>%
  ggplot() +
  geom_smooth(aes(x = desempenho, y = ols_step_fitted, color = "OLS com Dummies"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y= hlm2_fitted, color = "HLM2 Final"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y= ols_fitted, color = "OLS"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue1", "maroon1", "darkorchid")) +
  labs(x = "Desempenho", y = "Fitted Values") +
  theme_bw()



##################################### FIM ######################################
