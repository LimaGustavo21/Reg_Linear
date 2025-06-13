#Carregando os Dados e formatando tabela


#Variável Resposta (Renda Média Domiciliar per Capita)

exp <-  read.csv("Trabalho_1/Renda Média Domiciliar per Capita_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character")

exp$Renda_M_10 <- as.numeric(gsub(",", ".", gsub("\\.", "", exp$X2010)))

exp$X2010 <- NULL ; exp$Região.a.que.Pertence <- NULL

summary(exp)

exp <- exp[-1, ] #Remove a primeira linha "Estado do Paraná"


hist(exp$Renda_M_10,
     main = "Renda média domiciliar per capita",
     xlab = "Renda em R$",
     col = "grey",
     border = "white")

# Adiciona linha vertical na média
abline(v = mean(exp$Renda_M_10, na.rm = TRUE), col = "red", lty = 2, lwd = 2)
exp[exp$Renda_M_10 == max(exp$Renda_M_10),]

exp_ordenado <- exp[order(exp$Renda_M_10, decreasing = TRUE), ]

head(exp_ordenado)

#PIB

PIB <- read.csv("Trabalho_1/PIB per Capita_Tabela.csv",  sep = "\t", fileEncoding = "UTF-8", skip = 1,  colClasses = "character")


PIB$PIB21 <- as.numeric(gsub("\\.", "", PIB$X2021))

PIB$Região.a.que.Pertence <- NULL ; PIB$X2021 <- NULL

summary(PIB)

#População 


pop <-  read.csv("Trabalho_1/População Estimada - IBGE_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )


pop$populacao24 <- as.numeric(gsub("\\.", "", pop$X2024))

pop$Região.a.que.Pertence <- NULL ; pop$X2024 <- NULL


summary(pop)


#Taxa de alfabetização

alf <-  read.csv("Trabalho_1/Taxa de Alfabetização_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )





alf$Município.Estado <- alf$X ; alf$X <- NULL ; alf$X.1 <- NULL




alf <- alf[-c(1,2), ]#Remove a primeira linha "Estado do Paraná"

head(alf)

cols <- setdiff(names(alf), "Município.Estado")
alf[cols] <- lapply(alf[cols], function(x) as.numeric(gsub(",", ".", x)))

summary(alf)



#Taxa bruta de natalidade
nat <-  read.csv("Trabalho_1/Taxa Bruta de Natalidade_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )

nat$natalidade23 <- as.numeric(gsub(",", ".",  nat$X2023))  

nat$X2023 <- NULL ; nat$Região.a.que.Pertence <- NULL

head(nat)

summary(nat)

nat <- nat[-1, ]#Remove a primeira linha "Estado do Paraná"




#Gravidez na adolescencia 

grav <-  read.csv("Trabalho_1/Gravidez na Adolescência_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )


grav$grav_15a17 <- as.numeric(gsub(",", ".",  grav$grav_15a17))  

grav$Município.Estado <- grav$X ; grav$X <- NULL

grav$X.1 <- NULL



grav <- grav[-1, ]#Remove a primeira linha "Estado do Paraná"

head(grav)

summary(grav)



#Densidade demografica

densi <-  read.csv("Trabalho_1/Densidade Demográfica_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )





densi$densi_demo24 <-  as.numeric(gsub(",", ".", gsub("\\.", "", densi$X2024)))

densi$X2024 <- NULL ; densi$Região.a.que.Pertence <- NULL



summary(densi)

head(densi)
densi[is.na(densi$densi_demo24), ]


densi <- densi[-1, ]#Remove a primeira linha "Estado do Paraná"


#Turismo 
tur <-  read.csv("Trabalho_1/Estabelecimentos nas ACTs_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1 )

tur$Município.Estado <-  tur$X ; tur$X <- NULL ; tur$X.1 <- NULL


tur$Total <-  as.numeric(tur$Total)

tur$Alimentação <-  as.numeric(tur$Alimentação)
tur <- tur[-c(1,2), ]#Remove a primeira linha "Estado do Paraná"

tur[is.na(tur)] <- 0
head(tur)

summary(tur)



urb <-  read.csv("Trabalho_1/Grau de Urbanização_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1,  colClasses = "character" )



urb$pct_urb22 <-  as.numeric(gsub(",", ".", gsub("\\.", "", urb$X2022))) ; urb$X2022 <- NULL


urb$Região.a.que.Pertence <-  NULL


summary(urb)


#Distância até a capital 

cap <-  read.csv("Trabalho_1/Distância à Capital_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )


cap$dist_cap21 <-  as.numeric(gsub(",", ".", gsub("\\.", "", cap$X2021))) ;  cap$X2021 <-  NULL


cap$Município.Estado <- cap$Município

cap$Município <- NULL

cap$Região.a.que.Pertence <- NULL
cap[is.na(cap)] <- 0


summary(cap)



#Unidades de Ensino 

ensino <- read.csv("Trabalho_1/Estabelecimentos_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )


ensino$qtd_ensino24 <-  as.numeric(gsub(",", ".", gsub("\\.", "", ensino$Total))) ; ensino$Total <-  NULL

ensino$Município.Estado <- ensino$X ; ensino$X <- NULL

ensino$X.1 <- NULL




#Esgoto 
sane <- read.csv("Trabalho_1/Atendimento de Esgoto_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )


sane$un_san23 <-  as.numeric(gsub(",", ".", gsub("\\.", "", sane$Unidades.Atendidas...Total))) ; sane$Unidades.Atendidas...Total <-  NULL

sane$Município.Estado <- sane$X ; sane$X.1 <- NULL ;sane$X <- NULL

summary(sane)


sane[is.na(sane)] <- 0


summary(ensino)

fecun <- read.csv("Trabalho_1/Taxa de Fecundidade_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )

fecun$taxa_fecun10 <-  as.numeric(gsub(",", ".", gsub("\\.", "", fecun$X2010))) ; fecun$X2010 <-  NULL

fecun$Região.a.que.Pertence <- NULL
summary(fecun)



#Docentes_Ensino basico

prof <- read.csv("Trabalho_1/Docentes_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )

summary(prof)

prof$qtd_prof24 <-  as.numeric(gsub(",", ".", gsub("\\.", "", prof$Total))) ; prof$Total <-  NULL

prof$Município.Estado <- prof$X ; prof$X <- NULL ; prof$X.1 <- NULL 


#pop_faixa etária 

fx_etaria <- read.csv("Trabalho_1/População Censitária_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )

head(fx_etaria)



# Lista de colunas que você quer tratar
cols <- c("X5.anos", "X6.anos", "X7.anos", "X8.anos", "X9.anos", 
          "X10.anos", "X11.anos", "X12.anos", "X13.anos", "X14.anos",
          "X15.anos", "X16.anos", "X17.anos", "X18.anos")


fx_etaria[cols] <- lapply(fx_etaria[cols], function(x) as.numeric(gsub("\\.", "", x)))

fx_etaria$Pop_5_18 <- rowSums(fx_etaria[cols], na.rm = TRUE)

fx_etaria$Município.Estado <- fx_etaria$X

summary(fx_etaria)



#taxa de mortalidade
taxa_morte <- read.csv("Trabalho_1/Taxa de Mortalidade_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
taxa_morte$taxa_morte23 <- as.numeric(gsub(",", ".", gsub("\\.", "", taxa_morte$Geral..mil.habitantes.))); taxa_morte$Geral..mil.habitantes. <- NULL
taxa_morte$X.1 <- NULL
taxa_morte$Município.Estado <- taxa_morte$X 
taxa_morte$X <- NULL

#taxa de atividade
taxa_atividade <- read.csv("Trabalho_1/Taxa de Atividade_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
taxa_atividade$atv_18_ou_mais10 <- as.numeric(gsub(",", ".", gsub("\\.", "", taxa_atividade$X18.anos.ou.mais....))); taxa_atividade$X18.anos.ou.mais.... <- NULL
taxa_atividade$X.1 <- NULL
taxa_atividade$Município.Estado <- taxa_atividade$X 
taxa_atividade$X <- NULL

#agencias bancárias
agencias_bancarias <- read.csv("Trabalho_1/Agências Bancárias_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
agencias_bancarias$agenc_bancarias24 <- as.numeric(gsub(",", ".", gsub("\\.", "", agencias_bancarias$Total))); agencias_bancarias$Total <- NULL
agencias_bancarias$X.1 <- NULL
agencias_bancarias$Município.Estado <- agencias_bancarias$X 
agencias_bancarias$X <- NULL
#transformando os NA em 0
agencias_bancarias[is.na(agencias_bancarias)] <- 0
#teve o p valor muito alto: 0.22.. (nao vale a pena colocar no modelo reduzido)

#indice de gini
indice_gini <- read.csv("Trabalho_1/Índice de Gini_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", colClasses = "character" )
indice_gini$gini10 <- as.numeric(gsub(",", ".", gsub("\\.", "", indice_gini$Índice.de.Gini...Geral))); indice_gini$Índice.de.Gini...Geral <- NULL
indice_gini$X.1 <- NULL
indice_gini$Município.Estado <- indice_gini$X
indice_gini$X <- NULL
head(indice_gini)
summary(agencias_bancarias) #tem otimo p-valor *** mas deixou o p-valor do PIB21 um pouco pior ** (mas ainda interessante de deixar no modelo reduzido)

#taxa de aprovação
taxa_aprovacao <- read.csv("Trabalho_1/Taxa de Aprovação_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
taxa_aprovacao$aprovacao_medio23 <- as.numeric(gsub(",", ".", gsub("\\.", "", taxa_aprovacao$Ensino.Médio....))); taxa_aprovacao$Ensino.Médio.... <- NULL
taxa_aprovacao$X.1 <- NULL
taxa_aprovacao$Município.Estado <- taxa_aprovacao$X
taxa_aprovacao$X <- NULL
summary(taxa_aprovacao) #p-valor muito alto: 0.65 (nao vale a pena colocar no modelo reduzido)

#taxa de pobreza
taxa_pobreza <- read.csv("Trabalho_1/Taxa de Pobreza_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", colClasses = "character" )
taxa_pobreza$pobreza10 <- as.numeric(gsub(",", ".", gsub("\\.", "", taxa_pobreza$Taxa.de.Pobreza....))); taxa_pobreza$Taxa.de.Pobreza.... <- NULL
taxa_pobreza$X.1 <- NULL
taxa_pobreza$Município.Estado <- taxa_pobreza$X
taxa_pobreza$X <- NULL
summary(taxa_pobreza) #p-valor muito bom *** mas diminuiu ainda mais o p-valor do PIB21 ficou "*" : 0.042...

#taxa de ocupação
taxa_ocupacao <- read.csv("Trabalho_1/Taxa de Ocupação_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
taxa_ocupacao$ocupacao_18_mais10 <- as.numeric(gsub(",", ".", gsub("\\.", "", taxa_ocupacao$X18.anos.ou.mais....))); taxa_ocupacao$X18.anos.ou.mais.... <- NULL
taxa_ocupacao$X.1 <- NULL
taxa_ocupacao$Município.Estado <- taxa_ocupacao$X
taxa_ocupacao$X <- NULL
summary(taxa_ocupacao) #p-valor medio (analisar se vale a pena deixar no modelo reduzido depois): 0.013...

# Consumo_energia
Consumo_energia <- read.csv("Trabalho_1/Consumo_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
Consumo_energia$Município.Estado <- Consumo_energia$X
Consumo_energia$Total_consumo_energia <- as.numeric(gsub(",", ".", gsub("\\.", "", Consumo_energia$Total..Mwh.)))
Consumo_energia <- subset(Consumo_energia, select = c(Município.Estado, Total_consumo_energia))

# Estabelecimento de Saúde
Estabelecimento_saude <- read.csv("Trabalho_1/Estab. de Saúde - Tipo_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
Estabelecimento_saude$Município.Estado <- Estabelecimento_saude$X
Estabelecimento_saude$Total_estabelecimento_saude <- as.numeric(gsub(",", ".", gsub("\\.", "", Estabelecimento_saude$Total)))
Estabelecimento_saude <- subset(Estabelecimento_saude, select = c(Município.Estado, Total_estabelecimento_saude))

# Estabelecimento_setores
Estabelecimentos_gerais <- read.csv("Trabalho_1/Estabelecimentos nas ACTs_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
Estabelecimentos_gerais$Município.Estado <- Estabelecimentos_gerais$X
Estabelecimentos_gerais$Total_estabelecimentos <- as.numeric(gsub(",", ".", gsub("\\.", "", Estabelecimentos_gerais$Total)))
Estabelecimentos_gerais$Total_estabelecimentos_alimentacao <- as.numeric(gsub(",", ".", gsub("\\.", "", Estabelecimentos_gerais$Alimentação)))
Estabelecimentos_gerais <- subset(Estabelecimentos_gerais, select = c(Município.Estado, Total_estabelecimentos,Total_estabelecimentos_alimentacao))
Estabelecimentos_gerais[is.na(Estabelecimentos_gerais)] <- 0

# Frotas 
Frotas <- read.csv("Trabalho_1/Frota de Veículos_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
Frotas$Município.Estado <- Frotas$X
Frotas$Total_frotas <- as.numeric(gsub(",", ".", gsub("\\.", "", Frotas$Total)))
Frotas <- subset(Frotas, select = c(Município.Estado, Total_frotas))



dados <- merge(exp, PIB, by = "Município.Estado")

dados <- merge(dados, pop, by = "Município.Estado")

dados <- merge(dados, alf, by = "Município.Estado")

dados <- merge(dados, nat, by = "Município.Estado")

dados <- merge(dados, grav, by = "Município.Estado")

dados <- merge(dados, densi, by = "Município.Estado")

dados <- merge(dados, tur, by = "Município.Estado")

dados <- merge(dados, urb, by = "Município.Estado")

dados <- merge(dados, cap, by = "Município.Estado")

dados <- merge(dados, ensino, by = "Município.Estado")

dados <- merge(dados, sane, by = "Município.Estado")

dados <- merge(dados, fecun, by = "Município.Estado")

dados <- merge(dados, prof, by = "Município.Estado")

dados <- merge(dados, fx_etaria[, c("Município.Estado", "Pop_5_18")], 
               by = "Município.Estado", all.x = TRUE)

dados <- merge(dados, taxa_morte, by = "Município.Estado")

dados <- merge(dados, taxa_atividade, by = "Município.Estado")

dados <- merge(dados, agencias_bancarias, by = "Município.Estado")

dados <- merge(dados, indice_gini, by = "Município.Estado")

dados <- merge(dados, taxa_aprovacao, by = "Município.Estado")

dados <- merge(dados, taxa_pobreza, by = "Município.Estado")

dados <- merge(dados, taxa_ocupacao, by = "Município.Estado")

dados <- merge(dados, Consumo_energia, by = "Município.Estado")

dados <- merge(dados, Estabelecimento_saude, by = "Município.Estado")
dados <- merge(dados, Estabelecimentos_gerais, by = "Município.Estado")
dados <- merge(dados, Frotas, by = "Município.Estado")



dados$aln_por_prof <-  dados$Pop_5_18/ dados$qtd_prof24

dados$tamanho <- dados$populacao24 / dados$densi_demo24

head(dados)

dados$com_tur_km <- dados$Total / dados$tamanho


dados$densidade_saneamento <- dados$un_san23 / dados$tamanho
dados$unid_ensino_por_hab <- dados$qtd_ensino24 / dados$populacao24


dados$densidade_ensino <- dados$qtd_ensino24 / dados$tamanho

summary(dados)



modelo <- lm(Renda_M_10 ~ ., data = dados[,-1])
summary(modelo)

#---------------------------------------------------- FIZ NA AULA DA FERNANDA



library(MASS)

box <- boxcox(modelo_reduzido, lambda = seq(-2, 2, 0.1))

lambda <- box$x[which.max(box$y)]


dados$Renda_M_10 <-  log(dados$Renda_M_10)
#dados$populacao24 <-  log(dados$populacao24)
dados_1 <- dados

dados <- dados_1
dados <- dados[-c(104,141,158, 156, 302, 204),]
#dados <- dados[-c(95),]
summary(dados_1)

dados <- dados[-c(104,141,158,292),]
#,95,302,156
#atual
dados[156,]
#dados$Renda_M_10 <- dados$Renda_M_10/1000

#Testando variáveis importantes para o modelo
modelo_reduzido <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + pct_urb22 + dist_cap21 + taxa_fecun10 + atv_18_ou_mais10 , data = dados) # A melhor dentre as variáveis de alfabetismo foi a 65 +
summary(modelo_reduzido)



library(lmtest)



# Teste de Breusch-Pagan
bptest(modelo_reduzido)



bptest(modelo_reduzido, ~ fitted(modelo_reduzido) + I(fitted(modelo_reduzido)^2))
plot(modelo_reduzido$fitted.values, rstandard(modelo_reduzido),
     xlab = "Valores ajustados", ylab = "Resíduos studentizados")
abline(h = 0, col = "red")


# Padronização das variáveis explicativas
dados_pad <- dados
dados_pad$PIB21 <- scale(dados$PIB21)
dados_pad$populacao24 <- scale(dados$populacao24)
dados_pad$X65_mais <- scale(dados$X65_mais)
dados_pad$pct_urb22 <- scale(dados$pct_urb22)
dados_pad$dist_cap21 <- scale(dados$dist_cap21)
dados_pad$atv_18_ou_mais10 <- scale(dados$atv_18_ou_mais10)

# Ajusta o modelo padronizado
modelo_reduzido_pad <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + pct_urb22 + dist_cap21 + atv_18_ou_mais10, data = dados_pad)

# Sumário do modelo padronizado
summary(modelo_reduzido_pad)





#---------------------------------------------------------------------------------------------------------------
plot(modelo_reduzido$residuals)


residuos_padronizados <- rstandard(modelo_reduzido)

# Plota os resíduos padronizados
plot(residuos_padronizados, 
     main = "Resíduos Padronizados",
     ylab = "Resíduos Padronizados",
     xlab = "Índice",
     pch = 20, col = "black")
abline(h = 0, col = "red", lty = 2)

#testando os pressupostos

library(lmtest)

# Teste de Breusch-Pagan
bp_test <- bptest(modelo_reduzido)
p_valor <- round(bp_test$p.value, 4)

# Gráfico com p-valor no título
plot(modelo_reduzido$fitted.values, modelo_reduzido$residuals,
     main = paste("Resíduos vs Valores Ajustados\np-valor BP =", p_valor),
     xlab = "Valores Ajustados",
     ylab = "Resíduos",
     pch = 20, col = "black")
abline(h = 0, col = "grey", lty = 2)
#se os resíduos estão espalhados aleatoriamenre a pressuposição é aceita

# Realiza o teste de Shapiro-Wilk
shapiro_result <- shapiro.test(modelo_reduzido$residuals)

# QQ-plot dos resíduos
qqnorm(modelo_reduzido$residuals, 
       main = "QQ-Plot dos Resíduos com Teste de Normalidade",
       pch = 20, col = "black")
qqline(modelo_reduzido$residuals, col = "red", lty = 2)

# Adiciona os valores do teste no canto inferior direito do gráfico
text(x = 0.5, y = min(modelo_reduzido$residuals),
     labels = paste0("W = ", round(shapiro_result$statistic, 4),
                     "\n", "p = ", format.pval(shapiro_result$p.value, digits = 3, eps = .001)),
     pos = 4, col = "black", cex = 0.9)
#como no teste nosso p deu menor que 0.05 podemos dizer que os resíduos não seguem uma distribuição normal

#aqui temos uma detecção de outliers
dados[which(residuos_padronizados > 3), ]

dados$residuos_padronizados <- residuos_padronizados

dados[dados$residuos_padronizados > 2.5, c("Município.Estado", "residuos_padronizados")]
104,141,158, 292, 302, 204
#gráficos para mostrar esses dados outliers

cooks <- cooks.distance(modelo_reduzido)
plot(cooks, type = "h", main = "Distância de Cook", ylab = "Cook's Distance")
abline(h = 4 / nrow(dados), col = "red", lty = 2)


#metodo box cox

# Cálculo da distância de Cook
cooks <- cooks.distance(modelo_reduzido)

# Limite clássico para ponto influente
limite_cook <- 0.015

# Índices das observações influentes
influentes <- which(cooks > limite_cook)

# Tabela com os municípios influentes e suas respectivas distâncias de Cook
dados_influentes <- data.frame(
  Indice = influentes,
  Municipio = dados$Município.Estado[influentes],
  Distancia_Cook = cooks[influentes]
)
#---------------------------------------------------------------------------------------------------------------
dados_influentes <- dados_influentes[order(dados_influentes$Distancia_Cook, decreasing = TRUE), ]

# Visualiza a tabela ordenada
print(dados_influentes)



# Intervalos de confiança dos coeficientes (nível 95%)
ic_betas <- confint(modelo_reduzido_pad)

# Estimativas dos coeficientes
betas <- coef(modelo_reduzido_pad)

# Junta tudo em um data frame para plotagem
df_betas <- data.frame(
  Variavel = names(betas),
  Estimativa = betas,
  IC_inf = ic_betas[,1],
  IC_sup = ic_betas[,2]
)

# Remove o intercepto se quiser focar nos preditores
df_betas <- df_betas[df_betas$Variavel != "(Intercept)", ]

# Plot
library(ggplot2)
ggplot(df_betas, aes(x = reorder(Variavel, Estimativa), y = Estimativa)) +
  geom_point(size = 2, color = "blue") +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2, color = "darkgray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Intervalos de Confiança (95%) dos Coeficientes",
    x = "Variável",
    y = "Estimativa do Coeficiente"
  ) +
  theme_minimal()

  

  ggplot(df_betas, aes(x = Estimativa, y = reorder(Variavel, Estimativa))) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.2, color = "gray50") +
  geom_point(aes(color = Estimativa > 0), size = 3) +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "orange")) +
  labs(
    title = "Intervalos de Confiança (95%) dos Coeficientes",
    x = "Estimativa do Coeficiente",
    y = "Variável"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
  
  
  
  
  
  
  # 1. Retirando PIB21
  m1 <- lm(Renda_M_10 ~ populacao24 + X65_mais + pct_urb22 + dist_cap21 + taxa_fecun10 + atv_18_ou_mais10, data = dados)
  anova(m1, modelo_reduzido)
  
  # 2. Retirando populacao24
  m2 <- lm(Renda_M_10 ~ PIB21 + X65_mais + pct_urb22 + dist_cap21 + taxa_fecun10 + atv_18_ou_mais10, data = dados)
  anova(m2, modelo_reduzido)
  
  # 3. Retirando X65_mais
  m3 <- lm(Renda_M_10 ~ PIB21 + populacao24 + pct_urb22 + dist_cap21 + taxa_fecun10 + atv_18_ou_mais10, data = dados)
  anova(m3, modelo_reduzido)
  
  # 4. Retirando pct_urb22
  m4 <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + dist_cap21 + taxa_fecun10 + atv_18_ou_mais10, data = dados)
  anova(m4, modelo_reduzido)
  
  # 5. Retirando dist_cap21
  m5 <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + pct_urb22 + taxa_fecun10 + atv_18_ou_mais10, data = dados)
  anova(m5, modelo_reduzido)
  
  # 6. Retirando taxa_fecun10
  m6 <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + pct_urb22 + dist_cap21 + atv_18_ou_mais10, data = dados)
  anova(m6, modelo_reduzido)
  
  # 7. Retirando atv_18_ou_mais10
  m7 <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + pct_urb22 + dist_cap21 + taxa_fecun10, data = dados)
  anova(m7, modelo_reduzido)
  
  
  
  install.packages("car")
  library(car)
  
  par(mfrow = c(1, 1))
  plot(modelo)
  
  
  library(MASS)
  
  # Box-Cox
  box <- boxcox(modelo_reduzido, lambda = seq(-2, 2, 0.1),
                main = "Análise de Transformação Box-Cox",
                xlab = expression(lambda), ylab = "Log-Verossimilhança")
  
  # Encontrar o lambda ótimo
  lambda_otimo <- box$x[which.max(box$y)]
  lambda_otimo <- round(lambda_otimo, 3)
  
  # Adicionar linha vertical no lambda ótimo
  abline(v = lambda_otimo, col = "red", lty = 2, lwd = 2)
  
  # Adicionar legenda no gráfico
  legend("topright",
         legend = paste("Lambda ótimo =", lambda_otimo),
         col = "red", lty = 2, lwd = 2, bty = "n", cex = 0.95)