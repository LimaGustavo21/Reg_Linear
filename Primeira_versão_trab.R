
#Carregando os Dados e formatando tabela


#Variável Resposta (Renda Média Domiciliar per Capita)

exp <-  read.csv("Trabalho_1/Renda Média Domiciliar per Capita_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character")

exp$Renda_M_10 <- as.numeric(gsub(",", ".", gsub("\\.", "", exp$X2010)))

exp$X2010 <- NULL ; exp$Região.a.que.Pertence <- NULL

summary(exp)

exp <- exp[-1, ] #Remove a primeira linha "Estado do Paraná"



hist(exp$Renda_M_10)


#PIB

PIB <- read.csv("Trabalho_1/PIB per Capita_Tabela.csv",  sep = "\t", fileEncoding = "UTF-8", skip = 1,  colClasses = "character")


PIB$PIB21 <- as.numeric(gsub("\\.", "", PIB$X2021))

PIB$Região.a.que.Pertence <- NULL ; PIB$X2021 <- NULL

summary(PIB)

PIB <- PIB[-1, ] #Remove a primeira linha "Estado do Paraná"


#População 


pop <-  read.csv("Trabalho_1/População Estimada - IBGE_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )


pop$populacao24 <- as.numeric(gsub("\\.", "", pop$X2024))

pop$Região.a.que.Pertence <- NULL ; pop$X2024 <- NULL


summary(pop)

pop <- pop[-1, ]#Remove a primeira linha "Estado do Paraná"

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



dados <- merge(exp, PIB, by = "Município.Estado")


dados <- merge(dados, pop, by = "Município.Estado")

dados <- merge(dados, alf, by = "Município.Estado")

dados <- merge(dados, nat, by = "Município.Estado")

dados <- merge(dados, grav, by = "Município.Estado")

dados <- merge(dados, densi, by = "Município.Estado")

dados <- merge(dados, tur, by = "Município.Estado")



dados$tamanho <- dados$populacao24 / dados$densi_demo24

head(dados)

dados$com_tur_km <- dados$Total / dados$tamanho

summary(dados)

dados$Município.Estado <- NULL

modelo <- lm(Renda_M_10 ~ ., data = dados)
summary(modelo)

plot(modelo$residuals)


#Testando variáveis importantes para o modelo
modelo_reduzido <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais, data = dados) # A melhor dentre as variáveis de alfabetismo foi a 65 +
summary(modelo_reduzido)


modelo_reduzido_2 <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + natalidade23 , data = dados) 
summary(modelo_reduzido_2)



AIC(modelo_reduzido,modelo_reduzido_2 )#Aparentemente Natalidade não agregou pro modelo 

#------- Grav

dados$grav_15a17 <- ifelse(is.na(dados$grav_15a17), 0, dados$grav_15a17)

modelo_reduzido_3 <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + grav_15a17 , data = dados) 
summary(modelo_reduzido_3) #Gravidez também não 


# -------- Densdemo 
modelo_reduzido_4 <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + densi_demo24 , data = dados) 
summary(modelo_reduzido_4) #Densi também não 


# -------- Turismo 
modelo_reduzido_5 <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + Total , data = dados) 
summary(modelo_reduzido_5) #Densi também não 





