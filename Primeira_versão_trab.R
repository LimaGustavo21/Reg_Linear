
resp <- read.csv("Trabalho_1/PIB per Capita_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1)

exp <-  read.csv("Trabalho_1/Renda Média Domiciliar per Capita_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1)


dados <- merge(resp, exp, by = "Município.Estado")



dados$PIB <- as.numeric(gsub("\\.", "", dados$X2021))
dados$Renda <- as.numeric(gsub(",", ".", dados$X2010))


modelo <- lm(Renda ~ PIB, data = dados)
summary(modelo)

