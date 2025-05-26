#11 

temp <- seq(-5,5)

prod <- c(1,5,4,7,10,8,9,13,14,13,18)


md_temp <- mean(temp)
md_prod <-  mean(prod)


#Estimando b1
b1 <- sum((temp - md_temp)*(prod - md_prod)) / sum((temp - md_temp)**2)

#Estimando b0
b0 <-  md_prod - b1*md_temp


prod_est <- b0 + b1*temp


SQres <-  sum((prod - prod_est)**2)

sigma_est <-  sqrt(SQres/(length(temp)-2))

SQtem <- sum((temp - md_temp)**2)


EP_temp <-  sigma_est/sqrt(SQtem)


t <-  qt(0.025, df = 9)

#Criando um intervalo de confiança para o b1
int_conf <-  b1+c(1,-1)*t*EP_temp



t_temp <-  b1/EP_temp


#Testando a significancia de b1
pvalor <-  (1-pt(t_temp,df = 9))*2


#Um intervalo de confiança para o prod medio em temp = 3


prod_1 <- b0 + b1*3


EP_prod_1 <- sigma_est*sqrt((1/11) + (3 - md_temp)**2/SQtem )


int_conf_prod_1 <- prod_1 +c(1,-1)*t*EP_prod_1




prod_2 <-  b0 + b1*-2



EP_prod_dif <- sigma_est*sqrt(  (3 - (-2))**2/SQtem )


int_conf_dif <- (prod_1 - prod_2) +c(1,-1)*t*EP_prod_1




temp_f <- (12 - b0)/b1


b0 + b1*temp_f


ajuste <- lm(prod ~ temp)
summary(ajuste)
fitted(ajuste)


###########################

require(car)


summary(Duncan)

ajuste <- lm(prestige ~ education, data = Duncan)

print(ajuste)

fitted(ajuste)

plot(Duncan$prestige, fitted(ajuste), xlab='Prestígio observado', ylab='Prestígio ajustado', pch = 20)


plot(fitted(ajuste), resid(ajuste), xlab = 'Valores ajustados', ylab = 'Resíduos', pch = 20)


b0 <-  0.284
b1 <-  0.902

(b0 + 12.5*b1)


vcov(ajuste) 

confint(ajuste)

n <-  length(Duncan$prestige)

md_prestige <-  mean(Duncan$prestige)


sq_prestige <-  sum((Duncan$prestige - fitted(ajuste))**2)


sigma <-  sqrt(sq_prestige/ (length(Duncan$prestige) - 2))



ep_b1 <-  sigma / sqrt(sum((Duncan$education - mean(Duncan$education))**2))

sqrt(vcov(ajuste)[2,2])


pt(b1-6/ep_b1,df = length(Duncan$education)-2, lower.tail = T)*2


int_conf_B_1 <- b1 +c(1,-1)*t*ep_b1

t <-  qt(0.025, df = n-2)
int_media <- function(x) {
  
  x0 <-  b0 + b1 * x
  
  ep_x0 <- sigma * sqrt((1/n) + ((x - mean(Duncan$education))^2 / sum((Duncan$education - mean(Duncan$education))^2)))
  
  intervalo <- x0 + c(-1, 1) * t * ep_x0
  
  return(intervalo)
}
int_media(9);int_media(15);int_media(mean(Duncan$education))




predict(ajuste, newdata = data.frame(education = 15), interval = "confidence", level = 0.95)

anova(ajuste)


anova(ajuste)[1,2]/sum(anova(ajuste)[,2])

summary(ajuste)

        