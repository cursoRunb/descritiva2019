dados <- read.csv2("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/2007.csv",
                   encoding = "UTF-8")
summary(dados)

alunos <- data.frame(nomes=c("ana","bia","carol"),idades=c(10,20,30))
alunos[3,]
alunos$nomes == "carol"
indices_riobranco <- dados$Município== "TARAUACA"
barplot(summary(dados[indices_riobranco,"Tipo.de.Produto"]))
contagem <- (sort(summary(dados[indices_riobranco,"Tipo.de.Produto"]),decreasing = TRUE))
par(las=2)
barplot(contagem[contagem>0],)
enderecos <- locator(11)
for(i in 1:length(contagem[contagem>0])){
  text(enderecos$x[i],enderecos$y[i],contagem[contagem>0][i])
}
dados$Tipo.de.Produto

plot(dados$Volume.Original.Autorizado,dados$Volume.Remanescente)
summary(dados)
cores <- as.numeric(dados$Situação.Atual)
plot(dados$Volume.Original.Autorizado,dados$Volume.Remanescente,col=cores)
legend(locator(1),legend=(levels(dados$Situação.Atual)),fill=1:5)
identify(dados$Volume.Original.Autorizado,dados$Volume.Remanescente,
         labels = dados$Município)
options(digits = 5)

summary(dados$Área..ha.)[4]
quantile(dados$Área..ha.)
#para encontrar o percentil 90
posicao <- nrow(dados)*75/100
posicao
sort(dados$Área..ha.)[ceiling(posicao)]
?quantile
quantile(dados$Área..ha.,probs=seq(0,1,by=.1))
consumo <- c(7.5,2,12.5,5,15,18)

sqrt(sum((consumo-10)^2)/6)

sort(summary(as.factor(dados$Área..ha.)),decreasing=T)[1]
par(mfrow=c(1,1))
boxplot(dados$Área..ha.~dados$Município,main="Area")
boxplot(dados$Volume.Original.Autorizado~dados$Município,main="Volume")
boxplot(dados$Área..ha.)
boxplot(dados$Volume.Original.Autorizado)
par(las=2)
boxplot(mtcars$wt)
mtcars$wt
quantile(mtcars$wt)
(quantile(mtcars$wt)[4]-quantile(mtcars$wt)[2])*1.5
min(mtcars$wt)
quantile(mtcars$wt)[2]-1.54
quantile(mtcars$wt)[4]+1.54
sort(mtcars$wt,decreasing = T)


aa <- dados[dados$Município== "SENA MADUREIRA",]
dim(aa)
quantile(aa$Volume.Original.Autorizado)
