dadoscamarao <- read.table(file = "https://raw.githubusercontent.com/cursoRunb/descritiva2019/master/Dia1/dadoscamarao.txt",
                           header = TRUE, dec=",")
dadoscamarao
plot(dadoscamarao$Ct,dadoscamarao$Peso)
abline(h=mean(dadoscamarao$Peso))
abline(v=mean(dadoscamarao$Ct))
cor(dadoscamarao$Peso,dadoscamarao$Ct)
abline(lm(dadoscamarao$Peso~dadoscamarao$Ct))
lm(dadoscamarao$Peso ~ dadoscamarao$Ct)
summary(lm(dadoscamarao$Peso ~ dadoscamarao$Ct))
text(locator(1),"intercepto -26.8383    coef angular 0.4147 ")
table(dadoscamarao$Local,dadoscamarao$Sexo)
table(dadoscamarao$Local,dadoscamarao$Sexo)
aa <- table(dadoscamarao$Local,dadoscamarao$Sexo)
rowsum(aa)
rowSums(aa)
colSums(aa)
colSums(aa)/120
.7*37
.3*37
.7*32
.3*32
.3*51
.7*51
table(dadoscamarao$Local,dadoscamarao$Sexo)
aa
chisq.test(aa)
aa
teste <- chisq.test(aa)
teste
names(teste)
teste$expected
teste$parameter
teste$method
teste[1]
teste[2]
teste[3]
teste[5]
teste[6]
teste[7]
split(dadoscamarao,dadoscamarao$Local)
boxplot(dadoscamarao$Peso~dadoscamarao$Local)
separado <- split(dadoscamarao,dadoscamarao$Local)
calculareg <- function(dados){
  print(lm(dados$Peso~dados$Ct))
}
lapply(separado,calculareg)
calculareg <- function(x){
  print(lm(x$Peso~x$Ct))
}
separado
