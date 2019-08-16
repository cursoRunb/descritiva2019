obitos <- read.csv2("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/obitos.csv",encoding = "UTF-8")
obitos <- read.table("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/obitos.csv",encoding = "UTF-8",
                     sep=";",header = T)
head(obitos)
summary(obitos)
mean(obitos$X1.Região.Norte[-nrow(obitos)])
mean(as.numeric(obitos[obitos$Ano.do.Óbito == "2017", -c(1,ncol(obitos)) ]))
mean(as.matrix(obitos[5:15,2:3]))
anos <- as.numeric(as.character(obitos$Ano.do.Óbito[-nrow(obitos)]))
linhas_uteis <- which(anos>=2000 & anos<=2010)
mean(as.matrix(obitos[linhas_uteis,2:3]))




exporta <- read.table(  "https://raw.githubusercontent.com/gustavopompeu/ENAP/master/EXP_2019.txt",
                        head=T,sep=";")
summary(exporta)
exporta$CO_PAIS <- as.factor(exporta$CO_PAIS)
exporta$CO_MES <- as.factor(exporta$CO_MES)

for(i in c(2,3,4,5,7,8)){
  exporta[,i] <- as.factor(exporta[,i])
}
summary(exporta)

for(i in c("CO_MES","CO_NCM")){
  exporta[,i] <- as.factor(exporta[,i])
}


summary(exporta$CO_MES)
which.min(summary(exporta$CO_MES))
summary(exporta$SG_UF_NCM)
which.max(summary(exporta$SG_UF_NCM))
max(summary(exporta$SG_UF_NCM))

names(exporta)
min(exporta$KG_LIQUIDO)

table(exporta$CO_PAIS,exporta$SG_UF_NCM)
pais87 <- exporta[exporta$CO_PAIS=="87",]
table(pais87$SG_UF_NCM,pais87$CO_VIA)

sp <- exporta[exporta$SG_UF_NCM=="SP","KG_LIQUIDO"]
mean(sp)


names(exporta)

for(i in 1:7){
  for(j in (i+1):8){
    print(table(exporta[,i],exporta[,j]))
    print("")
    print("__________________________________________________________________________")
    print("")
  }
}

table(exporta[,2],exporta[,3])
table(exporta[,2],exporta[,4])
table(exporta[,2],exporta[,5])
table(exporta[,2],exporta[,6])
table(exporta[,2],exporta[,7])

tarifa <- read.csv("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/TarifaFornecimentoResidencial.csv")

pie(summary(tarifa$nomConcessao))
barplot(summary(tarifa$nomConcessao))
hist(tarifa$VlrTotaTRFConvencional,breaks = 14,main="Meu titulo")

cores <- ifelse(as.numeric(tarifa$nomConcessao)==1,3,8)

plot(tarifa$VlrTotaTRFConvencional,tarifa$VlrTRFBrancaPonta,
     xlab="Tarifa Convencional", 
     ylab="Tarifa Branca horario Ponta",
     main="Dispresão tarifa convencional e tarifa branca",
     col=cores,
     pch=16)
legend(.4,2,legend=c("Concessionaria","Permissionaria"),fill=c(3,8))
identify(tarifa$VlrTotaTRFConvencional,tarifa$VlrTRFBrancaPonta,
         labels = tarifa$SigDistribuidora)

plot(1:20,col=1:20,pch=16)

pie(summary(tarifa$nomConcessao))
locator(1)
legend(-1.08,-.67,legend = c("Concessionária = 53","Permissionaria 50"))

pie(summary(tarifa$nomConcessao),main="Pizza meio a meio",
    col=c(3,8))



texto <- paste(levels(tarifa$nomConcessao),summary(tarifa$nomConcessao))
legend(locator(1),legend = texto)
text(locator(1),"Acho esse gráfico muito importante")

matriz2 <- as.matrix(obitos[-nrow(obitos),-c(1,ncol(obitos))])

anos <- obitos$Ano.do.Óbito[-length(obitos$Ano.do.Óbito)]
anos <- as.numeric(as.character(anos))
taxa <- obitos$X1.Região.Norte[-length(obitos$X1.Região.Norte)]
plot(anos,taxa,type="b",ylim=c(min(matriz2),max(matriz2)))
points(anos,obitos$X2.Região.Nordeste[-nrow(obitos)],col=2,)
lines(anos,obitos$X3.Região.Sudeste[-nrow(obitos)],col=3)
lines(anos,obitos$X4.Região.Sul[-nrow(obitos)],col=4)
points(anos,obitos$X5.Região.Centro.Oeste[-nrow(obitos)],col=5)

abline(h=mean(matriz2))
