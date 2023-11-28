mediciones <- read.csv("tf6a15.CSV", as.is=FALSE)
filtro=subset(mediciones,edad %in% c(6,7,8,9),select=estado:imc)
attach(filtro)
diff(range(edad))
detach(filtro)
summary(filtro)
attach(filtro)
summary(estado)
detach(filtro)
dft=subset(filtro,estado %in% c("MEXICO","PUEBLA","NUEVO LEON","VERACRUZ","MORELOS","ZACATECAS","SONORA","DURANGO"),select=estado:imc)
dft$estado <- factor(dft$estado)
summary(dft)
##Grafica de Frecuencia
attach(dft)
a <- table(estado)
b <- ordered(a)
Tot <- sum(a,na.rm=FALSE)
Porcentaje <- round((a/Tot)*100, digits=2)
Porcentaje <- paste(Porcentaje,"%")
pie (a, labels = Porcentaje, col=rainbow(length(b)), main="Frecuencia x Estado")
Estados <- levels(dft$estado)
legend(x="topright", legend=c(Estados), fill=rainbow(length(b)))
detach(dft)
##Grafica de Poblacion por sexo
attach(dft)
c <- table(sexo,estado)
barplot(c, col=c("blue","pink"), cex.names = 0.8,legend.text=rownames(c))
detach(dft)
attach(dft)
table(estado, edad)
d <- table(estado,edad)
e <- table(estado,sexo)
Dur_pedad <- round(prop.table(d[1,1:4],margin=NULL),2)*100
Dur_pedad <- paste(Dur_pedad,"%")
Mex_pedad <- round(prop.table(d[2,1:4],margin=NULL),2)*100
Mex_pedad <- paste(Mex_pedad,"%")
Mor_pedad <- round(prop.table(d[3,1:4],margin=NULL),2)*100
Mor_pedad <- paste(Mor_pedad,"%")
NL_pedad <- round(prop.table(d[4,1:4],margin=NULL),2)*100
NL_pedad <- paste(NL_pedad,"%")
Pue_pedad <- round(prop.table(d[5,1:4],margin=NULL),2)*100
Pue_pedad <- paste(Pue_pedad,"%")
Son_pedad <- round(prop.table(d[6,1:4],margin=NULL),2)*100
Son_pedad <- paste(Son_pedad,"%")
Ver_pedad <- round(prop.table(d[7,1:4],margin=NULL),2)*100
Ver_pedad <- paste(Ver_pedad,"%")
Zac_pedad <- round(prop.table(d[8,1:4],margin=NULL),2)*100
Zac_pedad <- paste(Zac_pedad,"%")
edades <- c("7","8","9","10") 
estado_pedad <- data.frame(edades,Durango=Dur_pedad,Mexico=Mex_pedad,Morelos=Mor_pedad,NuevoLeon=NL_pedad,Puebla=Pue_pedad,Sonora=Son_pedad,Veracruz=Ver_pedad,Zacatecas=Zac_pedad)
Dur_sexo <- round(prop.table(e[1,1:2],margin=NULL),2)*100
Dur_sexo <- paste(Dur_sexo,"%")
Mex_sexo <- round(prop.table(e[2,1:2],margin=NULL),2)*100
Mex_sexo <- paste(Mex_sexo,"%")
Mor_sexo <- round(prop.table(e[3,1:2],margin=NULL),2)*100
Mor_sexo <- paste(Mor_sexo,"%")
NL_sexo <- round(prop.table(e[4,1:2],margin=NULL),2)*100
NL_sexo <- paste(NL_sexo,"%")
Pue_sexo <- round(prop.table(e[5,1:2],margin=NULL),2)*100
Pue_sexo <- paste(Pue_sexo,"%")
Son_sexo <- round(prop.table(e[6,1:2],margin=NULL),2)*100
Son_sexo <- paste(Son_sexo,"%")
Ver_sexo <- round(prop.table(e[7,1:2],margin=NULL),2)*100
Ver_sexo <- paste(Ver_sexo,"%")
Zac_sexo <- round(prop.table(e[8,1:2],margin=NULL),2)*100
Zac_sexo <- paste(Zac_sexo,"%")
sexos <- c("Hombre","Mujer")
estados_psexos <- data.frame(sexos,Durango=Dur_sexo,Mexico=Mex_sexo,Morelos=Mor_sexo,NuevoLeon=NL_sexo,Puebla=Pue_sexo,Sonora=Son_sexo,Veracruz=Ver_sexo,Zacatecas=Zac_sexo)
estado_pedad
estados_psexos
detach(dft)
attach(dft)
boxplot(porcentajegrasa ~ sexo, col=c("blue","pink"), outcex=0)
detach(dft)
attach(dft)
boxplot(estatura ~ estado, col=rainbow(length(b)), outcex=0, xlab="")
legend(x="bottomright", legend=c(Estados), fill=rainbow(length(b)))
detach(dft)
attach(dft)
##DECILES
quantile(cintura, seq(0.1,1, by=0.1))
##PERCENTIL 20, 40, 60, 80
quantile(cintura, seq(0,1, by=0.2))
##INTERVALOS
intervalos <- cut(cintura, breaks=c(0,55,59,63,70,100))
##MOSAICO
mosaicplot(table(intervalos,estado))
detach(dft)
attach(dft)
##Histogramas
hist(estatura, col="lightblue")
##Media
mean(estatura)
##Mediana
median(estatura)
hist(peso, col="orange")h
hist(peso, col="red", breaks=30)
##MEDIA MEDIANA Y MODA PESO
library(DescTools)
mean(peso)
median(peso)
Mode(peso)
##OJIVA
intervalos2 <-cut(peso, breaks=c(14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76))
casos_por_clase <- table(intervalos2)
casos_relativos <- table((intervalos2)/Tot)
casos_porcentuales <- round(casos_relativos*100,digits=2)
plot(cumsum(casos_por_clase), xlab="clase", ylab="Casos por clase")
lines(cumsum(casos_por_clase))
detach(dft)
attach(dft)
##CINTURA vs EDAD
table(cintura,edad)
mean(table(cintura,edad))
sd(table(cintura,edad))
sd(table(cintura,edad))/mean(table(cintura,edad))*100
##PESO vs EDAD
table(peso,edad)
mean(table(peso,edad))
sd(table(peso,edad))
sd(table(peso,edad))/mean(table(peso,edad))*100
detach(dft)
attach(dft)
##DIAGRAMAS DE DISPERSION
##PESO vs EDAD
plot(peso,edad)
##ESTATURA vs EDAD
plot(estatura,edad)
##IMC vs EDAD
plot(imc,edad)
detach(dft)
attach(dft)
ocho=subset(dft,edad %in% c(8), select=estado:imc)
ochoH=subset(ocho, sexo %in% c("Hombre"), select=estado:imc)
ochoM=subset(ocho, sexo %in% c("Mujer"), select=estado:imc)
attach(ochoH)
boxplot(imc ~ estado, col=rainbow(length(b)))
detach(ochoH)
attach(ochoM)
boxplot(imc ~ estado, col=rainbow(length(b)))
detach(ochoM)
nueve=subset(dft,edad %in% c(9), select=estado:imc)
nueveH=subset(nueve, sexo %in% c("Hombre"), select=estado:imc)
nueveM=subset(nueve, sexo %in% c("Mujer"), select=estado:imc)
attach(nueveH)
boxplot(imc ~ estado, col=rainbow(length(b)))
detach(nueveH)
attach(nueveM)
boxplot(imc ~ estado, col=rainbow(length(b)))
detach(nueveM)
detach(dft)
attach(dft)
##GRAFICOS ADICIONALES
hist(cintura, col="lightgreen")
table(edad,imc)
plot(table(edad,estado), col=rainbow(length(b)))
detach(dft)
