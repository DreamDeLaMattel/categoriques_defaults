getwd()
setwd("C:/Users/los40/Desktop/ESTADISTICA APLICADA 5 SEMESTRE/Analisis de datos categoricos/PROJECTE/latex")

#####Llegir dades, prefereixo pasar les dades a .csv i llegirles en aquest format per evitar errors, i problemes amb les variables. Cal dir que les variables es llegeixen com a factors (de la manera samuel com a caracters) tot i que es practicament irrelevant
Dades<-read.csv("default of credit card clients.csv", header = TRUE, sep=",")

######Checar que tot estigui be
head(Dades,3) ; names(Dades) ;length(Dades) ;dim(Dades); str(Dades);

######Eliminar primera fila perque son les explicacions de les dades (o noms opcionals)
Dades<-Dades[-1,]


######Agafem variables interesants, agafo edat i prestec per si les agafem tambe i les convertim en factors
dades<-Dades[,c(2,3,4,5,6,25)]
head(dades)
names(dades); length(dades); dim(dades)

######Cambi de nom de les variables
names(dades)<-c("Prestec","Sexe","Educacio","Estat.c","Edat","Falta"); names(dades)

######Comprovacions del data set
any(is.na(dades))     #comprovar si hi ha NA

class(dades$Prestec)     #comprovar tipus. Son factors! convertir Prestec i Edat
class(dades$Sexe)
class(dades$Educacio)
class(dades$Estat.c)
class(dades$Edat)     
class(dades$Falta)     #abans pero, comprovar nivells i max min valors.

table(dades$Prestec)     #prestecs de 10,000 a 1,000,000
levels(dades$Sexe)     #Nivells de 1 a 2 pero + sex
levels(dades$Educacio)#Nivells de 0 a 6????? podem suposar que 0 son valors perduts, i han aumentat les categories?
table(dades$Educacio)
levels(dades$Estat.c)#Nivells de 0 a 3???? podem suposar que 0 son valors perduts?
table(dades$Estat.c)
table(dades$Edat)     #edats de 21-79
levels(dades$Falta)
table(dades$Falta)
### Plantejar que fer amb les variables Educacio i Estat.c, si tractar els 0 com a valors perduts, i que fer amb els nivells extres d'educacio

######De moment, per convertir variables en els factors coneguts actuals, i ordenar nivells seria
dades$Prestec<-as.numeric(as.character(dades$Prestec))
dades$Sexe<-factor(dades$Sexe,levels=c(1,2),label=c("M","F"))
dades$Educacio<-factor(dades$Educacio,levels=c(0,1,2,3,4,5,6))     #fins que no decidim, poso 4
dades$Estat.c<-factor(dades$Estat.c,levels=c(0,1,2,3))     #fins que no decidim, poso 3
dades$Edat<-as.numeric(as.character(dades$Edat))
dades$Falta<-factor(dades$Falta,levels=c(0,1),labels=c("No","Si"))

######si volem utilitzar edat, i prestec podem recodificarla i utilitzarla com a factor, sino es igual...

recode.edat<-function(x){
  for (i in 1:length(x)){
    if (x[i]>=21 & x[i]<30) {dades$Edat[i]<<-'1'} else
      if (x[i]>=30 & x[i]<50) {dades$Edat[i]<<-'2'} else
        if (x[i]>=50 & x[i]<80) {dades$Edat[i]<<-'3'} 
  }
}
recode.edat(dades$Edat)
dades$Edat<-factor(dades$Edat,levels=c("1","2","3")); class(dades$Edat); table(dades$Edat); sum(table(dades$Edat))

recode.Prestec<-function(x){
  for (i in 1:length(x)){
    if (x[i]>=10000 & x[i]<50000) {dades$Prestec[i]<<-'1'} else
      if (x[i]>=50000 & x[i]<150000) {dades$Prestec[i]<<-'2'} else
        if (x[i]>=150000 & x[i]<500000) {dades$Prestec[i]<<-'3'} else
          if (x[i]>=500000 & x[i]<=1000000) {dades$Prestec[i]<<-'4'}
  }
}
recode.Prestec(dades$Prestec)  
dades$Prestec<-factor(dades$Prestec,levels=c(1,2,3,4),labels=c("10k-50k","50k-150k","150k-500k","500k-1m"))
class(dades$Prestec); table(dades$Prestec); sum(table(dades$Prestec))



######Descritptiva univariant

str(dades)
head(dades)
dim(dades)
n<-(dim(dades))[1]


tPrestec<-table(dades$Prestec)/n
tEducacio<-table(dades$Educacio)/n
tEstat.c<-table(dades$Estat.c)/n
tEdat<-table(dades$Edat)/n
tFalta<-table(dades$Falta)/n

layout(matrix(c(1,1,1,2,2,2,3,3,4,4,5,5),2,6,byrow=TRUE))
barplot(tPrestec,main="Prestec")
barplot(tEducacio,main="Educacio")
barplot(tEstat.c,main="Estat civil")
barplot(tEdat,main="Edat")
barplot(tFalta,main="Falta de pagament")




######Descriptiva bivariant

#creando tabla de contingencia (?) , creando vectores para hacer un frame 
table(dades$Prestec, dades$Falta); nivell1<-c(2756,1555);nivell2<-c(8212, 2767);nivell3<-c(11572,2210);nivell4<-c(824,104)
default.prestec = as.data.frame(rbind(nivell1,nivell2,nivell3,nivell4))

names(default.prestec)<-c("Pagament", "Falta") #asignaci??? de nombres en las columnas
chisq.test(default.prestec) #test de independencia chi quadrado

names(default.prestec)<-c("Pagament", "Falta")
ind1c<-chisq.test(default.prestec)


###interpretaciones del p-valor bajo
###1. It provides strong evidence to suggest that gender and ice cream flavour preference are dependent or have some association. (This is a probabilistic interpretation, but it is not very clear what it means on a practical level.) 
## 2. It provides strong evidence to suggest that men and women tend to have difference preferences for ice cream flavours. (This is a practical implication.)


table(dades$Educacio, dades$Falta); edu1<-c(8549,2036); edu2<-c(10700,3330);edu3<-c(3680,1237);edu4<-c(116,7)
default.Educacio = as.data.frame(rbind(edu1,edu2,edu3,edu4))
names(default.Educacio)<-c("Pagament", "Falta")
ind2c<-chisq.test(default.Educacio)



table(dades$Estat.c, dades$Falta); casat<-c(10453,3206); solter<-c(12623,3341); altres<-c(239,84);
table(dades$Estat.c, dades$Falta); casat<-c(10453,3206); solter<-c(12623,3341); altres<-c(239,84)
default.Estat.c= as.data.frame(rbind(casat,solter, altres))
names(default.Estat.c)<-c("Pagament", "Falta")
ind3c<-chisq.test(default.Estat.c)

table(dades$Edat, dades$Falta); menors30<-c(7421,2197); menors50<-c(13941,3761);menors80<-c(2002,678)
default.Edat = as.data.frame(rbind(menors30, menors50, menors80))
names(default.Edat)<-c("Pagament", "Falta")

chisq.test(default.Edat)

#interpretacions p-valors, tots son menors que 0.05, per tant podem dir que son dependents.

ind4c<-chisq.test(default.Edat)


indcpvalues<-c(ind1c$p.value, ind2c$p.value, ind3c$p.value, ind4c$p.value)
names(indcpvalues)<-c("Prestec","Educacio","Estat.Civil","Edat")




# REGRESIO LOGISTICA ####


dades$Prestec<-as.numeric(as.character(dades$Prestec))
dades$Sexe<-as.numeric(dades$Sexe,levels=c("M","F"))
dades$Educacio<-as.numeric(dades$Educacio,levels=c(1,2,3,4))     #fins que no decidim, poso 4
dades$Estat.c<-as.numeric(dades$Estat.c,levels=c(1,2,3))     #fins que no decidim, poso 3
dades$Edat<-as.numeric(as.character(dades$Edat))
dades$Falta<-as.numeric(as.character(dades$Falta))

summary(dades)

par(mfrow=c(2,2))
plot(dades$Edat,dades$Falta,xlab="Edat",ylab="Falta de pagament")
plot(dades$Educacio,dades$Falta,xlab="Educacio",ylab="Falta de pagament")
plot(dades$Estat.c,dades$Falta,xlab="Estat civil",ylab="Falta de pagament")
plot(dades$Prestec,dades$Falta,xlab="Prestec",ylab="Falta de pagament")

