# practica-23
polizas<- seq(0,80,by=1)
polizas
n=80
p=.20
pgm<- dbinom(polizas,n,p)

#Distribucion acumulada
FXGM <- pbinom(polizas, n, prob=p)
FXGM
plot(polizas,FXGM,type="s", ylab="Probabilidad acumulada")
points(polizas,pgm,type="b", col="red")
media_gm=n*p
media_gm
varianza_gm=n*p*(1-p)
varianza_gm
desvest_gm=varianza_gm^.5
desvest_gm

#obtenga el valor de E[(x^2)-2x]
val_esp<- sum(((polizas^2)-2*polizas)*pgm)
val_esp

#percentiles
gm_per<- seq(0,1,by=0.05)
gm_per
percentil70<- qbinom(0.70, n,prob=p)
percentil70

percentiles_gm<- qbinom(gm_per,n,prob=p)
percentiles_gm
#informe_gm_percentiles

#para calcular la probabilidad de tener cinco siniestros
probgm_1 <-dbinom(5,n,prob=p)
probgm_1

#calcular la probabilidad de tener p[1<=x<8]=p[x<=7]-p[x<=0]
probgm_3=pbinom(7,n,prob=p)-pbinom(0,n,prob=p)
probgm_3

#simular valores con distribucion binomial
simulados_gm<- rbinom(10000,n,prob=p)
simulados_gm
min(simulados_gm)
max(simulados_gm)
hist(simulados_gm,col="4")
media_gmsim<-mean(simulados_gm)
media_gmsim
varianza_gm<-var(simulados_gm)
varianza_gm


##El banco A ofrece prestamos educativos a estudiantes de la ciudad de nueva york,
#la tasa de incumplimiento es de 45, y la probabilidad de que sean a nivel posgrado es de un 32%,
# y la licenciatura 25%.
#probabilidades, distribucion acumulada
#percentiles
#media, varianza, desvest
#valor esperado E[2x+x^2]
#probabilidad para ambos casos
#histogramas
#realizar las simulaciones para ambos casos comparar que tan similares son a la distribucion original

polizas1 <- seq(0,80, by=1)
lamdaposgrado=45*.32
lamdalicenciatura=45*.25
posgrado<- dpois(polizas1,lambda = lamdaposgrado,)
hist(posgrado)
licenciatura<- dpois(polizas1,lambda = lamdalicenciatura)
hist(licenciatura)

#usando la distribución de Poisson
acpos <- ppois(polizas1,lambda = lamdaposgrado)  
aclic <- ppois(polizas1,lambda=lamdalicenciatura )

#graficamos la acumulada de posgrado
plot(polizas1,acpos)
lines(polizas1,posgrado)

#graficamos la acumulada de licenciatura
plot(polizas1,aclic)
lines(polizas1,licenciatura)

#media de ambas
media_posgrado=lamdaposgrado
media_licen=lamdalicenciatura
media_licen
media_posgrado

#varianza de ambas
varianza_posgrado=lamdaposgrado
varianza_licen=lamdalicenciatura
varianza_licen
varianza_posgrado

#desviación estándar
desv_posgrado=varianza_posgrado^.5
desv_licen=varianza_licen^.5
desv_licen
desv_posgrado

#obtenga el valor de E[(x^2)-2x] E[2x+x^2]
val_esppos<- sum(((polizas1*2)+polizas1^2)*posgrado)
val_esppos
val_esplic<- sum(((polizas1*2)+polizas1^2)*licenciatura)
val_esplic

#percentiles
gm_per<- seq(0,1,by=0.05)
percentiles_gmposgrado<- qpois(gm_per,lambda = lamdaposgrado)
percentiles_gmlicenci<- qpois(gm_per,lambda = lamdalicenciatura)
percentiles_gmlicenci

#####probabilidades
#probabilidad P[x<=5]
probposg <- ppois(5,lambda = lamdaposgrado)
probposg
problic <- ppois(5,lambda = lamdalicenciatura)
problic

#p[7<=x<=11]
probposg711 <- ppois(11,lambda = lamdaposgrado)-ppois(7,lambda = lamdaposgrado)
probposg711
problic711 <- ppois(11,lambda = lamdalicenciatura)-ppois(7,lambda = lamdalicenciatura)
problic711

#simular valores con distribucion poisson
#simulados posgrado
simulados_posgrado<- rpois(10000, lambda = lamdaposgrado)
simulados_posgrado
min(simulados_posgrado)
max(simulados_posgrado)
hist(simulados_posgrado,col="3")
media_simpos<-mean(simulados_posgrado)
media_simpos
varianza_simpos<-var(simulados_posgrado)
varianza_simpos

#simulados licenciatura
simulados_licenciatura<- rpois(10000, lambda = lamdalicenciatura)
simulados_licenciatura
min(simulados_licenciatura)
max(simulados_licenciatura)
hist(simulados_licenciatura,col="4")
media_simlic<-mean(simulados_licenciatura)
media_simlic
varianza_simlic<-var(simulados_licenciatura)
varianza_simlic
