#Solucion Pregunta 1
#Que tinguin mín 2 TV --> persones A Pi = 0.32
#Que tinguin màx una TV --> persones B Pi = 0.68

#ASSAIG DE BERNOULLI
x <- c(0,1)
fx <- c(0.68, 0.32)
fx[1]
fx[2]
#taula
cbind(x, fx)
#gràfic x-fx, entre els valors 0-1, tipus horitzontal i de color vermell
plot(x, fx, ylim=c(0,1), type="h", col="red")
#gràfic x-fx on veiem els punts (pch) de color vermell
points(x, fx, pch=16, col="red")


#construir una mostra aleatoria a n perosnes (replace true vol dir que quan entra una persona pot tornar a entrar mes tard)
n <- 400000
mostra <- sample(x, n, fx, replace=TRUE)
fi <- table(mostra)/n
fi
#freqüència relativa
br <- barplot(fi, ylim=c(0,1))
#funció de massa de probabilitat
lines(br, fx, ylim=c(0,1), type="h", col="red")
points(br, fx, pch=16, col="red")
#promitg (barplot)
xbar<- mean(mostra)
xbar
#valor esperat (punts)
mu <- sum(x*fx)
mu
#dades variança mostral
ssq <- var(mostra)
ssq
#variança de la funció massa de probabilitat
sigmasq <- sum((x-mu)^2*fx)
sigmasq
fx[1]*fx[2] #en bernoulli
#desviació típica de la funció massa de probabilitat
sqrt(sigmasq)


#calvo


#EXERCICI
#apartat 1.
n <- 43
set.seed(123) #és aleatori pero igual per tothom x tenir mateixos resultats
mostra <- sample(x, n, fx, replace=TRUE)
mostra
#encuesta
y <- function(i){sum(sample(x, n, fx, replace=TRUE))}
y(1) #resultat d'una encuesta

#bucle en R
m <- 400000 #resultat de m encuestas de n=43
encuestas <- sapply(1:m, y)
fi <- table(encuestas)/m
data.frame(fi)

dbinom(13, 43, 0.32)

#taula de probaibilitats
resultats <- 1:43
fy <- dbinom(resultats, 43, 0.32)
tauladeprob <- cbind(resultats, fy)
tauladeprob
#solucio es el valor 13 de tauladeprob

#apartat 2.
n <- 44
resultats <- 1:44
fy <- dbinom(resultats, 44, 0.32)
tauladeprob <- cbind(resultats, fy)
plot(resultats,fy, type='h', col="red", ylim=c(0,0.2))

Fy <- cumsum(fy)     
tauladeprob <- cbind( resultats, fy, Fy)
tauladeprob
plot(Fy, type='s', col="red")

pbinom(17, 44, 0.32)

#apartat 3.
resultats <- 1:24
fy <- dbinom(resultats, 24, 0.68)
Fy <- cumsum(fy)
tauladeprob <- cbind(resultats, fy, Fy)
tauladeprob
mu <- sum(resultats*fy)
mu
sigmasq <- sum((resultats-mu)^2*fy)
qbinom (0.25, 24, 0.68)
sigmasq
plot(Fy, type='s', col="red")
