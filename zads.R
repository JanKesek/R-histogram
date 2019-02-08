#Zad 1.
path<-getwd()
x <- c(80.45, 419, 203.62, 533.7, 245.22, 200.03, 56.06, 522.69, 408.49, 146.28, 69.68, 132.41, 567.07, 216.34, 177.51, 113.36, 110.89, 184.63, 70.85, 514.78, 120.41, 411.17, 487.01, 166.89, 199.69, 67.57, 612.57, 337.83, 239.7, 104.88, 68.21, 303.43, 768.38, 1185.76, 326.06, 373.93, 254.4, 1176.55, 144.26, 304.15, 1085.01, 273.39, 60.91, 227, 181.18, 70.18, 230.97, 99.31, 122.3, 1267.64, 108.85, 173.3, 624.29, 1018.57, 60.51, 365.37, 148.24, 803.63, 382.87, 275.97, 71.13, 707.33, 122.9, 337.42, 365.92, 649.69, 108.89, 382.84, 1234.31, 123.78, 269.17, 252.93, 997.34, 795.52, 297.03, 325.41, 175.01, 797.39, 491.35, 242.2, 59.49, 542.14, 99.56, 105.95, 602.48, 329.85, 739.1, 378.7, 309.82, 56.35, 840.76, 353.28, 212.33, 402.67, 198.69, 136.08, 58.39, 375.86, 198.53, 389.45, 245.95, 100.92, 247.21, 118.57, 569.29, 231.59, 140.23, 407.26, 453.06, 77.37, 127.33, 375.62, 234.12, 818.05, 351.68, 59.57, 235.25, 194.88, 1217.87, 145.96, 79.54, 482.7, 248.52, 250.27, 483.56, 85.23, 1398.74, 100.38, 430.42, 149.58, 123.85, 98.61, 60.81, 82.97, 221.66, 340.02, 246.8, 246.08, 141.33, 218.65, 493.23, 227.71, 142.63, 95.16, 598.05, 61.61, 741.08, 150.84)
summary(x)
# a)
# œrednia wynosi 335.77, a mediana 243.71
# b)
sd(x)
# odchylenie standardowe wynosi 290.3835
sd(x)/sqrt(length(x))
# B³ad œredniej wynosi 23.86938
# c)
# kurtoza i skoœnoœæ
hist(x,breaks=7,col="lightblue",border = "red")
rug(x,col="red")
grid()
lines(density(x),lty=2,lwd =2, col="red")
# jest to uk³ad normalny skoœny prawostronny

# Zad2
hist(x,breaks=11,plot=F)
# szereg rozdzielczy = 0 100 200 300 400 500 600 700 800 900 1000 1100 1200 1300 1400
getmode <- function(v) {
  v[which.max(table(v))]
}
getmode(x)
# Dominanta wynosi 80.45

#Zad 3.
# 300 sygna³ów
# PS=99,985 (sukces) PP=0,015 (pora¿ka)
# a) (znajduj¹ siê dwa zniekszta³cone sygna³y)
n = 300;q=0.015; lambda = q*n
(dpois(2, lambda=q*n))
# Prawdopodobnieñstwo wynosi 0.1124786

# b) co najmniej 3 zniekszta³cone sygna³y
#???
#3b
(ppois(3, lambda=3))
(sum(dpois(3:300, lambda=3)))
#dla conajmniej 3 sygna³ów wynosi: [1] 0.6472319
# [1] 0.5768099


# Zad 4.
lambda = 1.5 ; czas = 3
pexp(czas, rate=1/lambda, lower.tail=FALSE)
# prawdopodobieñstwo wynosi 0.1353353

# Zad 5.

#xad5
path <- getwd()
dane <- read.table(file=paste0(path,"/zad1.txt"), header = F,sep=";",dec=",")
attach(dane)
dane <- as.numeric(dane)
str(dane)

library(MASS)
#D³ugoœæ próby :
n<-length(dane)
#Åšrednia :
m.dane<-mean(dane,na.rm=TRUE)
#Odchylenie standard. : z próby :
s<-sd(dane,na.rm=TRUE)
#Kwantyl rozk³adu Normalego : alfa = 0.94
e<-qnorm(1-0.94/2)
#Margines b³êdu :
E<-e*s/sqrt(n)
#Przedzia³ ufnoœci :
m.dane+c(-E,+E)
#œredni wartoœæ wydatków mieszkaniowych: 333.9753 337.5686


#Zad 6.
# H0 œrednie s¹ jednakowe
#H1 œrednie s¹ ró¿ne
#dane
z6x<-c(36, 30, 43, 30, 28, 29, 28, 32, 29, 29, 41, 43, 31, 35, 28, 44)
z6y<-c(30, 45, 44, 40, 45, 40, 33, 29, 44, 44, 40)
alfa=0.08;n1<-length(z6x);n2<-length(z6y)
#Wartoœæ testowa
((mean(z6x)-mean(z6y))/sqrt(var(z6x)*(n1-1)+var(z6y)*(n2-1)))*sqrt(n1*n2*(n1+n2-2)/(n1+n2))
#wartoœæ krytyczna
qt(1-alfa/2,length(z6x)+length(z6y)-2)
# Wartoœæ testowa jest mniejsza od krytycznej wiêc odrzucamy H0

#Zadanie 8 /Analiza wariancji/
# Zebrano dane dotycz¹ce wydajnoœci pracowników (w %) oraz wysokoœci ich wynagrodzenia
# miesiêcznego (w z³):
# Grupy wydajnoœci pracowników X=[1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3]
# Wynagrodzenie Y=[1802.5, 1992.6, 1854.4, 1880.5, 1761.5, 1900.6, 1664.4, 1755.6, 1823.55, 1862.6,
# 1877.5, 1710.5, 1882.4, 1720.6,1950.4]
# Na podstawie powy¿szych danych na poziomie ufnoœci 0,98 zweryfikowaæ hipotezê o równoœci
# wynagrodzenia dla trzech wybranych podgrup wydajnoœci.

x$grupa <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3);
x$zl <- c(1802.5, 1992.6, 1854.4, 1880.5, 1761.5, 1900.6, 1664.4, 1755.6, 1823.55, 1862.6, 1877.5, 1710.5, 1882.4, 1720.6,1950.4);

#wykres
boxplot(x$zl~x$grupa,col="red",border="darkblue",pch=19,main="œrednie wynagrodzenia")
points(c(1,2,3),tapply(x$zl,x$grupa,mean),col="yellow",pch=19)

tapply(x$z,x$grupa,mean)
tapply(x$z,x$grupa,sd)

#analiza wariancji
av <- aov(Czas ~ Stanowisko,data=zad1)
summary(av)

#Zadanie 9 /Korelacja i regresja/
# Zebrano dane dotycz¹ce wydajnoœci pracy (w %) oraz œredniego dochodu (w tys. z³) w poszczególnych
# miesi¹cach danego roku:
# Wydajnoœæ X=[84, 116, 88, 152, 116, 124, 148, 168, 164, 172, 180, 172]
# Dochód Y=[3.1, 3.5, 3.7, 4.5, 4.2, 3.4, 4.3, 4.8, 4.2, 4.4, 5.1, 5.4]
# Na podstawie powy¿szych danych:

zad9$wydajnosc <- b(84, 116, 88, 152, 116, 124, 148, 168, 164, 172, 180, 172);
zad9$dochod <- b(3.1, 3.5, 3.7, 4.5, 4.2, 3.4, 4.3, 4.8, 4.2, 4.4, 5.1, 5.4);
summary(zad9$wydajnosc)
summary(zad9$dochod)
# a) Wyznaczyæ model, w którym dochód zale¿y od wydajnoœci; opisaæ jaki wp³yw ma zmiana
# wydajnoœci pracy na zmianê dochodu,

plot(wydajnosc~dochod, data=zad9, col=rgb(0,0,1,.6),pch=19)

model <- lm(wydajnosc~dochod, data=zad9)
model

#wydajnosc = 41.06 (+-8.057) * dochod + -32.811 (+-34.398) + ??
#odp: Zatem zwiêkszenie produkcji o 1 spowoduje zwiêkszenie kosztów o oko³o 41.06 z³.

# b) W jakiej czêœci zmiennoœæ dochodu jest wyjaœniana poprzez zbudowany model?

model <- lm(wydajnosc~dochod, data=zad9)
summary(model)

# odp: Na podstawie wspó³czynnika determinacji R2 = 0.6942 mo¿na powiedzieæ,
# ¿e zmiennoœæ Kosztów jest wyjasniona w 69% przez zbudowany model.
