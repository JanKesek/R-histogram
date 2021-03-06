#rozklad normalny
x<-seq(-4,4,length.out =  100)
dx<-dnorm(x)
px<-pnorm(x)
plot(x,dx,type = "l",lty=2,xlab="x val", ylim = c(0,1), col="red")
ipl <- x >= -1 & x<=1
polygon(c(-1,x[ipl],1),c(0,dx[ipl],0), border="red", col = "blue")
lines(x,px,lwd=2,col="black")
lines(c(0,0), c(0,0.4), lwd=3, col="yellow")

#normalny skosny prawy
rx<-rsnorm(100,mean=1,sd=1, xi=-3.5)
hist(rx, breaks = 11, probability = T, col = "lightblue", border = "red")
rug(rx, col = "red4")
lines(density(rx), lty=2, lwd=2, col="red")
lines(c(-1.75,-1.75), c(0,0.4), lwd=3, col="darkblue")
lines(c(-1.18, -1.18), c(0,0.4), lwd=3, col="darkgreen")
lines(c(-0.85, -0.85), c(0,0.4), lwd=3, col="darkgray")


#blad standardowy

sd(dane$wynik1)/sqrt(length(dane$wynik1))

#rozklad bernoulliego
colors2<-c("red","blue", "darkgreen", "gold","black")
s<-c(50,60,70,80,90)
x<-seq(0,50)
labels <- c("50 proba", "60 proba", "70 proba" ,
            "80 proba", "90 proba", "100")
dx<-dbinom(x,size=100, prob = 0.3)
plot(x,dx,type="l", col="green", ylim=c(0,2),
     main="rozklad Bernoulliego")
for (i in 1:5) {
  dx<-dbinom(x,size = s[i], prob = .3)
  lines(x,dx,col=colors2[i])
}
legend("topright", inset = .05, title = "Distributs", labels,
       lty = c(1,1,1,1,1,1), col = c(colors2, "green"))


colors <- c("red", "blue", "darkgreen", "gold", "black")
s<-c(.2,.3,.5,.6,.9)
x<-seq(0,50)
labels <- c("pr = .1","pr = .2", "pr = .3", "pr = .5", "pr = .6", "pr = .9")
dx<-dbinom(x,size=50,prob=.1)
plot(x, dx, type=�l�,col=�green�,ylim=c(0,.2),main="Rozk�ad Bernouliego")
for(i in 1:5){
  dx <- dbinom(x,size=50,prob=s[i])
  lines(x, dx, col=colors[i])
}
legend("topright", inset=.05, title="Distributions",
        labels, lty=c(1, 1, 1, 1, 1,1), col=c(colors,"green")) 




Prawdopodobie�stwo b��du jednego bitu podczas transmisji cyfrowej w jednym bajcie wynosi 0,02.
W transmisje mo�na przeprowadza� dowoln� liczb� razy.
Zak�adamy, �e transmisje s� niezale�ne .
Obliczy�

jakie jest prawdopodobie�stwo, �e drugi z kolei b��d wyst�pi dok�adnie w w �smym bicie?
  jakie jest prawdopodobie�stwo, �e drugi z kolei b��d zdarzy si� najp�niej w �smym bicie?
  jakie jest prawdopodobie�stwo, �e druga z kolei b��d nie zdarzy si� w pierwszych o�miu bitach?
  
  
  sukces<-0.98
  porazka<-0.02
  a.)
  p<-dbinom(1,size=7,prob = 0.02)
  p*porazka
  b.)
  p<-dbinom(2,size = 8, prob = 0.02)
  c,)
  p<-sum(dbinom(c(0,1), size = 8, prob = 0.02))
  
  Bernoulli i Poisson
  Je�li wiadomo, �e przeci�tnie 1% procesor�w danej firmy jest wadliwych, to jakie 
  jest prawdopodobie�stwo tego, �e w partii 410 procesor�w 6 b�dzie wadliwych?

n<-410
  q<-0.01
  dpois(6,q*n)
  
  W pewnej sieci �rednio zg�asza si� 3 awarie tygodniowo. 
  Przyjmuj�c, �e liczba awarii polega rozk�adowi 
  Poissona, obliczy� prawdopodobie�stwo tego, �e w losowo wybranym tygodniu 
  pracy zdarz� si� 3 awarie (0 awarii, 1 awaria, 2 awarie, co najwy�ej 3 awarie).
sum(dpois(0:3,3))

3^3*exp(-3)/factorial(3)
(3^3*exp(-3)/factorial(3))


Czas oczekiwania (w dniach) na z�o�enie komputera w pewnej firmie jest
zmienn� losow� o rozk�adzie jednostajnym w przedziale <2dni; 14dni>.
Obliczy� prawdopodobie�stwo tego, �e komputer b�dzie z�o�ony w czasie 
kr�tszym ni� 6 dni. Obliczy� prawdopodobie�stwo tego, �e komputer 
nie b�dzie z�o�ony w czasie kr�tszym ni� 6 dni.

bedzie:
  punif(6,min = 2, max = 14, lower.tail = T)
nie bedzie:
  punif(6, min = 2, max = 14, lower.tail = F)



Do�wiadczenie pokazuje, �e tygodniowe dochody z reklamy pewnego portalu 
internetowego maj� rozk�ad normalny z warto�ci� 
oczekiwan� 96 tys. z� i odchyleniem 
standardowym 6 tys. z�.

A.) Jakie jest prawdopodobie�stwo, �e dochody z 
reklamy w pewnym tygodniu b�d� mniejsze ni� 84 tys. z�

pnorm(84, 96, 6)

B.)
Jakie jest prawdopodobie�stwo, �e dochody z reklamy
w pewnym tygodniu b�d� wi�ksze ni� 118 tys. z�.
pnorm(118, 96, 6, lower.tail = F)

Jakie jest prawdopodobie�stwo, �e dochody z reklamy
w pewnym tygodniu b�d� r�wne 96 tys. z�.




Czas bezawaryjnej pracy pewnego urz�dzenia ma rozk�ad wyk�adniczy
z parametrem ?? = 3 godziny.
A.) Obliczy� warto�� przeci�tn� bezawaryjnego czasu pracy urz�dzenia.
pexp(3, 1/3, lower.tail = F)

B.) Obliczy� prawdopodobie�stwo, �e bezawaryjny
czas pracy urz�dzenia wynosi co najmniej 3 godziny.




Na podstawie 64 losowo wybranych rozm�w telefonicznych obliczono �redni� d�ugo�� 
rozmowy, kt�ra wynios�a 4.2 minuty. Z poprzednich bada� wiadomo, �e wariancja 
d�ugo�ci rozm�w telefonicznych wynosi 1.44 minuty 2.

Zak�adaj�c, �e czas rozm�w ma rozk�ad normalny oszacowa� przedzia�owo �redni� 
d�ugo�� rozmowy telefonicznej na poziomie ufno�ci 0.95 .

4.2+c(-(-(-qnorm(1-0.05/2)) * sqrt(1.44) / sqrt(64)),
      (-(-qnorm(1-0.05/2)) * sqrt(1.44) / sqrt(64)))

