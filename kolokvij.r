getwd()
setwd("C:/Users/FIFY/Desktop/statistika_1")
getwd()

#----------------------ZADATAK 7.6.---------------------------#

uvis<-read.csv2("uvis.csv",header=TRUE)
names(uvis)
edit(uvis)
dim(uvis)
attach(uvis)

edit(uvis)

z<-uvis$KOL.1
?ecdf
mode(KOL.1)
mode(OCJENA)
#dane avrijable su numerièkog tipa  (kol.1 je dnv, ocjena je diskretna numerièka avrijabla

#EMPIRIJSKA DISTRIBUCIJA VARIJABLE OCJENA
Fn<-ecdf(OCJENA)
Fn
plot(Fn)
Fn(OCJENA)
summary(Fn)

ocjena<-sort(OCJENA)
y<-(1/length(ocjena))*cumsum(rep(1,length(ocjena)))

#Procijenite vjerojatnost da je student kolokvij iz UVIS-a položio ocjenom vecom od 2, ali
#manjom od 5.
 max(y[ocjena<5])-max(y[ocjena<2])
 
 
#Nacrtajte histogram frekvencija i relativnih frekvencija za podatke koji su sadržani u varijabli
#OCJENA.
 
 win.graph(width=6, height=4, pointsize=8)
 par(mfrow=c(1,2))
 hist(OCJENA,freq=T)
 hist(OCJENA,probability=T)
 ?hist

#Za podatke sadržane u varijabli KOL-1 odredite vrijednosti aritmeticke sredine, moda (je li
#mod ovog niza podataka jedinstven?), donjeg kvartila, medijana i gornjeg kvartila. Ukratko
#protumacite znacenje svake od navedenih numerickih karakteristika.

KOL.1<-na.omit(KOL.1)
KOL.1
mean(KOL.1)
 #mod mozemo iscitati iz histograma i vidimo da nije jedinsven, postoje 2 jednakobrojno zapisana podatka

f.KOL.1<-table(uvis$KOL.1)
f.KOL.1
prop.table(f.KOL.1)
boxplot(KOL.2)
(f<-fivenum(KOL.2))
shapiro.test(KOL.2)  #p vrijednost nam je veæa od alfa koji iznosi 0.05, pa test ne ide u korist normalnosti sto znaci
#da odbacujemo H0 uz vjerojatnost da smo pri tome pogrijesili za najvise alfa

#alfa=0.05  , mu0=42.17391

#hipoteza
#H0: mu=42.17391
#H1: mu>42.17391
#alfa=0.05

KOL.P1<-na.omit(KOL.P1)
KOL.P1

x<-KOL.P1
n<-length(KOL.P1)
n

t.test(x,mu=42.17391 ,alternative="greater", conf.level=0.95)

#kako je p vrijednost manja od alfe, onda nemamo dovoljno dokaza za odbacivanje 
#nulhipote u korist alternativne
#koristili smo t-test iz razloga što nam je varijanca nepoznata 
#z-test koristimo kad nam je varijanca poznata




#Provoðenjem prikladnog statistickog testa provjerite je li na razini znacajnosti alfa = 0.05
#proporcija studenata koji su na drugom popravnom kolokviju (varijabla KOL-P2) prikupili
#više od 80 bodova statisticki znacajno razlicita od p0 = 0.1 Koji ste test odabrali i zašto?

#procjena proporcije(vjerojatnosti)->asimptotski test



#HIPOTEZA
#H0:p0=0.1
#H1:p1!=0.1 , provodim dvostrani test

KOL.P2<-na.omit(KOL.P2)
KOL.P2
x<-KOL.P2
x
vise<-x[x>80]
vise
length(vise)
n<-length(x)
n
?binom.test


binom.test(2,27, 0.1 ,alternative = "two.sided",conf.level = 0.95) 
 
# p vrijednost veca od alfa odbacujemo hpotezu h0 ,vjer je znacajno razlicita od 0.1
 
#teststat<-(0.39-0.1)/sqrt(0.09/27) 
#zalfa=qnorm(1-0.05/2,0,1)
#teststat
#c(-Inf,-zalfa, zalfa,Inf)
#test stat upada u krit podrucje pa odbacujemo hipotezu da je
  
  
#za n koji je dim uzorka to je dimenzija te avrijable koju promatram
#binomni test jer je uzorak mali 


#--------------------ZADATAK 7.4.-----------------#
 #Baza podataka glukoza.sta opisana je u primjeru 2.9. Poznato je da na nivou znacajnosti alfa= 0.05
#možemo prihvatiti hipotezu o normalnoj distribuiranosti podataka sadržanih u varijablama dob i
#glukoza.


#1. Intervalom pouzdanosti 95% procijenite ocekivanu koncentraciju glukoze.
#2. Postavite potrebne hipoteze i prikladnim testom provjerite je li na nivou znacajnosti alfa =
#0.05 ocekivana koncentracija glukoze statisticki znacajno veca od 5.5 mMol/L.
#3. Intervalom pouzdanosti 95% procijenite proporciju ispitanika kod kojih je koncentracija
#glukoze u krvi izmeðu 4 i 6 mMol/L.
#4. Postavite potrebne hipoteze i prikladnim testom provjerite je li na nivou znacajnosti alfa =
#0.05 proporcija ispitanika kod kojih je koncentracija gluoze veca od 8 mMol/l statisticki
#znacajni razlicita od 0.1.
#5. Protumacite sve dobivene rezultate u kontekstu promatranog problema.


glukoza<-read.csv2("glukoza.csv",header=T)
names(glukoza)
edit(glukoza)
attach(glukoza)
summary(glukoza)


#1. Intervalom pouzdanosti 95% procijenite ocekivanu koncentraciju glukoze.
  #---procjena oèekivanja!!
  #---varijanca nam nije poznata -->t-distribucija
  
x<-glukoza$koncentracija
x
n<-length(x)
n
pouzd=0.95


#napoznata varijanca
alfa <- 0.05
talfa <- qt(1-alfa/2,n-1)
dg <- mean(x) - talfa*sd(x)/sqrt(n-1)
gg <- mean(x) + talfa*sd(x)/sqrt(n-1)
c(dg,gg)
gg-dg

#kako da odna to interpretiram???



#2. Postavite potrebne hipoteze i prikladnim testom provjerite je li na nivou znacajnosti alfa =
#0.05 ocekivana koncentracija glukoze statisticki znacajno veca od 5.5 mMol/L.

#hipoteze:
#h0: mu=5.5
#h1: mu>5.5

#---procjena oèekivanja za velike uzorke,nepoznata varijanca

  t.test(x,mu=5.5,alternative="greater")

#kako mi je p vrijednost jako mala onda odbacujemo h0 u korist h1


#3. Intervalom pouzdanosti 95% procijenite proporciju ispitanika kod kojih je koncentracija
#glukoze u krvi izmeðu 4 i 6 mMol/L.
kon1<-x[x>=4&x<=6]
kon1

length(kon1)
y<-kon1
m<-length(y) 
m 

alfa<-0.05
 #koristim studentovu t-distr jer ne znam varijancu
talfa <- qt(1-alfa/2,m-1)
d <- mean(y) - talfa*sd(y)/sqrt(m-1)
g <- mean(y) + talfa*sd(y)/sqrt(m-1)
c(d,g)
g-d

 #kako da to interpretiram??
 
 
#4. Postavite potrebne hipoteze i prikladnim testom provjerite je li na nivou znacajnosti alfa =
#0.05 proporcija ispitanika kod kojih je koncentracija gluoze veca od 8 mMol/l statisticki
#znacajni razlicita od 0.1.

#provjera proporcije hipoteza, neponata varijanca

konc3<-x[x>8]
konc3

l<-length(konc3)
l

prop.test(28,102,alternative="greater")
#asimptotski test???

?prop.test


