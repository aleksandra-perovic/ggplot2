#instaliranje i ucitavanje paketa
#1) 
install.packages("ggplot2")
library(ggplot2)
#2)
install.packages("tidyverse")
library(tidyverse)

getwd()
setwd("C:/Users/USER/Downloads")

filmovi<-read.csv("Movie-Ratings.csv")

filmovi

#izmena naziva kolona
colnames(filmovi)<-c("Film","Zanr","OcenaKriticara","OcenaPublike","Budzet","Godina")

#uvid u strukturu fajla
head(filmovi)
summary(filmovi)
str(filmovi)

#kreiranje kategorija
filmovi$Godina<-factor(filmovi$Godina)
filmovi$Film<-factor(filmovi$Film)
filmovi$Zanr<-factor(filmovi$Zanr)

str(filmovi)

#-------------------Sloj podataka
ggplot(data=filmovi)

#-------------------Sloj estetike
ggplot(data=filmovi, aes(OcenaKriticara,OcenaPublike))
ggplot(data=filmovi, aes(x=OcenaKriticara,y=OcenaPublike))

#-------------------Sloj geometrije
ggplot(data=filmovi, aes(x=OcenaKriticara,y=OcenaPublike))+geom_point()
ggplot(data=filmovi, aes(x=OcenaKriticara,y=OcenaPublike))+geom_point()+geom_line()
ggplot(data=filmovi, aes(x=OcenaKriticara,y=OcenaPublike))+geom_line()+geom_point()

#sloje geometrije i settovanje
ggplot(data=filmovi, aes(x=OcenaKriticara,y=OcenaPublike))
      +geom_line(size=1.5, alpha=0.75, colour="Pink")

#-------------------Sloj skaliranja
#mapiranje
ggplot(data=filmovi, aes(x = OcenaKriticara, y = OcenaPublike, colour = Zanr)) + geom_point()

#automatski dodate skalirajuće funckije
ggplot(data=filmovi, aes(x = OcenaKriticara, y = OcenaPublike, 
                         colour = Zanr)) 
      + geom_point()
      + scale_x_continuous()
      + scale_y_continuous()
      + scale_colour_discrete()

#kastomizovanje skalirajućih funkcija
ggplot(data=filmovi, aes(x = OcenaKriticara, y = OcenaPublike, 
                         colour = Zanr)) + geom_point()+ scale_x_continuous(name = "kriticki sud")+ scale_y_continuous(limits = c(15,100))+ scale_colour_discrete()

#mapiranje vise estetskih atributa
ggplot(data=filmovi, aes(x = OcenaKriticara, y = OcenaPublike, colour = Budzet, size=Zanr)) + geom_point()

#mapiranje u okviru geometrijskog sloja
ggplot(data=filmovi, aes(x = OcenaKriticara, y = OcenaPublike, colour = Zanr)) + geom_point(aes(colour=Budzet,size=Godina)) 

#mapiranje izvan opsega na NA
ggplot(data=filmovi, aes(x = OcenaKriticara, y = OcenaPublike, 
                         colour = Budzet, shape=Zanr))+ geom_point()

ggplot(data=filmovi, aes(x = OcenaKriticara, y = OcenaPublike, 
                         colour = Budzet, shape=Zanr))+ geom_point()+ scale_shape_discrete(oob=scales::censor)

#-------------------Sloj statistike
#statistika u geometrijskom sloju

#geom_smooth
ggplot(data=filmovi, aes(OcenaKriticara, OcenaPublike)) + geom_point(aes(colour=Zanr)) + geom_smooth()
ggplot(data=filmovi, aes(OcenaKriticara, OcenaPublike)) + geom_point(aes(colour=Zanr)) + geom_smooth(fill=NA)

#boxplot
ggplot(data=filmovi, aes(x=Zanr, y=OcenaPublike, colour=Zanr)) 
      + geom_jitter()+geom_boxplot(size=1.2, alpha=0.7)
#violin
ggplot(data=filmovi, aes(x=Zanr, y=Godina, colour=Zanr)) +geom_violin(size=1.2,alpha=0.7, aes(fill=Zanr))


#-------------------Sloj koordinatnih sistema
#kartezijanski
ggplot(data=filmovi, aes(OcenaKriticara, OcenaPublike)) + geom_point(aes(colour=Budzet)) + geom_smooth()

ggplot(data=filmovi, aes(OcenaKriticara, OcenaPublike)) 
      + geom_point(aes(colour=Budzet)) 
      + geom_smooth()+coord_cartesian(xlim=c(75,100), ylim=c(60,100))

#polarni - PIE CHART
ggplot(data = filmovi, aes(x = factor(1), fill = Zanr))+ geom_bar(width = 1)

ggplot(data = filmovi, aes(x = factor(1), fill = Zanr))
      + geom_bar(width = 1) 
      + coord_polar(theta = "y")
      + scale_x_discrete(name=NULL)
      + scale_y_continuous(name=NULL)

#-------------------Sloj faseta
#facet_wrap()
ggplot(data=filmovi, aes(Budzet))+geom_histogram(aes(fill=Zanr), colour="Black")+facet_wrap(~Godina, ncol=3)

#facet_grid()
ggplot(data=filmovi, aes(x=OcenaKriticara, y=OcenaPublike, colour=Zanr))
      +geom_point(aes(size=Budzet))+geom_smooth()
      +facet_grid(Zanr~Godina, scales="free")


#-------------------Sloj tema

g<-ggplot(data=filmovi, aes(Budzet))+geom_histogram(binwidth = 10, aes(fill=Zanr), colour="Black")
gg<-g+labs(
  x="Budžet (milioni $)",
  y="Broj filmova",
  fill="Žanr",
  title="Odnos visine budžeta i broja filmova"
    )
gg+theme(axis.title.x=element_text(face="italic", colour="DarkGreen",size=10),
        axis.title.y=element_text(face="italic", colour="Red", size=10),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=7),
        
        legend.title=element_text(face="italic",size=10),
        legend.position=c(1,1),
        legend.justification = c(1,1),
        
        plot.title=element_text(face="bold",colour="DarkBlue",size=20),
        
        text=element_text(family="serif"))

  