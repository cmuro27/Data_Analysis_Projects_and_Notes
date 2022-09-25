library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(ggrepel)

my_data <- read_excel("C:/Users/cmuro/Downloads/disauto.xls",skip=1)
disauto <- my_data[order(my_data$Edad),]
disauto <- disauto %>%
mutate( Categoria = case_when(
Edad<=12 ~ "Niñez",
Edad>12 & Edad<18 ~ "Adolescencia",
Edad>=18 & Edad<=40 ~ "Adulto Joven" ,
Edad>40 & Edad<=60 ~ "Adulto Maduro",
Edad>60  ~ "Vejez"
))
disauto$Sexo<-as.factor(disauto$Sexo)
disauto$Sexo<-factor(disauto$Sexo,level=c(0,1),labels=c("Mujer","Hombre"))
cardiacos <- select(disauto,Taquicardia,Bradicardia,Palpitaciones,'Presión sanguínea baja',Edad,Sexo,Categoria)
fisicos <- select(disauto,'Falta de fuerza','Falta de aliento','Sudoracion fria','Subida de temperatura en forma repentina','Disminucion del apetito','Escalofrio diurno','Dolor Muscular',Edad,Sexo,Categoria)
neurologicos <- select(disauto,'Dolor de Cabeza','Sueño recurrente','Mareos','Desmayos','Insomnio','Ansiedad','Migraña','Espasmos nocturnos','Desasociacion de ideas','Perdida de la Memoria a corto plazo','Niebla Cerebral','Transtornos del sueño a largo plazo (Pesadillas)',Edad,Sexo,Categoria)
gastrointestinales <- select(disauto,Vómito,Mareos,Diarrea,Edad,Sexo,Categoria)


gastrointestinalesN <- subset(gastrointestinales,Categoria=='Niñez')
gastrointestinalesT <- subset(gastrointestinales,Categoria=='Adolescencia')
gastrointestinalesAj <- subset(gastrointestinales,Categoria=='Adulto Joven')
gastrointestinalesAm <- subset(gastrointestinales,Categoria=='Adulto Maduro')
gastrointestinalesV <- subset(gastrointestinales,Categoria=='Vejez')


neurologicosN <- subset(neurologicos,Categoria=='Niñez')
neurologicosT <- subset(neurologicos,Categoria=='Adolescencia')
neurologicosAj <- subset(neurologicos,Categoria=='Adulto Joven')
neurologicosAm <- subset(neurologicos,Categoria=='Adulto Maduro')
neurologicosV <- subset(neurologicos,Categoria=='Vejez')


#ANALIZAMOS GASTROINTESTINALES. 
#Primero lo hacemos en NI?EZ.

View(gastrointestinalesN)
#Histograma de ni?os con V?mito, Mareos, y Diarrea sacamos porcentajes

ggplot(data=gastrointestinalesN)+
  geom_histogram(aes(x=Vómito),
                 bins = 3)+
  labs(x='0=No, 1.0=Si.',y='Frecuencias',title = 'Vómito en la niñez')

ggplot(data=gastrointestinalesN)+
  geom_histogram(aes(x=Mareos),
                 bins = 3)+
  labs(x='0=No, 1.0=Si.',y='Frecuencias',title = 'Mareos en la niñez')

ggplot(data=gastrointestinalesN)+
  geom_histogram(aes(x=Diarrea),
                 bins = 3)+
  labs(x='0=No, 1.0=Si.',y='Frecuencias',title = 'Diarrea en la niñez')



#Pie charts de V?mitos, Mareos, y Diarrea seg?n sexo. Adem?s de porcentajes que presentan entre hombres y mujeres
x=sum(subset(gastrointestinalesN,Sexo=='Mujer')$V?mito)/length(subset(gastrointestinalesN,Sexo=='Mujer')$Vómito)
y=sum(subset(gastrointestinalesN,Sexo=='Hombre')$V?mito)/length(subset(gastrointestinalesN,Sexo=='Hombre')$Vómito)

ggplot(data=gastrointestinalesN, aes(x="", y=Vómito, fill=Sexo)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()+  # remove background, grid, numeric labels
  geom_text(label='The percentaje of')



x=sum(subset(gastrointestinalesN,Sexo=='Mujer')$Mareos)/length(subset(gastrointestinalesN,Sexo=='Mujer')$Mareos)
y=sum(subset(gastrointestinalesN,Sexo=='Hombre')$Mareos)/length(subset(gastrointestinalesN,Sexo=='Hombre')$Mareos)

ggplot(data=gastrointestinalesN, aes(x="", y=Mareos, fill=Sexo)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels



x=sum(subset(gastrointestinalesN,Sexo=='Mujer')$Diarrea)/length(subset(gastrointestinalesN,Sexo=='Mujer')$Diarrea)
y=sum(subset(gastrointestinalesN,Sexo=='Hombre')$Diarrea)/length(subset(gastrointestinalesN,Sexo=='Hombre')$Diarrea)

ggplot(data=gastrointestinalesN, aes(x="", y=Diarrea, fill=Sexo)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels



#AHORA EN ADULTO JOVEN

View(gastrointestinalesT)
#Histograma de adolescentes con V?mito, Mareos, y Diarrea sacamos porcentajes

ggplot(data=gastrointestinalesT)+
  geom_histogram(aes(x=Vómito),
                 bins = 3)+
  labs(x='0=No, 1.0=Si.',y='Frecuencias',title = 'Vómito en la niñez')

ggplot(data=gastrointestinalesT)+
  geom_histogram(aes(x=Mareos),
                 bins = 3)+
  labs(x='0=No, 1.0=Si.',y='Frecuencias',title = 'Mareos en la niñez')

ggplot(data=gastrointestinalesT)+
  geom_histogram(aes(x=Diarrea),
                 bins = 3)+
  labs(x='0=No, 1.0=Si.',y='Frecuencias',title = 'Diarrea en la niñez')



#Pie charts de Vómitos, Mareos, y Diarrea seg?n sexo. Adem?s de porcentajes que presentan entre hombres y mujeres
x=sum(subset(gastrointestinalesT,Sexo=='Mujer')$Vómito)/length(subset(gastrointestinalesT,Sexo=='Mujer')$Vómito)
y=sum(subset(gastrointestinalesT,Sexo=='Hombre')$Vómito)/length(subset(gastrointestinalesT,Sexo=='Hombre')$Vómito)



ggplot(data=gastrointestinalesT, aes(x="", y=Vómito, fill=Sexo)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()+  # remove background, grid, numeric labels
  geom_text(label='The percentaje of')



x=sum(subset(gastrointestinalesN,Sexo=='Mujer')$Mareos)/length(subset(gastrointestinalesN,Sexo=='Mujer')$Mareos)
y=sum(subset(gastrointestinalesN,Sexo=='Hombre')$Mareos)/length(subset(gastrointestinalesN,Sexo=='Hombre')$Mareos)

ggplot(data=gastrointestinalesT, aes(x="", y=Mareos, fill=Sexo)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels



x=sum(subset(gastrointestinalesT,Sexo=='Mujer')$Diarrea)/length(subset(gastrointestinalesN,Sexo=='Mujer')$Diarrea)
y=sum(subset(gastrointestinalesT,Sexo=='Hombre')$Diarrea)/length(subset(gastrointestinalesN,Sexo=='Hombre')$Diarrea)

ggplot(data=gastrointestinalesT, aes(x="", y=Diarrea, fill=Sexo)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels

















