library(readr)
datasetCsv <- read_delim("~/Desktop/Facu/AnalisisyEd/R/datasetCsv.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
Data <- datasetCsv
summary(Data)

#Queries a usar:

#1
print("Hombres")
hombres <- subset(Data,Data$SEXO=="MASC")
cantHom <- round(length(hombres$SEXO)/length(Data$SEXO)*100)

#2
print("Mujeres")
mujeres <- subset(Data,Data$SEXO=="FEME")
cantMuj <- round(length(mujeres$SEXO)/length(Data$SEXO)*100)

table(Data$SEXO)

#Grafico
library(plotrix)
library(dplyr)
pie3D(c(cantHom,cantMuj),main = "Percentage of men and women", labels = c(paste(cantHom,"%"),paste(cantMuj,"%")),col = c("slateblue1","pink"),labelcex = 1, explode = 0.1, theta = 1, mar = c(5,5,5,5))
legend(-0,1.1,c("Men","Women"),fill = c("slateblue1","pink"),cex = 0.5)


#Promedio de edad
edadMeda <- mean(Data$EDAD)

#3
hombresMmean <- subset(hombres,hombres$EDAD>edadMeda)
hombresMenmean <- subset(hombres,!hombres$EDAD>edadMeda)
print("Hombres mayores a la media de edad")
length(hombresMmean$EDAD)
print("Hombres menores a la media de edad")
length(hombresMenmean$EDAD)
table(hombres$EDAD)

#4
print("mujeres mayores a la media de edad")
mujeresMmean <- subset(mujeres,mujeres$EDAD>edadMeda)
mujeresMenMean <- subset(mujeres,!mujeres$EDAD>edadMeda)
print("Mujeres mayores a la media de edad")
length(mujeresMmean$EDAD)
print("Mujeres menores a la media de edad")
length(mujeresMenMean$EDAD)
table(mujeres$EDAD)

#Grafico
par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist((hombres$EDAD) , main="" , xlim=c(0,100), ylab="Amount of men", xlab="", ylim=c(0,100) , xaxt="n", las=1 , col="slateblue1", breaks=10)
par(mar=c(5,5,0,3))
hist((mujeres$EDAD ), main="" , xlim=c(0,100), ylab="Amount of women", xlab="Age", ylim=c(100,0) , las=1 , col="pink"  , breaks=10)

#5
print("Motivo de ingreso de los pacientes que terminaron en angioplastia")
operacionesAngio <- subset(Data,Data$PROCEDIMIENTO=="ANGIOPLASTIA")
table(operacionesAngio$`MOTIVO DE INGRESO`)
print("El mas repetido es:")
motMAsRep <- tail(names(sort(table(operacionesAngio$`MOTIVO DE INGRESO`))), 1)
motMAsRep
queryOfMAsRep <- subset(operacionesAngio,operacionesAngio$`MOTIVO DE INGRESO`==motMAsRep)
pacMotIngMAsRep <- subset(Data,Data$`MOTIVO DE INGRESO`==motMAsRep)
print("Pacientes que entraron con el mas repetido pero no terminaron en angio")
pacNoAngio <- subset(pacMotIngMAsRep,pacMotIngMAsRep$PROCEDIMIENTO!="ANGIOPLASTIA")

#Grafico
#Poner cantindad:
library(treemap)
library(plotly)
datos <- ((operacionesAngio$`MOTIVO DE INGRESO`))
values <- sort(table(datos))
values
theCats <- (names(values))
theCats
sort(cats)
sort(table(datos))
p <- plot_ly(
        name = "Reason for admission",
        type = 'treemap',
        labels = theCats,
        values = values 'admisions',
        parents = NA
)
p

#6
meidaBomba <- mean(Data$`TIEMPO DE BOMBA`,na.rm = TRUE)
pacMasMediaBomba <- subset(Data,Data$`TIEMPO DE BOMBA`>meidaBomba)
pacMenosMediaBomba <- subset(Data,!Data$`TIEMPO DE BOMBA`>meidaBomba)
print("Pacientes que estuvieron con mas de la media del tiempo de bomba")
length(pacMasMediaBomba$EDAD)
print("Pacientes que estuvieron con menos de la media del tiempo de bomba")
length(pacMenosMediaBomba$EDAD)

print("Lo relacionamos con la edad")

library(plotly)
datawithBomba <- subset(Data,!is.na(Data$`TIEMPO DE BOMBA`))
dataSortedByAGe <- Data[order(Data$EDAD)]

fig <- plot_ly(data = Data ,y = ~`TIEMPO DE BOMBA`, x = ~`EDAD`, type = "scatter",
        marker = list(size = 5,
                      color = 'rgba(255, 182, 193, .9)',
                      line = list(color = 'rgba(255, 5, 255, .8)',
                                  width = 2)))

fig
#Relacionar con complicaciones

#7
pacientesAngiop <- subset(Data,Data$PROCEDIMIENTO=="ANGIOPLASTIA")
Data$`NUMERO DE VASOS EXITOSOS`[is.na(Data$`NUMERO DE VASOS EXITOSOS`)]<-0
vasosAngiopNoExit <- subset(pacientesAngiop,pacientesAngiop$`NUMERO VASOS ANGIOP`!=pacientesAngiop$`NUMERO DE VASOS EXITOSOS`)
vasosAngiopExit <- subset(pacientesAngiop,pacientesAngiop$`NUMERO VASOS ANGIOP`==pacientesAngiop$`NUMERO DE VASOS EXITOSOS`)
print("Vasos angioplastados que no fueron exitosos")
nrow(vasosAngiopNoExit)
print("Vasos angioplastados que fueron exitosos")
nrow(vasosAngiopExit)
#personas que tuvieron una angioplastia exitosa y tenia angioplastiado el CD 
vasosCd <- subset(vasosAngiopExit,grepl("CD",vasosAngiopExit$`VASOS ANGIOPLASTIADOS`))

#Grafico
library(tidyverse)
library(viridis)
number <- length(names(table(Data$`VASOS ANGIOPLASTIADOS`)))
number
data <- data.frame(
  id=seq(1,number),
  individual=paste(names(table(Data$`VASOS ANGIOPLASTIADOS`))),
  value=as.vector(table(Data$`VASOS ANGIOPLASTIADOS`))
)
data
# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- data

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


# Start the plot
p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p

# PASAR A PORCENTAJES
number <- length(names(table(vasosAngiopNoExit$`VASOS ANGIOPLASTIADOS`)))
data <- data.frame(
  id=seq(1,number),
  individual=paste(names(table(vasosAngiopNoExit$`VASOS ANGIOPLASTIADOS`))),
  value=as.vector(table(vasosAngiopNoExit$`VASOS ANGIOPLASTIADOS`))
)
data
# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- data

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


# Start the plot
p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p

#8
pacComplicac <- subset(Data,Data$`COMPLICACIONES INMEDIATAS`>= 1 | Data$`COMPLICACIONES TARDIAS`>= 1)
pacSinComplicac <- subset(Data,!Data$`COMPLICACIONES INMEDIATAS`>= 1 | Data$`COMPLICACIONES TARDIAS`>= 1)
print("Pacientes con complicaciones inmediatas o tardias")
nrow(pacComplicac)
print("Pacientes sin complicaciones inmediatas o tardias")
nrow(pacSinComplicac)
print("En que edad surgieron mas")
table(pacComplicac$EDAD)
tail(names(sort(table(pacComplicac$EDAD))), 1)

plot_ly(y = pacComplicac$ï.., x = pacComplicac$EDAD, type = "scatter")



#9
print("Resumen coronopatia != numero de lesiones")
table(Data$`Resumen Coronariopatia`)
dataWithVasosAsInts <- Data
dataWithVasosAsInts$`Resumen Coronariopatia` <- factor(Data$`Resumen Coronariopatia`,labels=c(1,2,3,"TCI SOLO"))
resCorNotNroLes <- subset(dataWithVasosAsInts,dataWithVasosAsInts$`Resumen Coronariopatia`!=dataWithVasosAsInts$`NUMERO DE LESIONES`)

#Grafico
library(plotly)
vasos <- unique(subset(Data$`Resumen Coronariopatia`,Data$`Resumen Coronariopatia`!="NA"))
vasos <- unique(subset(Data$`Resumen Coronariopatia`,Data$`Resumen Coronariopatia`!="TCI solo"))

vas1 <- (subset(Data ,Data$`Resumen Coronariopatia`=="1 vaso"))
vas2 <- (subset(Data,Data$`Resumen Coronariopatia`=="2 vasos"))
vas3 <- (subset(Data,Data$`Resumen Coronariopatia`=="3 vasos"))

vasosEX <- c(nrow(vas1),nrow(vas2),nrow(vas3))

vas1NoEx <- subset(vas1,vas1$`NUMERO DE LESIONES`!=1)
vas2NoEx <- subset(vas2,vas2$`NUMERO DE LESIONES`!=2)
vas3NoEx <- subset(vas3,vas3$`NUMERO DE LESIONES`!=3)

vasosNoEX <- c(nrow(vas1NoEx),nrow(vas2NoEx),nrow(vas3NoEx))

datosAGraf <- data.frame(vasos, vasosEX, vasosNoEX)

fig <- plot_ly(datosAGraf, x = ~vasos, y = ~vasosNoEX, type = 'bar', name = 'Vasos no exitosos')
fig <- fig %>% add_trace(y = ~(vasosEX), name = 'Vasos exitosos')
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
fig
