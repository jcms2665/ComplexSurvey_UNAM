#Graficas con R
#Por Julio C�sar Mart�nez S�nchez


#0. Consideraciones iniciales

    #0.1  Instalar los paquete necesarios 
          install.packages(c("data.table","foreign","questionr","survey","ggplot2","gridExtra","grid"))

    #0.2  Cargar las librerias que se van a utilizar
          library(data.table)
          library(foreign)
          library(questionr)
          library(survey)
          library(ggplot2)
          library(gridExtra)
          library(grid)

    #0.3  Definir el directorio raiz de las bases de datos (Hay que utilizar \\ � / para definir la ruta
          setwd("C:/Users/JC/Desktop/1")
          SDEMT116<-data.frame(read.dbf("sdemt116.dbf"))

#1. Variables

    #1.1. Convertir las variables a num�ricas
          SDEMT116$R_DEF <-as.numeric(as.character(SDEMT116$R_DEF))
          SDEMT116$C_RES <-as.numeric(as.character(SDEMT116$C_RES))
          SDEMT116$EDA <-as.numeric(as.character(SDEMT116$EDA))
          SDEMT116$CLASE2 <-as.numeric(as.character(SDEMT116$CLASE2))
    
    #1.2. Filtrar a la poblaci�n que es de inter�s
          SD<-SDEMT116[which(SDEMT116$CLASE2==1 & (SDEMT116$EDA>=15 & SDEMT116$EDA<=98) & SDEMT116$R_DEF==0 & (SDEMT116$C_RES==1 | SDEMT116$C_RES==3)),]


    #1.3. Etiquetado de la variable sexo
          SD$SEXO <- factor(SD$SEX,levels = c(1,2),labels = c("Hombre", "Mujer"))
    
    #1.4. Verificar la recodificaci�n con un tabulado simple
          wtd.table(SD$SEXO,weights = SD$FAC)
    
    
    #1.5. Etiquetado de la variable posici�n en la ocupaci�n
          SD$POS_OCU <- factor(SD$POS_OCU,levels = c(1,2,3,4,5),labels = c("Subordinados", "Empleadores","CtaPropia", "SinPago","NE"))
    
    #1.6. Verificar la recodificaci�n con un tabulado simple
          wtd.table(SD$POS_OCU)

#2. Gr�ficas

    #2.1. Con 1 Variable Discreta
    
        #2.1.1. Indicar la base de datos y la variable
                g1<-ggplot(SD,aes(POS_OCU))
        
        #2.1.2. Definir el tipo de gr�fica y definir las caracter�sticas
                g1+geom_bar(fill="deepskyblue2")
        
        #2.1.3. Agregar las capas que se requieran.
                g1+geom_bar(fill="deepskyblue2")+
                  ggtitle("Posici�n en la Ocupaci�n")+
                  xlab("Rango de horas")+
                  ylab("N�mero de personas")


    #2.2. Con 2 Variable Discretas
                
        #2.2.1. Opci�n 1
                g2<-ggplot(SD,aes(POS_OCU))
                g2+geom_bar(fill="yellowgreen")+
                  facet_wrap(~ SEX)+
                  ggtitle("Posici�n en la Ocupaci�n")+
                  xlab("Rango de horas")+
                  ylab("N�mero de personas")

        #2.2.1. Opci�n 2
                g3<-ggplot(SD,aes(x=POS_OCU,fill=SEX))
                g3+geom_bar(position = "dodge")+
                  ggtitle("Posici�n en la Ocupaci�n")+
                  xlab("Rango de horas")+
                  ylab("N�mero de personas")

    #2.3. Con 1 Variable Cont�nua

          g4<-ggplot(SD,aes(EDA))
          g4+geom_area(stat = "bin",binwidth = 5)


#3. Agregar gr�ficas

    grafica1<-
      g1+geom_bar(fill="deepskyblue2")+
      ggtitle("Posici�n en la Ocupaci�n")+
      xlab("Rango de horas")+
      ylab("N�mero de personas")
    
    grafica2<-
      g2+geom_bar(fill="yellowgreen")+
      facet_wrap(~ SEX)+
      ggtitle("Posici�n en la Ocupaci�n")+
      xlab("Rango de horas")+
      ylab("N�mero de personas")
    
    grafica3<-
      g3+geom_bar(position = "dodge")+
      ggtitle("Posici�n en la Ocupaci�n")+
      xlab("Rango de horas")+
      ylab("N�mero de personas")
    
    grafica4<-
      g4+geom_area(stat = "bin",binwidth = 5)+
      ggtitle("Edad")+
      xlab("Rango")+
      ylab("Personas")


    grid.arrange(grafica1, grafica2,grafica3,grafica4,nrow=4)
