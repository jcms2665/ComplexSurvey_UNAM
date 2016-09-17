#Muestras complejas con R
#Por Julio César Martínez Sánchez
  
#0. Consideraciones iniciales

    #0.1  Instalar los paquete necesarios 
          install.packages(c("foreign","data.table","questionr","survey"))

    #0.2  Cargar las librerias que se van a utilizar
          library(data.table)
          library(foreign)
          library(questionr)
          library(survey)

    #0.3  Definir el directorio raiz de las bases de datos (Hay que utilizar \\ ó / para definir la ruta
          setwd("C:/Users/JC/Desktop/1")
          SDEMT116<-data.frame(read.dbf("sdemt116.dbf"))


#1. Variables
          
    #1.1. Convertir las variables a numéricas
          SDEMT116$R_DEF <-as.numeric(as.character(SDEMT116$R_DEF))
          SDEMT116$C_RES <-as.numeric(as.character(SDEMT116$C_RES))
          SDEMT116$EDA <-as.numeric(as.character(SDEMT116$EDA))
          SDEMT116$SEX <-as.numeric(as.character(SDEMT116$SEX))
          SDEMT116$CLASE2 <-as.numeric(as.character(SDEMT116$CLASE2))
          SDEMT116$HRSOCUP <-as.numeric(as.character(SDEMT116$HRSOCUP))

    #1.2. Convertir las variables a numéricas
          SD<-SDEMT116[which(SDEMT116$CLASE2 == 1 & 
                               SDEMT116$EDA>=15 & SDEMT116$EDA<=98 &
                               SDEMT116$R_DEF==0 &
                               (SDEMT116$C_RES==1 | SDEMT116$C_RES==3)),]
        
#2. Linealización por Series de Taylor

    #2.1. Promedio

          #2.1.1. Escenario 1: Muestreo aleatorio simple
                  ds_enoe1<-svydesign(id=~1,weight=~FAC,data=SD)
                  svy1<-svymean(~HRSOCUP, ds_enoe1, deff=TRUE)
                  cv1<-cv(svy1)*100
                  cv1

          #2.1.2. Escenario 2: Muestreo por conglomerados
                  ds_enoe2<-svydesign(id=~UPM,weight=~FAC,data=SD)
                  svy2<-svymean(~HRSOCUP, ds_enoe2, deff=TRUE)
                  cv2<-cv(svy2)*100
                  cv2

          #2.1.3. Escenario 3: Muestreo estratificado
                  ds_enoe3<-svydesign(id=~1, strata=~EST_D, weight=~FAC, data=SD, nest=TRUE)
                  svy3<-svymean(~HRSOCUP, ds_enoe3, deff=TRUE)
                  cv3<-cv(svy3)*100
                  cv3

          #2.1.4. Escenario 4: Muestreo complejo
                  ds_enoe4<-svydesign(id=~UPM, strata=~EST_D, weight=~FAC, data=SD, nest=TRUE)
                  svy4<-svymean(~HRSOCUP, ds_enoe4, deff=TRUE)
                  cv4<-cv(svy4)*100
                  cv4
          #2.1.5. Comparación de los 4 escenarios
                  x1<-c(cv1,cv2,cv3, cv4);x2<-c("MAS","Congl","Estr","Complex");res <- rbind(x2,x1)
                  res

    #2.2. Totales
          SD2<-SD[which(SD$SEX == 2),]
          ds_enoe5<-svydesign(id=~UPM, strata=~EST_D, weight=~FAC, data=SD2, nest=TRUE)
          options(survey.lonely.psu="adjust")
          svy5<-svytotal(~factor(ING7C), ds_enoe5, deff=TRUE)
          cv5<-cv(svy5)*100
          cv5
          
    #2.3. Tasas
          
          SD$REMUNE2C<-as.numeric(as.character(SD$REMUNE2C))
          SD$Tasa_trabajo_asalariado<-0
          SD$Tasa_trabajo_asalariado[SD$REMUNE2C==1]<-1
          ds_enoe6<-svydesign(id=~UPM, strata=~EST_D, weight=~FAC, data=SD, nest=TRUE)
          svy6<-svymean(~Tasa_trabajo_asalariado, ds_enoe6, deff=TRUE)
          cv6<-cv(svy6)*100
          cv6
          
#3. ¿Qué tiene R de especial?
          
          unam<-
            function(u, d = parent.frame()){
              ds_enoe5<-svydesign(id=~UPM, strata=~EST_D, weight=~FAC, data=d, nest=TRUE)
              options(survey.lonely.psu="adjust")
              svy5<-svytotal(~factor(d[,u]), ds_enoe5, deff=TRUE)
              cv5<-cv(svy5)*100
              return(cv5)
              }
          
          unam("POS_OCU",SDEMT116)