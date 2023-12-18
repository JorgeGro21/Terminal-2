library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(shiny)
library(bslib)
library(shinythemes)
quitar_acentos <- function(texto) {
  texto <- iconv(texto, to = "ASCII//TRANSLIT")
  return(texto)
}
ui <- navbarPage(
  
  theme = shinytheme("cerulean"),
  # Titulo
  "Proyecto",
  
  #tabpanel crea las pestallas
  tabPanel("Variables",
           fluidPage(
             # Contenido específico para la pestaña "Variables"
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Subir Base de Datos (CSV)", accept = ".csv")
               ),
               mainPanel(
                 titlePanel("Variables"),
                 verbatimTextOutput("men1"),
                 verbatimTextOutput("variables")
               )
             )
           )
  ),
  
  tabPanel("Búsqueda",
           fluidPage(
             titlePanel("Búsqueda de datos"),
             textInput("texto", "¿Qué desea buscar en la base de datos?"),
             actionButton("aplicar","Buscar"),
             verbatimTextOutput("filtro1"),
             #verbatimTextOutput("filtro5"),
             
             DT::dataTableOutput("resultado")
           )
  ),
  
  #navbarMenu crea subsecciones en la barra de pestañas
  navbarMenu("Gráficos",
           tabPanel("Gráfica de Barras",
                    fluidPage(
                      
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("var","Variable que será el eje X",
                                      choices = c("hotel","is_canceled",
                                                  "arrival_date_year",
                                                  "arrival_date_month",
                                                  "meal","country","market_segment",
                                                  "distribution_channel",
                                                  "reserved_room_type",
                                                  "assigned_room_type",
                                                  "deposit_type","customer_type",
                                                  "reservation_status"))
                        ),
                        mainPanel(
                          titlePanel("Gráfica de Barras"),
                          plotOutput("barras")
                        )
                      )
                    )
           ),
           
           tabPanel("Histograma",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          textInput("ejeX2", "Variable a graficar"),
                          actionButton("crear2","Crear")
                        ),
                        mainPanel(
                          titlePanel("Histograma"),
                          plotOutput("histograma")
                        )
                      )
                    )
           )
  ),
  navbarMenu("Ayuda",
    tabPanel("¿Cómo buscar?",
             fluidPage(
               actionButton("f1","Ver primera forma de buscar"),
               verbatimTextOutput("forma1"),
               actionButton("f2","Ver segunda forma de buscar"),
               verbatimTextOutput("forma2"),
               verbatimTextOutput("men2")
             )
             
    ),
    tabPanel("Descripción sobre la Base de datos",
             fluidPage(
               titlePanel("Base de Datos"),
               DTOutput("descripcion")
             )
    )
    
  )
)


server <- function(input, output) {
  
  
  options(shiny.maxRequestSize=90*1024^2)
  
  #cargamos la base de datos
  datos <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  
  #aqui llega el texto escrito y lo depura
  palabras <- eventReactive(input$aplicar,{
   
    #se eliminan los signos que puedan molestar y se convierte a minusculas   
    palabras_clave <- quitar_acentos(input$texto)
    palabras_clave <- gsub("[?]","",palabras_clave)
    palabras_clave <- gsub("[¿]","",palabras_clave)
    palabras_clave <- gsub("[¡]","",palabras_clave)
    palabras_clave <- gsub("[!]","",palabras_clave)
    palabras_clave <- gsub(",","",palabras_clave)
    
    #el vector con todo el texto se separa las palabras ylescrea una entrada paracada una
    palabras_clave <- unlist(strsplit(palabras_clave, "\\s+"))
    palabras_clave <- tolower(palabras_clave)
    
    
    return(palabras_clave)
  })
  
  #Aqui se separan los números de la cadena de texto
  numeros <-eventReactive(input$aplicar,{
    filtroP <- input$texto
    numeros_clave <- numeros_clave <- as.numeric(unlist(regmatches(filtroP,gregexpr("\\b\\d+\\b", filtroP))))
    return(numeros_clave)
  })

  #Crea una tabla de las variables de la base de datos y les agrega una descripcion
  output$descripcion <- renderDT({
    req(input$file)
    datatable(
      data.frame(
        Columna = colnames(datos()),
        Descripcion = c("Index", "Tipo de hotel (Resort, City)",
                        "Indica si se canceló la reservación (Sí, No)",
                        "Número de días entre reservación y día de llegada",
                        "Año de llegada","Mes de llegada","Número de la semana de llegada"
                        ,"Día de llegada","Número de fines de semana (sábado o domingo) que se registraron reservaciones",
                        "Número de noches entre semana (lunes a viernes) que se reservaron",
                        "Número de adultos","Número de niños","Número de bebés","Tipo de desayuno (Desayuno, Media Comida, Comida Completa)",
                        "País de origen (siglas del país, por ejemplo ESP, FRA)",
                        "Reservación (Onlin, Offline TA/TO, Direct)",
                        "Canal por el cual se completo la reservación (Direct, TA/TO, Corporate)",
                        "La persona ha reservado en más ocasiones (Sí, No)",
                        "Número de veces que han cancelado su reservación",
                        "Número de veces que no han cancelado su reservación",
                        "Tipo de habitación reservada inicialmente (De la A a H)",
                        "Tipo de habitación que se asignó (De la A a H)",
                        "Número de veces que cambiaron de reservación",
                        "Tipo de déposito (No Deposit, Non Refund)",
                        "ID de la agencia de viaje que hizo la reservación (1 al 535)",
                        "ID de la compañia que hizo la reservación (6 a 543)",
                        "Número de días de espera antes de confirmarse la recervación",
                        "El tipo de cliente (por ejemplo, Transient, Contract, Group, Transiet-Party)",
                        "La tarifa diaria promedio (precio por habitación) para la reserva",
                        "Número de espacios de estacionamiento solicitados",
                        "Número de requerimientos especiales solicitados",
                        "Estado de la reservación (Canceled, Check-Out, Check-In, No-Show",
                        "Día en que se actualizo el estado de la reservación (DD-MM-AA)")
      ),
      editable = TRUE,
      options = list(
        pageLength = 33,
        lengthMenu = c(5, 10, 15),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  })
  
  #Nos muestra las variables de la base de datos
  output$variables<- renderPrint({
    req(input$file)
    glimpse(datos())
  })
  
  #hace la grafica de barras
  output$barras <- renderPlot({
    req(input$var)
    filtroP <- input$grafica
    
    plot <- ggplot() + theme_void()
    
    X1 <- input$var
    plot <- ggplot(datos(),aes(x=datos()[,X1],fill=datos()[,X1]))+
      geom_bar(width = 0.9, stat="count",
               position = position_dodge()                
      )
    plot
  })
  
  #hace el histograma
  output$histograma <- renderPlot({
    req(input$crear2)
    filtroP <- input$grafica
    
    plot <- ggplot() + theme_void()
    
    X1 <- tolower(input$ejeX2)
    
    plot <- ggplot(datos(),aes(x=datos()[,X1]))+
      geom_histogram(binwidth = 0.1, col='black', fill='red')
    plot
  })
  
  #se filtran los datos segun ña variable
  datos_filtrados <- eventReactive(input$aplicar,{
    
    BD<-datos()
    filtroP<-palabras()
    filtroN<-numeros()
    
    #por hotel
    if("hotel"%in%filtroP){
      if("mas"%in%filtroP||"city"%in%filtroP){
        BD<-BD[BD$hotel == "City Hotel",]
        
      }
      if("menos"%in%filtroP||"resort"%in%filtroP){
        BD<-BD[BD$hotel == "Resort Hotel",]
        
      }
      
    }
    
    #por reservacion
    if(("reservacion"%in%filtroP||"reservaciones"%in%filtroP)&&("cancelaron"%in%filtroP||"cancelada"%in%filtroP||"canceladas"%in%filtroP)){
      if("no"%in%filtroP){
        BD<-BD[BD$is_canceled==0,]
      }
      else{
        BD<-BD[BD$is_canceled==1,]
      }
    }
    
    #por año
    if("2015"%in%filtroP||"2016"%in%filtroP||"2017"%in%filtroP){
      if("2015"%in%filtroP){
        BD<-BD[BD$arrival_date_year==2015,]
      }
      else if("2016"%in%filtroP){
        BD<-BD[BD$arrival_date_year==2016,]
      }
      else if("2017"%in%filtroP){
        BD<-BD[BD$arrival_date_year==2017,]
      }
      else if("2015"%in%filtroP&&"2016"%in%filtroP){
        BD<-BD[BD$arrival_date_year%in% c("2015","2016"),]
      }
      else if("2015"%in%filtroP&&"2017"%in%filtroP){
        BD<-BD[BD$arrival_date_year%in% c("2015","2017"),]
      }
      else if("2017"%in%filtroP&&"2016"%in%filtroP){
        BD<-BD[BD$arrival_date_year%in% c("2016","2017"),]
      }
    }
    
    #por dia
    if("dia"%in%filtroP||"dias"%in%filtroP){
      
      if("los"%in%filtroP ||"el"%in%filtroP){
        for (i in 1:length(filtroN)) {
          if(filtroN[i]>=1&&filtroN[i]<=31){
            dia[i]<-filtroN[i]
            
          }
        }
        BD<-BD[BD$arrival_date_day_of_month %in% dia,]
      }
      if("entre"%in%filtroP){
        for (i in 1:length(filtroN)) {
          if(filtroN[i]>=1&&filtroN[i]<=31){
            dia[i]<-filtroN[i]
            
          }
        }
        
        if(dia[1]>dia[2]){
          b<-dia[2]
          dia[2]<-dia[1]
          dia[1]<-b
        }
        a=dia[2]-dia[1]
        
        dia2<-c()
        for(i in 1:(a+1)){
          dia2[i]<-dia[1]+i-1
        }
        BD<-BD[BD$arrival_date_day_of_month %in% dia2,]
      }
    }
    
    #por mes meses
    if("enero"%in%filtroP||"febrero"%in%filtroP||"marzo"%in%filtroP||"abril"%in%filtroP||"mayo"%in%filtroP||"junio"%in%filtroP||"julio"%in%filtroP||"agosto"%in%filtroP||"septiembre"%in%filtroP||"octubre"%in%filtroP||"noviembre"%in%filtroP||"diciembre"%in%filtroP){
      meses<-c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")
      meses2<- list(enero="January",febrero="February",marzo="March",abril="April",
                    mayo= "May",junio= "June",julio= "July",agosto="August", 
                    septiembre="September", octubre="October", noviembre="November", diciembre="December")
      meses22<- list("1"="January","2"="February","3"="March","4"="April",
                    "5"= "May","6"= "June","7"= "July","8"="August", 
                    "9"="September", "10"="October", "11"="November", "12"="December")
      meses3<-intersect(filtroP,meses)
      BD<-BD[BD$arrival_date_month %in% meses2[meses3],]

      if("en"%in%filtroP){
        meses3<-intersect(filtroP,meses)
        BD<-BD[BD$arrival_date_month %in% meses2[meses3],]
      }
      if("entre"%in%filtroP){
        meses3<-intersect(filtroP,meses)
        for(i in 1:12){
          if(length(intersect(meses[i],meses3))>0){
            a[i]=i
          }
          else{
            a[i]=0
          }
        }
        pos <- which(a > 0)
        if(pos[1]>pos[2]){
          a<-pos[1]
          pos[1]<-pos[2]
          pos[2]<-a
        }
        while(pos[1]<pos[2]){
          a[pos[1]]<-pos[1]
          pos[1]<-pos[1]+1
        }
        #a<-as.character(a,na.rm=TRUE)
        BD<-BD[BD$arrival_date_month %in% meses22[a],]
      }
    }
   
    #por pais####
    if("mexico" %in% filtroP||"china" %in% filtroP||
       "india" %in% filtroP||"brasil" %in% filtroP||"rusia" %in% filtroP||
       ("reino" %in% filtroP&&"unido"%in%filtroP)||"francia" %in% filtroP||
       "alemania" %in% filtroP||"japon" %in% filtroP||"canada" %in% filtroP||
       "australia" %in% filtroP||("estados" %in% filtroP&&"unidos"%in%filtroP)||
       "italia" %in% filtroP||"españa" %in% filtroP||("corea" %in% filtroP&&"sur"%in%filtroP)||
       "portugal" %in% filtroP||"argentina" %in% filtroP||"colombia" %in% filtroP||"peru" %in% filtroP||
       "chile" %in% filtroP||
       "afganistan" %in% filtroP|| 
       "albania" %in% filtroP|| 
       "andorra" %in% filtroP|| 
       "angola" %in% filtroP||
       "antartida" %in% filtroP|| 
       "arabia" %in% filtroP||
       "austria" %in% filtroP|| 
       "bahamas" %in% filtroP||
       "bangladesh" %in% filtroP|| 
       "barbados" %in% filtroP|| 
       "belgica" %in% filtroP|| 
       "belice" %in% filtroP||
       "bermudas" %in% filtroP|| 
       "bolivia" %in% filtroP||
       "bulgaria" %in% filtroP|| 
       "camboya" %in% filtroP|| 
       "qatar" %in% filtroP|| 
       "catar" %in% filtroP|| 
       "vaticano" %in% filtroP|| 
       "costa" %in% filtroP||
       "croacia" %in% filtroP|| 
       "cuba" %in% filtroP||
       "dinamarca" %in% filtroP|| 
       "ecuador" %in% filtroP|| 
       "egipto" %in% filtroP||
       "salvador" %in% filtroP|| 
       "estonia" %in% filtroP||
       "filipinas" %in% filtroP|| 
       "finlandia" %in% filtroP|| 
       "ghana" %in% filtroP|| 
       "grecia" %in% filtroP||
       "groenlandia" %in% filtroP|| 
       "guatemala" %in% filtroP|| 
       "guinea" %in% filtroP|| 
       "haiti" %in% filtroP||
       "honduras" %in% filtroP|| 
       "hungria" %in% filtroP||
       "indonesia" %in% filtroP|| 
       "iran" %in% filtroP|| 
       "irak" %in% filtroP||
       "navidad" %in% filtroP|| 
       "islandia" %in% filtroP|| 
       "israel" %in% filtroP||
       "jamaica" %in% filtroP|| 
       "kenia" %in% filtroP||
       "letonia" %in% filtroP|| 
       "libano" %in% filtroP||
       "luxemburgo" %in% filtroP|| 
       "madagascar" %in% filtroP|| 
       "marruecos" %in% filtroP|| 
       "nepal" %in% filtroP||
       "nicaragua" %in% filtroP|| 
       "nigeria" %in% filtroP||
       "noruega" %in% filtroP ||
       "pakistan" %in% filtroP|| 
       "panama" %in% filtroP||
       "paraguay" %in% filtroP| 
       "polonia" %in% filtroP|| 
       "senegal" %in% filtroP||
       "sudafrica" %in% filtroP|| 
       "suiza" %in% filtroP|| 
       "tailandia" %in% filtroP){
      paises<-c("estados", "china", "india", "brasil", "rusia",
                 "reino", "francia", "alemania", "japon", "canada",
                 "australia", "mexico", "italia", "españa", "corea",
                 "argentina", "colombia", "peru", "chile","portugal", 
                "afganistan", "albania", "andorra", "angola", "antartida",  
                "arabia", "austria",  "bahamas", "bangladesh","barbados", "belgica", "belice",
                "bermudas", "bolivia", 
                "bulgaria", "camboya", "qatar", "catar", "vaticano", 
                "costa", "croacia", "cuba", "dinamarca", "ecuador", 
                "egipto", "salvador", "estonia", "filipinas", "finlandia", 
                "ghana", "grecia", "groenlandia", "guatemala", "guinea", 
                "haiti", "honduras", "hungria", "indonesia", "iran", 
                "irak", "navidad", "islandia", "israel", "jamaica", 
                "kenia", "letonia", "libano", "luxemburgo", "madagascar", 
                "marruecos", "nepal", "nicaragua", "nigeria", "noruega", 
                "pakistan", "panama", "paraguay", "polonia", "senegal", 
                "sudafrica", "suiza", "tailandia")
      
      paises2<-list(estados="USA", china="CHN", india="IND", brasil="BRA", rusia="RUS",
                    reino="GBR", francia="FRA", alemania="DEU", japon="JPN", canada="CAN",
                    australia="AUS", mexico="MEX", italia="ITA", españa="ESP", corea="KOR",
                    argentina="ARG", colombia="COL", peru="PER", chile="CHL",portugal="PRT", 
                    afganistan="AFG", albania="ALB", andorra="AND", angola="AGO", antartida="ATA",  
                    arabia="SAU", austria="AUT",  bahamas="BHS", bangladesh="BGD", 
                    barbados="BRB", belgica="BEL", belice="BLZ", bermudas="BMU", bolivia="BOL", 
                    bulgaria="BGR", camboya="KHM", qatar="QAT", catar="QAC", vaticano="VAT", 
                    costa="CRI", croacia="HRV", cuba="CUB", dinamarca="DNK", ecuador="ECU", 
                    egipto="EGY", salvador="SLV", estonia="EST", filipinas="PHL", finlandia="FIN", 
                    ghana="GHA", grecia="GRC", groenlandia="GRL", guatemala="GTM", guinea="GIN", 
                    haiti="HTI", honduras="HND", hungria="HUN", indonesia="IDN", iran="IRN", 
                    irak="IRQ", navidad="CXR", islandia="ISL", israel="ISR", jamaica="JAM", 
                    kenia="KEN", letonia="LVA", libano="LBN", luxemburgo="LUX", madagascar="MDG", 
                    marruecos="MAR", nepal="NPL", nicaragua="NIC", nigeria="NGA", noruega="NOR", 
                    pakistan="PAK", panama="PAN", paraguay="PRY", polonia="POL", senegal="SEN", 
                    sudafrica="ZAF", suiza="CHE", tailandia="THA")
      
      paisesF<-intersect(filtroP,paises)
      BD<-BD[BD$country %in% paises2[paisesF],]
      
      
    }#####
    
    #por semana
    if("semana"%in%filtroP&&length(filtroN)>0){
      semana<-c()
      for (i in 1:length(filtroN)) {
        if(filtroN[i]>=1&&filtroN[i]<=53){
          semana[i]<-filtroN[i]
          
        }
      }
      if(semana>=1&&semana<=53){
        BD<-BD[BD$arrival_date_week_number %in% semana,]
      }
    }
    
    #por el tipo de comida
    if("comida"%in%filtroP||"desayuno"%in%filtroP||"media"%in%filtroP||"completa"%in%filtroP){
      comidas<-c("desayuno","media","completa")
      comidas2<-list(desayuno="BB",media="HB",completa="FB")
      f<-intersect(comidas,filtroP)
      BD<-BD[BD$meal %in% comidas2[f],]
    }
    
    #por el tipo de habitacion recibida
    if("habitacion"%in%filtroP&&("dio"%in%filtroP||"asigno"%in%filtroP)){
      hab<-c("a", "b", "c", "d", "e" ,"f", "g", "h", "l" , "p")
      hab2<-list(a="A", b="B", c="C", d="D", e="E" ,f="F", g="G", h="H", l="L" , p="P")
      f<-intersect(hab,filtroP)
      BD<-BD[BD$assigned_room_type %in%hab2[f],]
    }
    
    #por habioatcion pedida
    if("habitacion"%in%filtroP&&("reservaron"%in%filtroP||"pidieron"%in%filtroP)){
      hab<-c("a", "b", "c", "d", "e" ,"f", "g", "h", "l" , "p")
      hab2<-list(a="A", b="B", c="C", d="D", e="E" ,f="F", g="G", h="H", l="L" , p="P")
      f<-intersect(hab,filtroP)
      BD<-BD[BD$reserved_room_type %in% hab2[f],]
    }
    
   
    
    BD
  })
  
  #muestra las tablas
  output$resultado <- DT::renderDataTable({
    req(input$aplicar)
    datos_filtrados()
  })
 
  #contesta posibles preguntas
  output$filtro1 <- renderPrint({
    req(input$aplicar)
    filtroP<-palabras()
   
    BD<-datos_filtrados()
    BD2<-datos()
    text1<-c()
    
    if(("cuantas"%in%filtroP||"cuantos"%in%filtroP)){
      
      #filtro de personas por hotel####
      if("personas"%in%filtroP&&"hotel"%in%filtroP){
        f<-sum(BD$adults)+sum(BD$children,na.rm=TRUE)+sum(BD$babies,na.rm=TRUE)
        text1<-c(text1,paste("El número de personas registradas en el hotel fue de: ",toString(f)))
        
        
      }
      if(("bebes"%in%filtroP)&&"hotel"%in%filtroP){
        f<-sum(BD$babies,na.rm=TRUE)
        text1<-c(text1,paste("El número de bebés registrados en el hotel fue de: ",toString(f)))
        
      }
      if("adultos"%in%filtroP&&"hotel"%in%filtroP){
        f<-sum(BD$adults)
        text1<-c(text1,paste("El número de adultos registrados en el hotel fue de: ",toString(f)))
        
        
      }
      if("niños"%in%filtroP&&"hotel"%in%filtroP){
        f<-sum(BD$children,na.rm=TRUE)
        text1<-c(text1,paste("El número de niños registrados en el hotel fue de: ",toString(f)))
        
        
      }
      ####
      
      
      #filtro de personas en la base####
      if(("bebes"%in%filtroP)&&"hay"%in%filtroP){
        f<-sum(BD2$babies,na.rm=TRUE)
        text1<-c(text1,paste("El número de bebés registrados fue: ",toString(f)))
        
      }
      if("adultos"%in%filtroP&&"hay"%in%filtroP){
        f<-sum(BD2$adults)
        text1<-c(text1,paste("El número de adultos registrados fue: ",toString(f)))
        
        
      }
      if("niños"%in%filtroP&&"hay"%in%filtroP){
        f<-sum(BD2$children,na.rm=TRUE)
        text1<-c(text1,paste("El número de niños registrados fue: ",toString(f)))
        
      }
      if("personas"%in%filtroP&&"hay"%in%filtroP){
        f<-sum(BD2$adults)+sum(BD2$children,na.rm=TRUE)+sum(BD2$babies,na.rm=TRUE)
        text1<-c(text1,paste("El número de personas registradas fue: ",toString(f)))
        
        
      }
      ####
      
      
      
      
      #filtro de reservaciones canceladas o no####
      if(("personas"%in%filtroP||"reservaciones"%in%filtroP||"reservación"%in%filtroP)&&("cancelada"%in%filtroP||"cancelaron"%in%filtroP||"canceladas"%in%filtroP)){
        
        f<-sum(BD$is_canceled)
        if("no"%in%filtroP){
          f<-sum(BD$is_canceled==0)
          text1<-c(text1,paste("El número de personas que no cancelaron fue: ",toString(f)))
          
        }
        else{
          text1<-c(text1,paste("El número de personas que cancelaron fue: ",toString(f)))
          
        }
        
      }
      ####
      
      
      
      #filro####
      
      #filtro de personas por mes####
      if("personas"%in%filtroP&&("enero"%in%filtroP||"febrero"%in%filtroP||"marzo"%in%filtroP||"abril"%in%filtroP||"mayo"%in%filtroP||"junio"%in%filtroP||"julio"%in%filtroP||"agosto"%in%filtroP||"septiembre"%in%filtroP||"octubre"%in%filtroP||"noviembre"%in%filtroP||"diciembre"%in%filtroP)){
        f<-sum(BD$adults)+sum(BD$children,na.rm=TRUE)+sum(BD$babies,na.rm=TRUE)
        text1<-c(text1,paste("El número de personas registradas  en el hotel fue de: ",toString(f)))
        
        
      }
      if(("bebes"%in%filtroP)&&("enero"%in%filtroP||"febrero"%in%filtroP||"marzo"%in%filtroP||"abril"%in%filtroP||"mayo"%in%filtroP||"junio"%in%filtroP||"julio"%in%filtroP||"agosto"%in%filtroP||"septiembre"%in%filtroP||"octubre"%in%filtroP||"noviembre"%in%filtroP||"diciembre"%in%filtroP)){
        f<-sum(BD$babies,na.rm=TRUE)
        text1<-c(text1,paste("El número de bebés registrados  en el hotel fue de:  ",toString(f)))
        
      }
      if("adultos"%in%filtroP&&("enero"%in%filtroP||"febrero"%in%filtroP||"marzo"%in%filtroP||"abril"%in%filtroP||"mayo"%in%filtroP||"junio"%in%filtroP||"julio"%in%filtroP||"agosto"%in%filtroP||"septiembre"%in%filtroP||"octubre"%in%filtroP||"noviembre"%in%filtroP||"diciembre"%in%filtroP)){
        f<-sum(BD$adults)
        text1<-c(text1,paste("El número de adultos registrados  en el hotel fue de: ",toString(f)))
        
        
      }
      if("niños"%in%filtroP&&("enero"%in%filtroP||"febrero"%in%filtroP||"marzo"%in%filtroP||"abril"%in%filtroP||"mayo"%in%filtroP||"junio"%in%filtroP||"julio"%in%filtroP||"agosto"%in%filtroP||"septiembre"%in%filtroP||"octubre"%in%filtroP||"noviembre"%in%filtroP||"diciembre"%in%filtroP)){
        f<-sum(BD$children,na.rm=TRUE)
        text1<-c(text1,paste("El número de niños registrados  en el hotel fue de: ",toString(f)))
        
        
      }
      #####
      
      
      #Filtro por año####
      if("personas"%in%filtroP&&("2015"%in%filtroP||"2016"%in%filtroP||"2017"%in%filtroP)){
        f<-sum(BD$adults)+sum(BD$children,na.rm=TRUE)+sum(BD$babies,na.rm=TRUE)
        text1<-c(text1,paste("El número de personas registradas en el hotel fue de: ",toString(f)))
        
        
      }
      if(("bebes"%in%filtroP)&&("2015"%in%filtroP||"2017"%in%filtroP||"2016"%in%filtroP)){
        f<-sum(BD$babies,na.rm=TRUE)
        text1<-c(text1,paste("El número de bebés registrados en el hotel fue de: ",toString(f)))
        
      }
      if("adultos"%in%filtroP&&("2015"%in%filtroP||"2016"%in%filtroP||"2017"%in%filtroP)){
        f<-sum(BD$adults)
        text1<-c(text1,paste("El número de adultos registrados en el hotel fue de: ",toString(f)))
        
        
      }
      if("niños"%in%filtroP&&("2015"%in%filtroP||"2016"%in%filtroP||"2017"%in%filtroP)){
        f<-sum(BD$children,na.rm=TRUE)
        text1<-paste("El número de niños registrados en el hotel fue de: ",toString(f))
        
        
      }
      
      
      #####
      
      #Filtro 
      
      
      
      
    }
    if ("cual" %in% filtroP ) {
      if (("mas" %in% filtroP ) && ("reservaciones" %in% filtroP || "reservado" %in% filtroP)) {
        text1 <- c(text1, "El hotel con más reservaciones es el City")
      }
      
      if ("menos" %in% filtroP && ("reservaciones" %in% filtroP || "reservado" %in% filtroP)) {
        text1 <- c(text1, "El hotel con menos reservaciones es el Resort")
      }
      
    
    }
    
    cat(paste(text1,collapse = "\n"))
 
    
  })
  #mensajes o sugerencias
  output$forma1<-renderText({
    req(input$f1)
  paste("La primera forma de buscar es la clásica, por ejemplo:\n
      Ejemplo 1: quiero filtrar los hoteles del año 2017
      Te devolverá solo la tabla de los datos filtrados\n
      Ejemplo 2:cuantas personas cancelaron su reservacion
      Te devolverá la tabla filtrada y el conteo de las personas")
  })
  
  output$forma2<-renderText({
    req(input$f2)
    paste("La segunda forma de buscar es por etiquetas, por ejemplo:\n
          filtrar + hoteles cancelados + mexico + enero + 2016
          Te devolverá solo la tabla de los datos filtrados")
    
  })
  
  output$men1<-renderText({
    req(input$file)
    paste("Las variables se usan para graficar en el histograma.")
  })
  
  output$men2<-renderText({
    paste("
          Para buscar por día debe ingresar lo siguiente
          Día 'número', por ejemplo, gente que se quedó el día 25
          
          Para buscar el estado debe ingresar si el estado está finalizado o cancelado.
          Por ejemplo, personas con el estado cancelado
          
          Para buscar el tipo de habitación que reservó y se le asignó (se tiene habitacion A, B, C, D, E ,F, G, H, L y P) ingresa algo similar a
          Ejemplo 1: personas que se les asignó/dió la habitación A
          Ejemplo 2: personas que pidieron/reservaron habitación A
          Para usar el segundo tipo de busqueda use
          Ejemplo: asignó/dió/reservaron/pidieron habitacion A + en 2017")
  })
}


shinyApp(ui = ui, server = server)
