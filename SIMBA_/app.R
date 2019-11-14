library(shiny)
library(ggplot2)
library(plotly)
library(evaluate)
library(shinythemes)
library(shinyalert)
library(tidyverse)
library(maps)
library(ggrepel)
library(forecast)
library(tseries)
library(vars)
library(MTS)
library(rmarkdown)
library(grid) 
library(rnn) 
library(seastests)
library(webshot)
library(shinyjs)
library(orca)




ideal <- read.csv("a-fito-integrado1.csv", stringsAsFactors = F)
ideal2 <- read.csv("a-fito-integrado2.csv", stringsAsFactors = F)
ideal3<- read.csv("a-biotoxinas-integrado122.csv", stringsAsFactors = F)



  ui <- fluidPage( 
  	theme = shinytheme("cerulean"),

   tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        font-size: 18px;
      }
    ")))



    ,tagList(


    #shinythemes::themeSelector(),
    navbarPage(id="ana1",title="SIMBA",

      #navbarMenu("Informações", icon = icon("gears"),

      #tabPanel("Início", shiny::includeHTML("www/index.html"), icon = icon("home"))),
       #tabPanel("Contactos",shiny::includeHTML("www/contact.html"),icon=icon("phone"))),
     #tabPanel("Início", shiny::includeHTML("www/index.html"), icon = icon("home")),  
     # tabPanel("Informações",icon=icon("gears")



     # 	),

      tabPanel("Visualização", value = "A",
      sidebarLayout(
        sidebarPanel(
             helpText("Nesta secção, poderá visualizar os dados em tabelas e gráficos.")

          ),

     tabsetPanel(id = "dataset",

      
      tabPanel("Dados",

            DT::dataTableOutput("contents")

           ,icon=icon("table")),
        tabPanel("Gráficos" ,


conditionalPanel(condition = "output.fileUploaded ",
        sidebarLayout(
                    sidebarPanel(
                h3("Biotoxinas Marinhas"),                
                   radioButtons("disp1", "Opções",choices = c(Capitania = "Capitania",'Comparar Zonas' = "CompararZonas"),selected = "Capitania"),

                 conditionalPanel(condition = "input.disp1 == 'Capitania'",
                       

                  selectInput(inputId = "nameCapitania",
                      label = "Capitanias de interesse: ",
                      choices = c(""),
                      selected = "Aveiro"),
                  
                    selectInput(inputId = "show_vars",
                      label = "Zonas de interesse: ",
                      choices = c(""),
                      selected = "RIAV1"),

                  checkboxGroupInput("show_vars1", "Espécie:",
                           choices = c(""), selected = c("Mexilhão"))
                  


                  ,tags$hr(style="border-color: DeepSkyBlue;"),
                  checkboxInput("VerTabela", "Ver tabela", TRUE),
                  checkboxInput("VerLimite", "Ver limite regulamentar", TRUE)
                  ),

                 conditionalPanel(condition = "input.disp1 == 'CompararZonas'",

                     selectizeInput("show_vars2",
                               label = "Zonas de interesse: ",
                               choices = c(""),
                               multiple = T,
                               options = list(maxItems = 3, placeholder = 'Seleciona uma zona'),
                               selected = "RIAV1"),

                     selectInput(inputId = "nameComparar",
                      label = "Espécie: ",
                      choices = c("Mexilhão","Berbigão","A. Japonesa","A. Boa","A. Macha","Longueirão","Lambujinha","Conquilha","Pé-de-burrinho"),
                      selected = "Mexilhão"),
                     tags$hr(style="border-color: DeepSkyBlue;"),
                  checkboxInput("VerLimite1", "Ver limite regulamentar", TRUE)
                  ),
                 
                 actionButton('downloadPlotB','Descarregar gráfico',class = "btn-primary")

              ),
               mainPanel(

                 plotlyOutput("trendPlotCapitania"),

                   
                conditionalPanel(condition = "input.VerTabela & input.disp1 == 'Capitania'",
                  tags$hr(),
                   DT::dataTableOutput("contents1"))

              )
              ),
tags$hr(style="border-color: DeepSkyBlue;"),
       sidebarLayout(
              sidebarPanel(
                h3("Fitoplâncton Tóxico"),
     
                selectizeInput("name1",
                               label = "Zonas de interesse: ",
                               choices = unique(ideal$Zona.de.produção),
                               multiple = T,
                               options = list(maxItems = 3, placeholder = 'Seleciona uma zona'),
                               selected = "L3"),
                tags$hr(style="border-color: DeepSkyBlue;"),checkboxInput("VerTabela1", "Ver tabela", TRUE),
                actionButton('downloadPlotF','Descarregar gráfico',class = "btn-primary")
              ),

              mainPanel(
               
                plotlyOutput("trendPlot2"),
                
                conditionalPanel(condition = "input.VerTabela1",
                  tags$hr(),
                DT::dataTableOutput("contents2"))
              )),
tags$hr(style="border-color: DeepSkyBlue;"),
      sidebarLayout(
              
              sidebarPanel(

                h3("Biotoxinas Marinhas e Fitoplâncton Tóxico"),
                
                
              selectInput(inputId = "select",
                      label = "Capitanias de interesse: ",
                      choices = c(""),
                      selected = "Aveiro"),
                  
                    selectInput(inputId = "show_vars3",
                      label = "Zonas de interesse: ",
                      choices = c(""),
                      selected = "RIAV1"),

                   checkboxGroupInput("show_vars4", "Espécie:",
                      choices = c("Mexilhão","Berbigão","A. Japonesa","A. Boa","A. Macha","Longueirão","Lambujinha","Conquilha","Pé-de-burrinho"), selected = c("Mexilhão")),
                   tags$hr(style="border-color: DeepSkyBlue;"),
                  checkboxInput("VerLimite2", "Ver limite regulamentar", TRUE),
                  actionButton('downloadPlotBF','Descarregar gráfico',class = "btn-primary")

                  



              ),

              mainPanel(
                plotlyOutput("trendPlot3")

              ))), conditionalPanel(condition = "output.fileUploaded2 & input.dataset == 'Gráficos'" ,
               useShinyalert())


          ,icon=icon("bar-chart-o"))

    #       tabPanel("Mapa", 

      
    #         sidebarLayout(


    # conditionalPanel(condition = "output.fileUploaded ",
                   
    #        sidebarPanel(
    #         h3("SECÇÃO AINDA EM CONSTRUÇÃO!"),
    #           h3("Biotoxinas"),
    #           verbatimTextOutput("txtout"),
    #           tags$hr(style="border-color: DeepSkyBlue;"),
    #           h3("Fitoplâncton: "),
    #           verbatimTextOutput("txtout2")
              
    #           ),
    #          mainPanel(
    #            conditionalPanel(condition = "output.fileUploaded ",
    #              plotOutput("trendPlotMaps"))


    #           )
    #           ),
    #             conditionalPanel(condition = "output.fileUploaded2 & input.dataset == 'Mapa'" ,
    #            useShinyalert()
                
    #           )  
            
    #         )

    #           ,icon=icon("map")

    #           )
           
      )

   )
  ,icon=icon("pie-chart")
  ),tabPanel(id = "ana","Análise" ,

  tabsetPanel(id = "analise",



     
     tabPanel("Modelos Autorregressivos",
       conditionalPanel(condition = "output.fileUploaded ",
      sidebarLayout(
         
       
          sidebarPanel(
          helpText("Nesta secção, poderá analisar as séries originais, fazer previsões das mesmas e gerar o relatório."),
            tags$hr(style="border-color: DeepSkyBlue;"),

            selectInput(inputId = "modelos",
                      label = "Escolha o modelo: ",
                      choices = c("ARIMA","VAR","VARMA"),
                      selected = "ARIMA"),

          
           
          radioButtons("ajustar", "Ajustar modelo",choices = c(Auto = "Auto",Manual = "Manual"),selected = "Auto"),
          
          

            

          

           conditionalPanel(condition = "input.ajustar=='Manual' && input.modelos=='ARIMA'",
            
            numericInput("numP", label = h5("Valor para p"), min = 0,value = 1),
            numericInput("numQ", label = h5("Valor para q"), min = 0,value = 0),
            numericInput("diff", label = h5("Grau de diferenciação"), min = 0,value = 0)),

           conditionalPanel(condition = "input.ajustar=='Manual' && input.modelos=='VAR'",
            
            numericInput("numPVAR", label = h5("Valor para p"), min = 0,value = 1)
            ),

           conditionalPanel(condition = "input.ajustar=='Manual' && input.modelos=='VARMA'",
            
            numericInput("numPVARMA", label = h5("Valor para p"), min = 0,value = 1),
            numericInput("numQVARMA", label = h5("Valor para q"), min = 0,value = 0)
           )  ,



           tags$hr(style="border-color: DeepSkyBlue;"),
                       
                  selectInput(inputId = "show_vars_Capitania",
                      label = "Capitanias de interesse: ",
                      choices = c(""),
                      selected = "Aveiro"),
                  
                    selectInput(inputId = "show_vars_zonas",
                      label = "Zonas de interesse: ",
                      choices = c(""),
                      selected = "RIAV1"),

                  radioButtons("show_especie", "Espécie",choices = c(Mexilhã = "Mexilhão"),selected = "Mexilhão"),

                   tags$hr(style="border-color: DeepSkyBlue;"),


                    # # Select whether to overlay smooth trend line
                    # conditionalPanel(condition = "input.modelos=='ARIMA' && input.VerDadosPrevisao=='Dados'",

                    # checkboxInput(inputId = "smoother", label = strong("Linha de tendência"), value = FALSE) ,

                    # # Display only if the smoother is checked
                    # conditionalPanel(condition = "input.smoother == true",
                    #                  sliderInput(inputId = "f1", label = "Suavização:",
                    #                              min = 1, max = 30, value = 2, step = 1,
                    #                              animate = animationOptions(interval = 100)) ),

                   # helpText("Aviso: O valor selecionado será usado para suavizar a série."),
                   # tags$hr(style="border-color: DeepSkyBlue;") ),

                   

                    radioButtons("VerDadosPrevisao", "Visualizar",choices = c("Dados","Previsão"),selected = "Dados"),
                    tags$hr(style="border-color: DeepSkyBlue;"),
                    
                    downloadButton("report", "Relatório",class = "btn-primary")

                    
             ),
           mainPanel(

            conditionalPanel(condition="input.VerDadosPrevisao == 'Dados'",
             plotlyOutput("CapitaniaTS"),verbatimTextOutput("txtoutStationary"),
             tags$hr(style="border-color: DeepSkyBlue;"),
             plotlyOutput("plotForecastingFito"),verbatimTextOutput("txtoutStationary2")
             )

             ,tags$hr(style="border-color: DeepSkyBlue;"),

             conditionalPanel(condition="input.VerDadosPrevisao == 'Previsão'",


             
             	radioButtons("VerPrevisao", "Mostrar",choices = c("Previsão","Dados ajustados"),selected = "Previsão"),

             	conditionalPanel(condition="input.VerPrevisao == 'Previsão'",
             	plotOutput("plotForecastingTESTE")),

             	conditionalPanel(condition="input.VerPrevisao == 'Dados ajustados'",
             	plotOutput("plotForecastingFITTED")) ,
             
             downloadButton('downloadPlot','Descarregar gráfico',class = "btn-primary")
              ,tags$hr(style="border-color: DeepSkyBlue;"),
                   

            conditionalPanel(condition="input.modelos== 'ARIMA'",
                   h4("Detalhes do modelo ARIMA"),
                   verbatimTextOutput("txtoutARIMA"),
                   plotOutput("plotForecastingTESTEResiduals") ),

             conditionalPanel(condition="input.modelos== 'VAR'",
                   h4("Detalhes do modelo VAR"),
                   verbatimTextOutput("txtoutVAR"),
                   plotOutput("plotForecastingTESTEResidualsVAR") ),


             conditionalPanel(condition="input.modelos== 'VARMA'",
                   h4("Detalhes do modelo VARMA"),
                   verbatimTextOutput("txtoutVARMA"),
                   plotOutput("plotForecastingTESTEResidualsVARMA") )


              )

              )
           )), icon=icon("fast-backward")
        

      ),
               
     tabPanel("Redes Neuronais Artificiais",
      conditionalPanel(condition = "output.fileUploaded",


    sidebarLayout(


       
          sidebarPanel(
          helpText("Nesta secção, poderá analisar as séries originais, fazer previsões das mesmas e gerar o relatório."),
            tags$hr(style="border-color: DeepSkyBlue;"),
          selectInput(inputId = "modelos_ANN",
                      label = "Escolha o modelo: ",
                      choices = c("NNAR","RNN"),
                      selected = "NNAR"),

          radioButtons("ajustar1", "Ajustar modelo",choices = c(Auto = "Auto",Manual = "Manual"),selected = "Auto"),

           conditionalPanel(condition = "input.ajustar1=='Manual' && input.modelos_ANN=='NNAR'",
            
            numericInput("numPNNAR", label = h5("Valor para p"), min = 0,value = 1)),

           conditionalPanel(condition = "input.ajustar1=='Manual' && input.modelos_ANN=='RNN'",
            
            numericInput("numLearningrate", label = h5("Taxa de aprendizagem"), min = 0,value = 1,step=0.1),
            numericInput("numhidden_dim", label = h5("Dimensões das camadas ocultas"), min = 0,value = 10,step=1) ,
            numericInput("numEpochs", label = h5("Numero de iterações (epocas)"), min = 0,value = 100,step=10) 
            )
          ,

          tags$hr(style="border-color: DeepSkyBlue;"),

          selectInput(inputId = "Capitania_ANN",
                      label = "Capitanias de interesse: ",
                      choices = c(""),
                      selected = "Aveiro"),

           selectInput(inputId = "Zonas_ANN",
                      label = "Zonas de interesse: ",
                      choices = c(""),
                      selected = "RIAV1"),

            radioButtons("Especie_ANN", "Espécie",choices = c(Mexilhão = "Mexilhão"),selected = "Mexilhão"),
            tags$hr(style="border-color: DeepSkyBlue;"),


        # conditionalPanel(condition = "input.modelos_ANN=='NNAR' && input.VerDadosPrevisao2=='Dados'",

        #     checkboxInput(inputId = "smoother_ANN", label = strong("Linha de tendência"), value = FALSE) ,

        #             # Display only if the smoother is checked
        #             conditionalPanel(condition = "input.smoother_ANN == true",
        #                              sliderInput(inputId = "f2", label = "Suavização:",
        #                                          min = 1, max = 30, value = 2, step = 1,
        #                                          animate = animationOptions(interval = 100)) ) ,
        #             helpText("Aviso: O valor selecionado será usado para suavizar a série."),

        #             tags$hr(style="border-color: DeepSkyBlue;") ),
           
             radioButtons("VerDadosPrevisao2", "Visualizar",choices = c("Dados","Previsão"),selected = "Dados"),
              tags$hr(style="border-color: DeepSkyBlue;"),
            downloadButton("report_ANN", "Relatório",class = "btn-primary")



            ),
           mainPanel(
            conditionalPanel(condition="input.VerDadosPrevisao2 == 'Dados'",
                plotlyOutput("CapitaniaTS_ANN"),
                tags$hr(style="border-color: DeepSkyBlue;"),
                plotlyOutput("plotForecastingFito_ANN")),

            conditionalPanel(condition="input.VerDadosPrevisao2 == 'Previsão'",

               radioButtons("VerPrevisaoANN", "Mostrar",choices = c("Previsão","Dados ajustados"),selected = "Previsão"),

             	conditionalPanel(condition="input.VerPrevisaoANN == 'Previsão'",
             	plotOutput("plotForecasting_ANNTESTE")),

             	conditionalPanel(condition="input.VerPrevisaoANN == 'Dados ajustados'",
             	plotOutput("plotForecastingFITTED_ANN")), 


          
              downloadButton('downloadPlot2','Descarregar gráfico',class = "btn-primary"),
            tags$hr(style="border-color: DeepSkyBlue;"),

            conditionalPanel(condition="input.modelos_ANN== 'NNAR'",
                   h4("Detalhes do modelo NNAR"),
                   verbatimTextOutput("txtoutNNAR"),
                   plotOutput("plotForecastingNNARResiduals") ),

                   conditionalPanel(condition="input.modelos_ANN== 'RNN'",
                   h4("Detalhes do modelo RNN"),
                   verbatimTextOutput("txtoutRNN"),
                   plotOutput("plotForecastingRNNerror") ) 
 



            )


                
              )

          )

      ),icon=icon("connectdevelop")),tabPanel("Modelo Selecionado",   

     sidebarLayout(
                    

       
          sidebarPanel(helpText("Nesta secção, poderá ver o modelo selecionado para uma determiada zona (Baseado nas medidas de erro)."),
            tags$hr(style="border-color: DeepSkyBlue;"),

             selectInput(inputId = "Capitania_BESTMODEL",
                      label = "Capitanias de interesse: ",
                      choices = c(""),
                      selected = "Aveiro"),

           selectInput(inputId = "Zonas_BESTMODEL",
                      label = "Zonas de interesse: ",
                      choices = c(""),
                      selected = "RIAV1"),
           radioButtons("Especie_BESTMODEL", "Espécie",choices = c(Mexilhão = "Mexilhão"),selected = "Mexilhão"),
           tags$hr(style="border-color: DeepSkyBlue;"),

           radioButtons("BESTMODEL", "Modelo selecionado",choices = c("Modelos Autorregressivos","Redes Neuronais Artificiais"),selected = "Modelos Autorregressivos")

            ),

          mainPanel(

            conditionalPanel(condition="input.BESTMODEL == 'Modelos Autorregressivos'",
                   h4("Detalhes do modelo selecionado"),
                   verbatimTextOutput("txtoutBESTMODEL"),
                   tags$hr(style="border-color: DeepSkyBlue;"),
                   plotOutput("plotForecastingBESTMODEL"),
                   tags$hr(style="border-color: DeepSkyBlue;"),
                   plotOutput("plotForecastingBESTMODELRESIDUAL")
                   ),

            
          conditionalPanel(condition="input.BESTMODEL == 'Redes Neuronais Artificiais'",
                   h4("Detalhes do modelo selecionado"),
                   verbatimTextOutput("txtoutBESTMODEL_ANN"),
                   tags$hr(style="border-color: DeepSkyBlue;"),
                   plotOutput("plotForecastingBESTMODEL_ANN"),
                   tags$hr(style="border-color: DeepSkyBlue;"),
                   plotOutput("plotForecastingBESTMODEL_ANN_RESIDUAL")
                   )
          )

            




          )


) ) ,




  icon=icon("line-chart")),
      tabPanel("Contactos" ,icon=icon("phone"),

      	sidebarLayout(
      		sidebarPanel(
helpText("Campus Alameda - Av. Rovisco Pais, 1 1049-001 Lisboa, Tel: +351 218 417 000, Fax: +351 218 499 242, mail@tecnico.ulisboa.pt")
      			

      			

      			),mainPanel(pre(includeText("contactos.txt")) ,   

                div(
      id = "form",
      
      textInput("Firstname", "Primeiro nome", ""),
      textInput("LastName", "Último nome"),
      textInput("Email", "E-mail"),
      textInput("Sugest", "Dúvida ou sugestão"),
      actionButton("submit", "Submeter", class = "btn-primary")
    ))

      		)
  
    

      	)
  )
 )   
 ) 

#=================================================================================================================================================

  server <- function(input, output,session) {
  	plotreport<-1
    plotreport1 <- 1
    plotreport2 <-1

    plotreport_ANN<-1
    plotreport1_ANN<-1
    plotreport2_ANN<-1
   AccuracyModel <-0
   AccuracyModelANN<-0

   SAVEPLOT <- 0
   SAVEPLOT_ANN <- 0
   SAVEPLOT_B <- 0
   SAVEPLOT_F <- 0
   SAVEPLOT_BF <- 0
   SAVEPLOT_FItted<-0
   
    

     df = reactive({


      inFile <- "a-biotoxinas-integrado12.csv"
      df<-read.csv(inFile, header = TRUE,stringsAsFactors=TRUE)
     
     
   })


    df2 = reactive({ #Dados Fitoplan seccao analise 

      df_trend <- df()[df()$Zona.de.produção == input$show_vars_zonas, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$show_especie, ] 


     TableFito <- ideal[ideal$Zona.de.produção == input$show_vars_zonas, ]
      TableBio<- df_trend3
      "Produtoras de DSP" <- rep(0,length(TableBio$Espécie)) 
      TableBio <- cbind(TableBio,`Produtoras de DSP`)
      apagar <- c()
    


    #ALINHAR DATAS======================================================================

   for(i in 1:length(TableBio$Data.da.colheita)) {
    
    for(j in 1:length(TableFito$Data.da.colheita)){
        if(TableBio$Data.da.colheita[i] == TableFito$Data.da.colheita[j])
        {
            apagar <- c(apagar,TableFito$Produtoras.de.DSP[j])
        }
    }
    
    if(length(apagar)==0 || apagar=="-Inf")
    {
        apagar <- TableFito[as.Date(TableFito$Data.da.colheita) >= as.Date(TableBio$Data.da.colheita[i]) - 5 & as.Date(TableFito$Data.da.colheita) <=  as.Date(TableBio$Data.da.colheita[i]) + 5 ,]
        
        if(length(apagar$Produtoras.de.DSP)==0){
            TableBio$Produtoras.de.DSP[i] <- 20
        }else{
            TableBio$Produtoras.de.DSP[i] <- max(apagar$Produtoras.de.DSP)
        }
        
    }else{
        TableBio$Produtoras.de.DSP[i] <- max(apagar)
    }
    
    apagar <- c()
    
   }
      

    df2 <- TableBio    
     
   })








       df3 = reactive({ #Dados Fitoplan seccao analise 

      df_trend <- df()[df()$Zona.de.produção == input$Zonas_ANN, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$Especie_ANN, ] 


     TableFito <- ideal[ideal$Zona.de.produção == input$Zonas_ANN, ]
      TableBio<- df_trend3
      "Produtoras de DSP" <- rep(0,length(TableBio$Espécie)) 
      TableBio <- cbind(TableBio,`Produtoras de DSP`)
      apagar <- c()
    


    #ALINHAR DATAS======================================================================

   for(i in 1:length(TableBio$Data.da.colheita)) {
    
    for(j in 1:length(TableFito$Data.da.colheita)){
        if(TableBio$Data.da.colheita[i] == TableFito$Data.da.colheita[j])
        {
            apagar <- c(apagar,TableFito$Produtoras.de.DSP[j])
        }
    }
    
    if(length(apagar)==0 || apagar=="-Inf")
    {
        apagar <- TableFito[as.Date(TableFito$Data.da.colheita) >= as.Date(TableBio$Data.da.colheita[i]) - 5 & as.Date(TableFito$Data.da.colheita) <=  as.Date(TableBio$Data.da.colheita[i]) + 5 ,]
        
        if(length(apagar$Produtoras.de.DSP)==0){
            TableBio$Produtoras.de.DSP[i] <- 20
        }else{
            TableBio$Produtoras.de.DSP[i] <- max(apagar$Produtoras.de.DSP)
        }
        
    }else{
        TableBio$Produtoras.de.DSP[i] <- max(apagar)
    }
    
    apagar <- c()
    
   }
      

    df3 <- TableBio    
     
   })




 df4 = reactive({ #Dados Fitoplan seccao analise 

      df_trend <- df()[df()$Zona.de.produção == input$Zonas_BESTMODEL, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$Especie_BESTMODEL, ] 


     TableFito <- ideal[ideal$Zona.de.produção == input$Zonas_BESTMODEL, ]
      TableBio<- df_trend3
      "Produtoras de DSP" <- rep(0,length(TableBio$Espécie)) 
      TableBio <- cbind(TableBio,`Produtoras de DSP`)
      apagar <- c()
    


    #ALINHAR DATAS======================================================================

   for(i in 1:length(TableBio$Data.da.colheita)) {
    
    for(j in 1:length(TableFito$Data.da.colheita)){
        if(TableBio$Data.da.colheita[i] == TableFito$Data.da.colheita[j])
        {
            apagar <- c(apagar,TableFito$Produtoras.de.DSP[j])
        }
    }
    
    if(length(apagar)==0 || apagar=="-Inf")
    {
        apagar <- TableFito[as.Date(TableFito$Data.da.colheita) >= as.Date(TableBio$Data.da.colheita[i]) - 5 & as.Date(TableFito$Data.da.colheita) <=  as.Date(TableBio$Data.da.colheita[i]) + 5 ,]
        
        if(length(apagar$Produtoras.de.DSP)==0){
            TableBio$Produtoras.de.DSP[i] <- 20
        }else{
            TableBio$Produtoras.de.DSP[i] <- max(apagar$Produtoras.de.DSP)
        }
        
    }else{
        TableBio$Produtoras.de.DSP[i] <- max(apagar)
    }
    
    apagar <- c()
    
   }
      

    df4 <- TableBio    
     
   })




output$downloadPlot <- downloadHandler(
         filename = function(){paste("input$plotForecastingTESTE",'.png',sep='')},
         content = function(file){
          ggsave(file,plot=SAVEPLOT)

    }

    )

output$downloadPlot2 <- downloadHandler(
         filename = function(){paste("input$plotForecasting_ANNTESTE",'.png',sep='')},
         content = function(file){
          ggsave(file,plot=SAVEPLOT_ANN)

    }

    )



    

   #==============================================================================================DOWNLOAD
   output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.pdf",

      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).

        
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        # forecastPlot1()

        # Set up parameters to pass to Rmd document
        params <- list(B=plotreport1,F=plotreport2,p = plotreport,t=input$modelos,A=AccuracyModel)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )  

    #==============================================================================================DOWNLOAD
   output$report_ANN <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "reportANN.pdf",

      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).

        
        tempReport <- file.path(tempdir(), "reportANN.Rmd")
        file.copy("reportANN.Rmd", tempReport, overwrite = TRUE)
        # forecastPlot1_ANN()

        # Set up parameters to pass to Rmd document
        params <- list(B=plotreport1_ANN,F=plotreport2_ANN,p = plotreport_ANN,t=input$modelos_ANN,A=AccuracyModelANN)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )  

    









# observe({
#        if(input$modelos!="ARIMA"){
#          updateCheckboxInput(session, "smoother", value =FALSE )
#       }
#      })

# observe({
#        if(input$modelos_ANN!="NNAR"){
#          updateCheckboxInput(session, "smoother_ANN", value =FALSE )
#       }
#      })

observe({
       if(input$modelos =="VARMA"){
         updateRadioButtons(session, "VerPrevisao",choices = "Previsão",selected = "Previsão")
     }else{
     	updateRadioButtons(session, "VerPrevisao",choices = c("Previsão","Dados ajustados"),selected = "Previsão")
     }
     })


observe({

       if(input$modelos_ANN =="RNN"){
         updateRadioButtons(session, "VerPrevisaoANN",choices = "Previsão",selected = "Previsão")
     }else{
     	updateRadioButtons(session, "VerPrevisaoANN",choices = c("Previsão","Dados ajustados"),selected = "Previsão")
     }
     
     })






  output$plotForecasting_ANNTESTE <- renderPlot({ 

  	  df_trend <- df()[df()$Zona.de.produção == input$Zonas_ANN, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$Especie_ANN, ] 
      Data <-  as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y")
      ultimaData <- max(Data)

     NovaData <- ultimaData + 7
     for(i in 1:3){
       NovaData <- c(NovaData,max(as.Date(NovaData))+7)
      }


lag_transform <- function(x, k= 1){
    
      lagged =  c(rep(NA, k), x[1:(length(x)-k)])
      DF = as.data.frame(cbind(lagged, x))
      colnames(DF) <- c( paste0('x-', k), 'x')
      DF[is.na(DF)] <- 0
      return(DF)
}
scale_data = function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  
}

invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for( i in 1:t){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}

       if(input$modelos_ANN=="NNAR"){

        Bio_ts <- ts(df_trend3[, c('Toxinas.Lipofílicas')])

      #    if(input$smoother_ANN){
      #    Bio_ts = ma(Bio_ts, order=input$f2)
      #    Bio_ts <- na.omit(Bio_ts)
      #    showNotification("Serie foi suavizada!",type="message")
      # }




  #CRIAR CONJUNTO DE TESTE (será usado no Accuracy)
   dataTeste <- Bio_ts[(length(Bio_ts)-3):length(Bio_ts)]
   dataTrain <- Bio_ts[1:(length(Bio_ts)-4)]

   if(input$ajustar1=="Manual"){  
   fitTeste <- nnetar(dataTrain,p=input$numPNNAR)
  
  }else{
      fitTeste <- nnetar(dataTrain)
  }

   fcastTeste <- forecast(fitTeste,h=4)
   accuracyTeste<- accuracy(fcastTeste,dataTeste)
   AccuracyModelANN <<- accuracyTeste

 

 if(input$ajustar1=="Manual"){  
  fit <- nnetar(Bio_ts,p=input$numPNNAR)
  }else{
      fit <- nnetar(Bio_ts)
  }


 

   output$plotForecastingFITTED_ANN  <- renderPlot({  
        qq<- ggplot(df_trend3,aes(x=as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y"),y=as.numeric(df_trend3$Toxinas.Lipofílicas),colour="Dados originais"))+geom_line() +
      xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)")+ 
      geom_line(aes(y=fit$fitted, linetype = "Dados ajustados"), colour= 'DeepSkyBlue') + 
      scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue"))))+labs(color=" ") + 
      theme_classic() + theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
      qq
       
	})



    
    fcast <- forecast(fit, h=4)

    #Mudar 20 Para 0
      df_trend <- df()[df()$Zona.de.produção == input$Zonas_ANN, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$Especie_ANN, ] 




    Bio_ts <- ts(df_trend3[, c('Toxinas.Lipofílicas')])
    ts2 <- c(Bio_ts,fcast$mean)

    time <- c(Data,NovaData)
    var1 <- ts2
    DF <- data.frame(time, var1)
    p <- ggplot(DF, aes(time, var1, colour=(time>=ultimaData)))+ xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") + geom_line(aes(group=1))+ geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'NavyBlue')+ 
    scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue")))) +labs(color=" ") + scale_color_manual(labels = c("Original", "Previsão"), values = c("blue", "red")) + theme_classic() +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
    print(p)
    

     # TESTES PARA QUALIDADE DO MODELO
    shapiro<- shapiro.test(residuals(fit)) #teste Normalidade
    BoxPierce <- Box.test(fit$residuals, lag = 1, type = c("Box-Pierce")) #Teste independencia dos residuos
    BoxLjung <- Box.test(fit$residuals, lag = 1, type = c("Ljung-Box")) #Teste independencia dos residuos

  print(input$Zonas_ANN)
  print(input$Especie_ANN)
  print(accuracyTeste)
  print(fit)
  

    output$txtoutNNAR <- renderText({
      paste("Shapiro-Wilk teste de normalidade","| w:",shapiro$statistic ,"P-value: ",shapiro$p.value,"\n",
       BoxPierce$method, "| X-squared:", BoxPierce$statistic, "P-value: ", BoxPierce$p.value , "\n", 
       BoxLjung$method, "| X-squared:", BoxLjung$statistic, "P-value: ", BoxLjung$p.value, "\n", "\n", 
       "Medidas de precisão para o modelo de previsão:" ,"\n",
       "Conjunto de treino |" ,
       "ME:", accuracyTeste[1,1], "RMSE:",accuracyTeste[1,2],"MAE:",accuracyTeste[1,3],"\n","MPE:",accuracyTeste[1,4],"MAPE:",accuracyTeste[1,5],"\n","\n",

       "Conjunto de teste |",
        "ME:", accuracyTeste[2,1], "RMSE:",accuracyTeste[2,2],"MAE:",accuracyTeste[2,3],"\n","MPE:",accuracyTeste[2,4],"MAPE:",accuracyTeste[2,5]
        ,sep = " ")
    })

    output$plotForecastingNNARResiduals  <- renderPlot({ 
    tsdisplay(residuals(fit), main='Resíduos NNAR')
     })
   
    
   }

    if(input$modelos_ANN=="RNN"){
    	 Bio_ts <- ts(df_trend3[, c('Toxinas.Lipofílicas')])
    	 diffed = diff(Bio_ts, differences = 1)
    	 supervised = lag_transform(diffed, 1)
    	 N = nrow(supervised)
         n = round(N *0.9, digits = 0)
         train = supervised[1:n, ]
         test  = supervised[(n+1):N,  ]
        
        Scaled = scale_data(train, test, c(-1, 1))

		y_train = Scaled$scaled_train[, 2]
		x_train = Scaled$scaled_train[, 1]

		y_test = Scaled$scaled_test[, 2]
		x_test = Scaled$scaled_test[, 1]

		dim(x_train) <- c(length(x_train), 1, 1)
		dim(y_train) <- c(length(y_train), 1, 1)


		
      if(input$ajustar1=="Manual"){ 
       model <- trainr(Y=y_train,X=x_train,learningrate = input$numLearningrate,hidden_dim = input$numhidden_dim,numepochs = input$numEpochs)
      } else{

      	model <- trainr(Y=y_train,X=x_train,learningrate = 0.5,hidden_dim = 10,numepochs=100)
      }


		
        Eperror <- model$error



		scaler = Scaled$scaler
		predictions = numeric(4)
		X = x_test[length(x_test)]

      #Previsão de 5 semanas      
		for(i in 1:4){
		     dim(X) = c(1,1,1)
		     yhat = model %>% predictr(X)
		     X = yhat
		     # invert scaling
		     yhat = invert_scaling(yhat, scaler,  c(-1, 1))
		     # invert differencing
		     yhat  = yhat + Bio_ts[(n+i)]

		     # store
		     predictions[i] <- yhat
		 }


         #TESTAR FITTED VALUES train
		 predictionsTRAIN = numeric(length(x_train))

		 for(i in 1:length(x_train)){

		 	 Y = x_train[i]

             dim(Y) = c(1,1,1)

		     yhat2 = model %>% predictr(Y)
		     # invert scaling
		     yhat2 = invert_scaling(yhat2, scaler,  c(-1, 1))
		     # invert differencing
		     yhat2  = yhat2 + Bio_ts[(i)]
		     # store
		     predictionsTRAIN[i] <- yhat2
		 }

		 #TESTAR FITTED VALUES teste
		 predictionsTESTE = numeric(4)
		 for(i in 1:4){

		 	 Z = x_test[i]

             dim(Z) = c(1,1,1)

		     yhat3 = model %>% predictr(Z)
		     # invert scaling
		     yhat3 = invert_scaling(yhat3, scaler,  c(-1, 1))
		     # invert differencing
		     yhat3  = yhat3 + Bio_ts[(i+2)]
		     # store
		     predictionsTESTE[i] <- yhat3
		 }
          
        
          accuracyTesteFitted <-accuracy(Bio_ts[1:n],predictionsTRAIN)
          accuracyTeste<- accuracy(Bio_ts[n+1:n+3],predictionsTESTE)	 
          AccuracyModelANN <<- accuracyTesteFitted

        #Mudar 20 Para 0
      df_trend <- df()[df()$Zona.de.produção == input$Zonas_ANN, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$Especie_ANN, ] 

    Bio_ts <- ts(df_trend3[, c('Toxinas.Lipofílicas')])

    ts2 <- c(Bio_ts,predictions)
    time <- c(Data,NovaData)
    var1 <- ts2
    DF <- data.frame(time, var1)
    p <- ggplot(DF, aes(time, var1, colour=(time>=ultimaData)))+ xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") + geom_line(aes(group=1))+ geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'NavyBlue')+ 
    scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue")))) + labs(color=" ") + scale_color_manual(labels = c("Original", "Previsão"), values = c("blue", "red")) + theme_classic() +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
    print(p)



    output$txtoutRNN <- renderText({

    	 paste("Medidas de precisão para o modelo de previsão:" ,"\n",
       
       "Conjunto de treino |" ,

       "ME:", accuracyTesteFitted[1], "RMSE:",accuracyTesteFitted[2],"MAE:",accuracyTesteFitted[3],"\n","MPE:",accuracyTesteFitted[4],"MAPE:",accuracyTesteFitted[5],"\n","\n",

       "Conjunto de teste |",
        "ME:", accuracyTeste[1], "RMSE:",accuracyTeste[2],"MAE:",accuracyTeste[3],"\n","MPE:",accuracyTeste[4],"MAPE:",accuracyTeste[5]

               ,sep = " ")

	})
   
     output$plotForecastingRNNerror <- renderPlot({ 
       plot(colMeans(Eperror),type = "l",xlab="Época",ylab = "Erro",main='Erros ao longo das épocas')
     })
    
    
    }

   

   plotreport_ANN<<-recordPlot()
   SAVEPLOT_ANN <<- p

  	})
#===================================================================================





  observe({
       if(input$modelos!="ARIMA"){
        showNotification("Dados de biotoxinas e fitoplâncton serão usados!",type="warning")
      }
     })

  observe({
       if(input$modelos=="ARIMA"){
        showNotification("Dados de biotoxinas serão usados!",type="warning")
      }
     })


   observeEvent(input$downloadPlotB,{
     orca(SAVEPLOT_B, "Biotoxinas.png")
    showNotification("Gráfico guardado no diretório",type="message")
  })

   observeEvent(input$downloadPlotF,{
     orca(SAVEPLOT_F, "Fitoplan.png")
    showNotification("Gráfico guardado no diretório",type="message")
  })

   observeEvent(input$downloadPlotBF,{
     orca(SAVEPLOT_BF, "BiotFito.png")
    showNotification("Gráfico guardado no diretório",type="message")
  })


   observe({

      df_trend <- df()[df()$Capitania == input$nameCapitania, ]
      x <- unique(df_trend$Zona.de.produção)
      x<-sort(x)
    # Can also set the label and select items
    updateSelectInput(session, "show_vars",
      label = paste("Zonas de interesse: ", length(x)),
      choices = x,
      selected = head(x, 1)
    )
  })

   observe({

      df_trend <- df()[df()$Capitania == input$nameCapitania, ]
      df_trend1 <- df_trend[df_trend$Zona.de.produção == input$show_vars, ]
      x <- unique(df_trend1$Espécie)
      x<-sort(x)
    # Can also set the label and select items
    updateCheckboxGroupInput(session, "show_vars1",
      label = paste("Espécie: ", length(x)),
      choices = x,
      selected = head(x, 1)
    )
  })


observe({

      df_trend <- df()[df()$Zona.de.produção %in% input$show_vars2, ]
      x <- unique(df_trend$Espécie)
      x<-sort(x)
    # Can also set the label and select items
    updateSelectInput(session, "nameComparar",
      label = paste("Espécie: ", length(x)),
      choices = x,
      selected = head(x, 1)
    )
  })


   output$fileUploaded3 <- reactive({
    return(input$update)
     })
   #======================================================================================================================
    output$fileUploaded <- reactive({
    return(TRUE)
     })
      outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

   #=====================================================================================================================
    output$fileUploaded2 <- reactive({
    return(is.null(input$file1))
     })
      outputOptions(output, 'fileUploaded2', suspendWhenHidden=FALSE)

   #======================================================================================================================
   
   observe({
      x <- unique(df()$Capitania)
      x<-sort(x)
    # Can also set the label and select items
    updateSelectInput(session, "nameCapitania",
      label = paste("Capitanias de interesse: ", length(x)),
      choices = x,
      selected = "Aveiro"
    )

    updateSelectInput(session, "Capitania_ANN",
      label = paste("Capitanias de interesse: ", length(x)),
      choices = x,
      selected = "Aveiro"
    )

     updateSelectInput(session, "Capitania_BESTMODEL",
      label = paste("Capitanias de interesse: ", length(x)),
      choices = x,
      selected = "Aveiro"
    )

  })

   observe({
      x <- unique(df()$Zona.de.produção)
      x<-sort(x)
    # Can also set the label and select items
    updateSelectInput(session, "show_vars2",
      label = paste("Zonas de interesse: ", length(x)),
      choices = x,
      selected = "L3"
    )
  })

   observe({
      x <- unique(df()$Capitania)
      x<-sort(x)
    # Can also set the label and select items
    updateSelectInput(session, "select",
      label = paste("Capitanias de interesse: ", length(x)),
      choices = x,
      selected = "Aveiro"
    )
  })

    observe({

      df_trend <- df()[df()$Capitania == input$select, ]
      x <- unique(df_trend$Zona.de.produção)
      x<-sort(x)
    # Can also set the label and select items
    updateSelectInput(session, "show_vars3",
      label = paste("Zonas de interesse: ", length(x)),
      choices = x,
      selected = head(x, 1)
    )
  })

    observe({

      df_trend <- df()[df()$Capitania == input$Capitania_ANN, ]
      x <- unique(df_trend$Zona.de.produção)
      x<-sort(x)
    # Can also set the label and select items
    updateSelectInput(session, "Zonas_ANN",
      label = paste("Zonas de interesse: ", length(x)),
      choices = x,
      selected = head(x, 1)
    )
  })

    observe({

      df_trend <- df()[df()$Capitania == input$Capitania_BESTMODEL, ]
      x <- unique(df_trend$Zona.de.produção)
      x<-sort(x)
    # Can also set the label and select items
    updateSelectInput(session, "Zonas_BESTMODEL",
      label = paste("Zonas de interesse: ", length(x)),
      choices = x,
      selected = head(x, 1)
    )
  })


    
    

     observe({

      df_trend <- df()[df()$Capitania == input$select, ]
      df_trend1 <- df_trend[df_trend$Zona.de.produção == input$show_vars3, ]
      x <- unique(df_trend1$Espécie)
      x<-sort(x)
    # Can also set the label and select items
    updateCheckboxGroupInput(session, "show_vars4",
      label = paste("Espécie: ", length(x)),
      choices = x,
      selected = head(x, 1)
    )
  })


observe({
      x <- unique(df()$Capitania)
      x<-sort(x)
    # Can also set the label and select items
    updateSelectInput(session, "show_vars_Capitania",
      label = paste("Capitanias de interesse: ", length(x)),
      choices = x,
      selected = "Aveiro"
    )
  })


 observe({

      df_trend <- df()[df()$Capitania == input$show_vars_Capitania, ]
      x <- unique(df_trend$Zona.de.produção)
      x<-sort(x)
    # Can also set the label and select items
    updateSelectInput(session, "show_vars_zonas",
      label = paste("Zonas de interesse: ", length(x)),
      choices = x,
      selected = head(x, 1)
    )
  })

  observe({

      df_trend <- df()[df()$Capitania == input$show_vars_Capitania, ]
      df_trend1 <- df_trend[df_trend$Zona.de.produção == input$show_vars_zonas, ]
      x <- unique(df_trend1$Espécie)
      x<-sort(x)
    # Can also set the label and select items
    updateRadioButtons(session, "show_especie",
      label = paste("Espécie: ", length(x)),
      choices = x,
      selected = head(x, 1)
    )
  })

  observe({

      df_trend <- df()[df()$Zona.de.produção == input$Zonas_ANN, ]
      #df_trend1 <- df_trend[df_trend$Zona.de.produção == input$show_vars_zonas, ]
      x <- unique(df_trend$Espécie)
      x<-sort(x)
    # Can also set the label and select items
    updateRadioButtons(session, "Especie_ANN",
      label = paste("Espécie: ", length(x)),
      choices = x,
      selected = head(x, 1)
    )
  })

  observe({

      df_trend <- df()[df()$Zona.de.produção == input$Zonas_BESTMODEL, ]
      #df_trend1 <- df_trend[df_trend$Zona.de.produção == input$show_vars_zonas, ]
      x <- unique(df_trend$Espécie)
      x<-sort(x)
    # Can also set the label and select items
    updateRadioButtons(session, "Especie_BESTMODEL",
      label = paste("Espécie: ", length(x)),
      choices = x,
      selected = head(x, 1)
    )
  })











output$selected_var <- renderText({ 
      df_trend <- df()[df()$Capitania == input$nameCapitania, ]
      df_trend3 <- unique(df_trend$Zona.de.produção)
    paste(df_trend3)
  })


    output$contents <- DT::renderDataTable({    
      DT::datatable(df(), options = list(pageLength = 10, language= list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese.json')))
  })




output$contents1 <- DT::renderDataTable({
      df_trend <- df()[df()$Zona.de.produção == input$show_vars, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$show_vars1, ]
      DT::datatable(df_trend3, options = list(pageLength = 7, language= list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese.json')))
  })




   output$trendPlotCapitania <- renderPlotly({


     
      
     if (length(input$nameCapitania) == 0) {
      print("Selecione pelo menos uma capitania")
    } else {

         if(input$disp1 == 'Capitania'){

          validate(
      need(length(input$show_vars1) != 0, "Por favor, selecione pelo menos uma espécie."),
      errorClass = "myClass"

      )
           df_trend <- df()[df()$Zona.de.produção == input$show_vars, ]
           df_trend3 <- df_trend[df_trend$Espécie %in% input$show_vars1, ]
           

          ay <- list(
         
          overlaying = "y",
          side = "lefth",
          title = "Toxinas AO+DTXs ( Microgramas AO equiv/kg)",
          size = 14
          )
 

          if(input$VerLimite==TRUE){
          p <- plot_ly() %>%
          add_lines(as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y"), y = 160,name="Limite regulamentar") %>%
          add_lines(as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df_trend3$Toxinas.Lipofílicas),by = df_trend3$Espécie, color = df_trend3$Espécie, yaxis = "y") %>%

          layout(
          title = "", yaxis = ay,
          xaxis = list(title="Data"),
          legend=list(orientation = 'h',y=20) 
       
        ) 
       }else{
           p <- plot_ly() %>%
         
          add_lines(as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df_trend3$Toxinas.Lipofílicas),by = df_trend3$Espécie, color = df_trend3$Espécie,, yaxis = "y") %>%
        

          layout(
          title = "", yaxis = ay,
          xaxis = list(title="Data"),
          legend=list(x=1.1,y=1.1)
          
        ) 
    



       }

         }else{

            validate(
            need(length(input$show_vars2) != 0, "Por favor, selecione pelo menos uma zona de interesse."),
            errorClass = "myClass"
            )

              df_trend <- df()[df()$Zona.de.produção %in% input$show_vars2, ]
              df_trend3 <- df_trend[df_trend$Espécie %in% input$nameComparar, ]




        ay <- list(
         
          overlaying = "y",
          side = "lefth",
          title = "Toxinas AO+DTXs ( Microgramas AO equiv/kg)",
          size = 14
          )

       if(input$VerLimite1 == TRUE){
          p <- plot_ly() %>%
           add_lines(as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y"), y = 160,name="Limite regulamentar") %>%
          add_lines(as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df_trend3$Toxinas.Lipofílicas), by = df_trend3$Zona.de.produção, color = df_trend3$Zona.de.produção, yaxis = "y") %>%
        
          layout(
          title = "", yaxis = ay,
          xaxis = list(title="Data"),
          legend=list(orientation = 'h',y=20) 
         
        ) 
        }else{
            
          p <- plot_ly() %>%
      
          add_lines(as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df_trend3$Toxinas.Lipofílicas), by = df_trend3$Zona.de.produção, color = df_trend3$Zona.de.produção, yaxis = "y") %>%
       
          layout(
          title = "", yaxis = ay,
          xaxis = list(title="Data"),
          legend=list(orientation = 'h',y=20) 
        
        ) 
         }

        }

     

      

    }

    SAVEPLOT_B<<-p

  })


output$trendPlotMaps <- renderPlot({
# Portugal <- map_data("world") %>% filter(region=="Portugal")
# data=world.cities %>% filter(country.etc=="Portugal")

# ggplot() +
#   geom_polygon(data = Portugal, aes(x=long, y = lat, group = group), fill="grey", alpha= 1) +
#   geom_point( data=data, aes(x=long, y=lat, alpha=pop),color="DeepSkyBlue",alpha=1,size=3) +
#   geom_text_repel( data=data %>% arrange(pop) %>% tail(2), aes(x=long, y=lat, label=name), size=5) +
#   geom_point( data=data %>% arrange(pop) %>% tail(2), aes(x=long, y=lat), color="red", size=3) +
#   theme_void() + coord_map() + theme(legend.position="none")


  })

  output$trendPlot2 <- renderPlotly({
#TODO MUDAR PARA ENTRAR JUNTAMENTE COM OS DADOS DE BIOTOXINAS

validate(
      need(length(input$name1) != 0, "Por favor, selecione pelo menos uma zona de interesse.")
      )

    if (length(input$name1) == 0) {
      print("Selecione pelo menos uma zona")
    } else {
      df_trend1 <- ideal[ideal$Zona.de.produção %in% input$name1, ]


    ay <- list(
          overlaying = "y",
          side = "lefth",
          title = "Causadoras de DSP (células/l)",
          size = 14
          )


          p <- plot_ly() %>%
          
          add_lines(as.Date(df_trend1$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df_trend1$Produtoras.de.DSP),by = df_trend1$Zona.de.produção, name = df_trend1$Zona.de.produção, yaxis = "y") %>%
          layout(
          title = "", yaxis = ay,
          xaxis = list(title="Data"),
          legend=list(orientation = 'h',y=20)  
        ) %>%
          add_trace(name = 'trace 1', mode = 'lines+markers')  

        

    }

    SAVEPLOT_F<<-p

  })

  output$contents2 <- DT::renderDataTable({
      df_trend1 <- ideal[ideal$Zona.de.produção %in% input$name1, ]
      DT::datatable(df_trend1, options = list(pageLength = 7, language= list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese.json')))
  })




output$CapitaniaTS  <- renderPlotly({
      df_trend <- df()[df()$Zona.de.produção == input$show_vars_zonas, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$show_especie, ]

        

    
  # if(input$smoother){

  #           cnt_ma = ma(df_trend3$Toxinas.Lipofílicas, order=input$f1) 


  #         p<- ggplot(data=df_trend3, aes(x = as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df_trend3$Toxinas.Lipofílicas))) + 
  #          geom_line() + xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") + geom_line(aes(y = cnt_ma), color = "red") + 
  #          geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'DeepSkyBlue') + 
  #          scale_linetype_manual(name = " ", values = c(1), guide = guide_legend(override.aes = list(color = c("DeepSkyBlue")))) + theme_classic() + theme(axis.title.x = element_text(vjust=-5) , plot.margin = (unit(c(.5, .5, 2, .5), "cm")))
  #         print(p)
  #          plotreport1<<-recordPlot()
  #         ggplotly(p)  %>%
  #         layout(
          
  #         legend=list(orientation = 'h',y=20) 
  #       ) 
         
  

  #         }else{
           

           p<- ggplot(data=df_trend3, aes(x = as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df_trend3$Toxinas.Lipofílicas))) +
            geom_line() + geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'SteelBlue') + xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") +
            scale_linetype_manual(name = "", values = c(1), guide = guide_legend(override.aes = list(color = c("DodgerBlue")))) + theme_classic() + theme(axis.title.x = element_text(vjust=-5) , plot.margin = (unit(c(.5, .5, 2, .5), "cm")))
         print(p)
           plotreport1<<-recordPlot()
           ggplotly(p)  %>%
          layout(
          
          legend=list(orientation = 'h',y=20) 
        ) 

          # }


  })

output$CapitaniaTS_ANN  <- renderPlotly({
      df_trend <- df()[df()$Zona.de.produção == input$Zonas_ANN, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$Especie_ANN, ]


       # if(input$smoother_ANN){

       #      cnt_ma = ma(df_trend3$Toxinas.Lipofílicas, order=input$f2) 

       #   p<- ggplot(data=df_trend3, aes(x = as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df_trend3$Toxinas.Lipofílicas))) + 
       #     geom_line() + xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") + geom_line(aes( y = cnt_ma), color = "red") + 
       #     geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'DeepSkyBlue') + 
       #     scale_linetype_manual(name = " ", values = c(1), guide = guide_legend(override.aes = list(color = c("DeepSkyBlue")))) + theme_classic() + theme(axis.title.x = element_text(vjust=-5) , plot.margin = (unit(c(.5, .5, 2, .5), "cm")))
          
       #    print(p)
       #     plotreport1_ANN<<-recordPlot()

           

       #    ggplotly(p)  %>%
       #    layout(
          
       #    legend=list(orientation = 'h',y=20) 
       #  ) 




       #    }else{

               p<- ggplot(data=df_trend3, aes(x = as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df_trend3$Toxinas.Lipofílicas))) +
            geom_line() + geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'SteelBlue') + xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") +
            scale_linetype_manual(name = "", values = c(1), guide = guide_legend(override.aes = list(color = c("DodgerBlue")))) + theme_classic() + theme(axis.title.x = element_text(vjust=-5) , plot.margin = (unit(c(.5, .5, 2, .5), "cm")))


           print(p)
           plotreport1_ANN<<-recordPlot()

           ggplotly(p)   %>%
          layout(
          
          legend=list(orientation = 'h',y=20) ) 
             #

          # }

  })


#==============================================================================================================================================
output$plotForecastingTESTE  <- renderPlot({ 

	  df_trend <- df()[df()$Zona.de.produção == input$show_vars_zonas, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$show_especie, ] 
      Data <-  as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y")
      ultimaData <- max(Data)

      NovaData <- ultimaData + 7
     for(i in 1:3){
       NovaData <- c(NovaData,max(as.Date(NovaData))+7)
      }

      if(input$modelos=="ARIMA"){

      	Bio_ts <- ts(df_trend3[, c('Toxinas.Lipofílicas')])

      # 	 if(input$smoother){
      #    Bio_ts = ma(Bio_ts, order=input$f1)
      #    Bio_ts <- na.omit(Bio_ts)
      #    showNotification("Serie foi suavizada!",type="message")
      # }

      if(input$ajustar=="Manual"){
       fit <- arima(Bio_ts,order= c(input$numP,input$diff,input$numQ))
      }else{
        #Parametros ajustados automaticamente
       fit <- auto.arima(Bio_ts,seasonal = isSeasonal(Bio_ts))
       print("-------------------DATA---------")
       print(input$show_vars_zonas)
       print(input$show_especie)
       print(fit)
      }
      

     

     #SAVEPLOT_FItted <<-qq

     output$plotForecastingFITTED  <- renderPlot({  
        qq<- ggplot(df_trend3,aes(x=as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y"),y=as.numeric(df_trend3$Toxinas.Lipofílicas),colour="Dados originais"))+geom_line() +
      xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)")+ 
      geom_line(aes(y=fit$fitted, linetype = "Dados ajustados"), colour= 'DeepSkyBlue') + 
      scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue"))))+labs(color=" ") + 
      theme_classic() + theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
      print(qq)
       
	})


    #CRIAR CONJUNTO DE TESTE (será usado no Accuracy)
   dataTeste <- Bio_ts[(length(Bio_ts)-3):length(Bio_ts)]
   dataTrain <- Bio_ts[1:(length(Bio_ts)-4)]
   order =fit$arma

   fitTeste <- Arima(dataTrain,order = c(order[1],order[6],order[2]),seasonal = list(order = c(order[3], order[7], order[4]),period = order[5]))
   fcastTeste <- forecast(fitTeste,h=4)
   accuracyTeste<- accuracy(fcastTeste,dataTeste)


   AccuracyModel <<-accuracyTeste

#MUDAR 20 PARA 0
      # df_trend <- ideal3[ideal3$Zona.de.produção == input$show_vars_zonas, ]
      # df_trend3 <- df_trend[df_trend$Espécie %in% input$show_especie, ] 



    fcast <- forecast(fit, h=4)
    ts2 <- c(ts(df_trend3[, c('Toxinas.Lipofílicas')]),fcast$mean)
    time <- c(Data,NovaData)
    var1 <- ts2
    DF <- data.frame(time, var1)
    p <- ggplot(DF, aes(time, var1, colour=(time>=ultimaData)))+ xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") + 
    
    geom_line(aes(group=1))+
    # geom_line(df_trend3,aes(x=as.Date(Data.da.colheita,"%d/%m/%Y"),y = fit$fitted)) + 
    geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'NavyBlue')+ 
    scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue"))))+ labs(color=" ") +
    scale_color_manual(labels = c("Original", "Previsão"), values = c("blue", "red")) + theme_classic() +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
    print(p)

   
    #+ geom_line(aes(y=fit$fitted, linetype = "Limite regulamentar"), colour= 'DeepSkyBlue')

   #ggsave("myplot.png", plot = p)
  

    # TESTES PARA QUALIDADE DO MODELO
    shapiro<- shapiro.test(residuals(fit)) #teste Normalidade
    BoxPierce <- Box.test(fit$residuals, lag = 1, type = c("Box-Pierce")) #Teste independencia dos residuos
    BoxLjung <- Box.test(fit$residuals, lag = 1, type = c("Ljung-Box")) #Teste independencia dos residuos
    #accuracy <- accuracy(fit)

    print(input$show_vars_zonas)
    print(input$show_especie)
    print(accuracyTeste)

   output$txtoutARIMA <- renderText({
      paste(" AIC:",fit$aic,"| BIC:",fit$bic,"\n",shapiro$method,"| w:",shapiro$statistic ,"P-value: ",shapiro$p.value,"\n",
       BoxPierce$method, "| X-squared:", BoxPierce$statistic, "P-value: ", BoxPierce$p.value , "\n", 
       BoxLjung$method, "| X-squared:", BoxLjung$statistic, "P-value: ", BoxLjung$p.value, "\n", "\n", 
       "Medidas de precisão para o modelo de previsão:" ,"\n",
       "Conjunto de treino |" ,

       "ME:", accuracyTeste[1,1], "RMSE:",accuracyTeste[1,2],"MAE:",accuracyTeste[1,3],"\n","MPE:",accuracyTeste[1,4],"MAPE:",accuracyTeste[1,5],"\n","\n",

       "Conjunto de teste |",
        "ME:", accuracyTeste[2,1], "RMSE:",accuracyTeste[2,2],"MAE:",accuracyTeste[2,3],"\n","MPE:",accuracyTeste[2,4],"MAPE:",accuracyTeste[2,5]
        ,sep = " ")
    })

  output$plotForecastingTESTEResiduals  <- renderPlot({ 
    tsdisplay(residuals(fit), main='Resíduos')
   })

   }

    #===================================================================================

          
		  if(input$modelos == "VAR"){
		  	data <- cbind(as.numeric(df2()$Toxinas.Lipofílicas), as.numeric(df2()$Produtoras.de.DSP))

       #  if(input$show_vars_Capitania=="Aveiro" && input$show_vars_zonas=="RIAV1" && input$show_especie=="Mexilhão"){
       #     write.table(data, "tabelaBIoAnalaise.csv")}
		  	  # #MUDAR 20 PARA 0
		  	df_trend <- df()[df()$Zona.de.produção == input$show_vars_zonas, ]
            df_trend3 <- df_trend[df_trend$Espécie %in% input$show_especie, ] 

            var_select <-0

		  	if(input$ajustar=="Manual"){

            var <- VAR(data,input$numPVAR)

             pred <- VARpred(var,orig = 0, h = 4)
             ts2 <- c(df_trend3$Toxinas.Lipofílicas,pred$pred[,1])
             


          #CRIAR CONJUNTO DE TESTE (será usado no Accuracy)
         dataTeste <- cbind(as.numeric(df2()$Toxinas.Lipofílicas[(length(df2()$Toxinas.Lipofílicas)-3):length(df2()$Toxinas.Lipofílicas)]), 
         as.numeric(df2()$Produtoras.de.DSP[(length(df2()$Produtoras.de.DSP)-3):length(df2()$Produtoras.de.DSP)]))
         dataTrain <- cbind(as.numeric(df2()$Toxinas.Lipofílicas[1:(length(df2()$Toxinas.Lipofílicas)-4)]), as.numeric(df2()$Produtoras.de.DSP[1:(length(df2()$Produtoras.de.DSP)-4)]))
          
         
           fitTrainAll <-vars::VAR(dataTrain,input$numPVAR)
           fitted_var <-fitted(fitTrainAll)[,1]

           

           fitTeste <-VAR(dataTrain,input$numPVAR)
           pred <- VARpred(fitTeste,orig = 0, h = 4)

           

          }else{
	       #Parametro ajustado automaticamente
	         var_select <- VARselect(data)
             var <- MTS::VAR(data,min(var_select$selection))
             

             pred <- VARpred(var,orig = 0, h = 4)
             +
             ts2 <- c(df_trend3$Toxinas.Lipofílicas,pred$pred[,1])
            

           fitAll <-vars::VAR(data,min(var_select$selection))
           fittedVAR <-fitted(fitAll)[,1]

         
 


          #CRIAR CONJUNTO DE TESTE (será usado no Accuracy)
         dataTeste <- cbind(as.numeric(df2()$Toxinas.Lipofílicas[(length(df2()$Toxinas.Lipofílicas)-3):length(df2()$Toxinas.Lipofílicas)]), 
         as.numeric(df2()$Produtoras.de.DSP[(length(df2()$Produtoras.de.DSP)-3):length(df2()$Produtoras.de.DSP)]))
         dataTrain <- cbind(as.numeric(df2()$Toxinas.Lipofílicas[1:(length(df2()$Toxinas.Lipofílicas)-4)]), as.numeric(df2()$Produtoras.de.DSP[1:(length(df2()$Produtoras.de.DSP)-4)]))
          
         
           fitTrainAll <-vars::VAR(dataTrain,min(var_select$selection))
           fitted_var <-fitted(fitTrainAll)[,1]

           fitTeste <-MTS::VAR(dataTrain,min(var_select$selection))
           pred <- VARpred(fitTeste,orig = 0, h = 4)
            
	       }
            
           

            SIZEDATA <-length(as.Date(df2()$Data.da.colheita,"%d/%m/%Y"))
            DATAFITTED <- df2()[1:(SIZEDATA-1),]

      output$plotForecastingFITTED  <- renderPlot({     
			        qq<- ggplot(DATAFITTED,aes(x=as.Date(DATAFITTED$Data.da.colheita,"%d/%m/%Y"),y=as.numeric(DATAFITTED$Toxinas.Lipofílicas),colour="Dados originais"))+geom_line() +
			        xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") + 
			        geom_line(aes(y=fittedVAR, linetype = "Dados ajustados"), colour= 'DeepSkyBlue') + 
			      	scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue"))))+labs(color=" ") + 
			      	theme_classic() + theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
			      	qq
			          
           })

          accuracyTesteFitted <-accuracy(dataTrain[,1],fitted_var)
          accuracyTeste<- accuracy(pred$pred[,1],dataTeste[,1])

          AccuracyModel <<-accuracyTesteFitted

        


		    time <- c(Data,NovaData)
		    var1 <- ts2
		    DF <- data.frame(time, var1)
		   p<- ggplot(DF, aes(time, var1, colour=(time>=ultimaData))) + xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") + geom_line(aes(group=1))+ geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'NavyBlue')+ 
           scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue"))))+labs(color=" ") + scale_color_manual(labels = c("Original", "Previsão"), values = c("blue", "red"))  + theme_classic() +
       theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
		   print(p)

       # TESTES PARA QUALIDADE DO MODELO
       shapiro<- shapiro.test(var$residuals[,1]) #teste Normalidade
       BoxPierce <- Box.test(var$residuals[,1], lag = 1, type = c("Box-Pierce")) #Teste independencia dos residuos
       BoxLjung <- Box.test(var$residuals[,1], lag = 1, type = c("Ljung-Box")) #Teste independencia dos residuos
       #accuracy <- accuracy(var)


        output$txtoutVAR <- renderText({
               paste(" AIC:",var$aic,"| BIC:",var$bic,"\n",shapiro$method,"| w:",shapiro$statistic ,"P-value: ",shapiro$p.value,"\n",
               BoxPierce$method, "| X-squared:", BoxPierce$statistic, "P-value: ", BoxPierce$p.value , "\n",
                BoxLjung$method, "| X-squared:", BoxLjung$statistic, "P-value: ", BoxLjung$p.value,"\n", "\n", 
               "Medidas de precisão para o modelo de previsão:" ,"\n",
       "Conjunto de treino |" ,

       "ME:", accuracyTesteFitted[1], "RMSE:",accuracyTesteFitted[2],"MAE:",accuracyTesteFitted[3],"\n","MPE:",accuracyTesteFitted[4],"MAPE:",accuracyTesteFitted[5],"\n","\n",

       "Conjunto de teste |",
        "ME:", accuracyTeste[1], "RMSE:",accuracyTeste[2],"MAE:",accuracyTeste[3],"\n","MPE:",accuracyTeste[4],"MAPE:",accuracyTeste[5]

               ,sep = " ")
            })

          output$plotForecastingTESTEResidualsVAR  <- renderPlot({ 
            tsdisplay(var$residuals[,1], main='Resíduos')

           })

   	}


		   if(input$modelos == "VARMA"){

		   	#Ljung-Box statistics.  SELECIONAR O MODELO USANDO ECCM
		   	data <- cbind(as.numeric(df2()$Toxinas.Lipofílicas), as.numeric(df2()$Produtoras.de.DSP))
		   	df_trend <- df()[df()$Zona.de.produção == input$show_vars_zonas, ]
        df_trend3 <- df_trend[df_trend$Espécie %in% input$show_especie, ] 
		   	Bestp=0
			Bestq=0

            #Selecionar os melhores paramentros (p e q)
            if(input$ajustar=="Manual"){
                	Bestp=input$numPVARMA
                	Bestq=input$numQVARMA
            	}else{

			 SelectVarma=Eccm(data)
						SizeSelectVarma <- dim(SelectVarma$pEccm)
						min=10
						

							for(i in 1:SizeSelectVarma[1]){
							    for(j in 1:SizeSelectVarma[2]){
							        
							        if(SelectVarma$pEccm[i,j]>=0.05){
							            sum<-i+j
							            if(sum<min){
							                min<-sum
							                Bestp=i-1
							                Bestq=j-1
							            }
							        }
							    }
							}

            	}
           
 
            
         #CRIAR CONJUNTO DE TESTE (será usado no Accuracy)
         dataTeste <- cbind(as.numeric(df2()$Toxinas.Lipofílicas[(length(df2()$Toxinas.Lipofílicas)-3):length(df2()$Toxinas.Lipofílicas)]), 
         as.numeric(df2()$Produtoras.de.DSP[(length(df2()$Produtoras.de.DSP)-3):length(df2()$Produtoras.de.DSP)]))
         dataTrain <- cbind(as.numeric(df2()$Toxinas.Lipofílicas[1:(length(df2()$Toxinas.Lipofílicas)-4)]), as.numeric(df2()$Produtoras.de.DSP[1:(length(df2()$Produtoras.de.DSP)-4)]))
        
         fitTeste <-VARMA(dataTrain, p=Bestp,q=Bestq)
         pred <- VARMApred(fitTeste,orig = 0, h = 4)
         accuracyTeste<- accuracy(pred$pred[,1],dataTeste[,1])
         AccuracyModel <<-accuracyTeste
  





             
             varma <- VARMA(data, p=Bestp,q=Bestq)

             pred <- VARMApred(varma,orig = 0, h = 4)
		     ts2 <- c(df_trend3$Toxinas.Lipofílicas,pred$pred[,1])
		     
		    time <- c(Data,NovaData)
		    varma1 <- ts2
		    DF <- data.frame(time, varma1)
		  
		          p<-ggplot(DF, aes(time, varma1, colour=(time>=ultimaData))) + xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") + geom_line(aes(group=1))+ geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'NavyBlue')+ 
        		 scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue"))))+labs(color=" ") + scale_color_manual(labels = c("Original", "Previsão"), values = c("blue", "red"))  + theme_classic() +
       			 theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
		   	     print(p)

		           
          
  
       #TESTES PARA QUALIDADE DO MODELO
       shapiro<- shapiro.test(varma$residuals[,1]) #teste Normalidade
       BoxPierce <- Box.test(varma$residuals[,1], lag = 1, type = c("Box-Pierce")) #Teste independencia dos residuos
       BoxLjung <- Box.test(varma$residuals[,1], lag = 1, type = c("Ljung-Box")) #Teste independencia dos residuos




        output$txtoutVARMA <- renderText({
              
                paste(" AIC:",varma$aic,"| BIC:",varma$bic,"\n",shapiro$method,"| w:",shapiro$statistic ,"P-value: ",shapiro$p.value,"\n",
                BoxPierce$method, "| X-squared:", BoxPierce$statistic, "P-value: ", BoxPierce$p.value , "\n",
                BoxLjung$method, "| X-squared:", BoxLjung$statistic, "P-value: ", BoxLjung$p.value,"\n", "\n", 
               "Medidas de precisão para o modelo de previsão:" ,"\n",
               "Conjunto de teste |" ,

                 "ME:", accuracyTeste[1], "RMSE:",accuracyTeste[2],"MAE:",accuracyTeste[3],"\n","MPE:",accuracyTeste[4],"MAPE:",accuracyTeste[5]

               ,sep = " ")
            })

          output$plotForecastingTESTEResidualsVARMA  <- renderPlot({ 
            tsdisplay(varma$residuals[,1], main='Resíduos')
            
           })
            
		   }


   plotreport<<-recordPlot()
    SAVEPLOT <<-p
    




 })

output$plotForecastingFito  <- renderPlotly({
  
 
    
       p<-ggplot(data=df2(), aes(x = as.Date(df2()$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df2()$Produtoras.de.DSP)), ann = FALSE) + geom_line() +
         xlab("Data") + ylab("Causadoras de DSP (células/l)") + theme_classic() + theme(axis.title.x = element_text(vjust=-5) , plot.margin = (unit(c(.5, .5, 2, .5), "cm"))) 

     print(p)
     plotreport2<<-recordPlot()
       
           ggplotly(p) 
      
  
  })


output$plotForecastingFito_ANN  <- renderPlotly({
    
      p<- ggplot(data=df3(), aes(x = as.Date(df3()$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df3()$Produtoras.de.DSP)), ann = FALSE) + geom_line() +
         xlab("Data") + ylab("Causadoras de DSP (células/l)") + theme_classic() + theme(axis.title.x = element_text(vjust=-5) , plot.margin = (unit(c(.5, .5, 2, .5), "cm"))) 

      print(p)
     plotreport2_ANN<<-recordPlot()
       
    ggplotly(p)    

  
  })



output$plotForecastingBESTMODEL <-renderPlot({ 

      df_trend <- df()[df()$Zona.de.produção == input$Zonas_BESTMODEL, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$Especie_BESTMODEL, ] 
      Data <-  as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y")
      ultimaData <- max(Data)

      NovaData <- ultimaData + 7
     for(i in 1:3){
       NovaData <- c(NovaData,max(as.Date(NovaData))+7)
      }

     


     
#===========================================================================================================================================ARIMA

        Bio_ts <- ts(df_trend3[, c('Toxinas.Lipofílicas')])

       fit <- auto.arima(Bio_ts,seasonal = isSeasonal(Bio_ts))
      


    #CRIAR CONJUNTO DE TESTE (será usado no Accuracy)
   dataTeste <- Bio_ts[(length(Bio_ts)-3):length(Bio_ts)]
   dataTrain <- Bio_ts[1:(length(Bio_ts)-4)]
   order =fit$arma

   fitTeste <- Arima(dataTrain,order = c(order[1],order[6],order[2]),seasonal = list(order = c(order[3], order[7], order[4]),period = order[5]))
   fcastTeste <- forecast(fitTeste,h=4)
   accuracyARIMA<- accuracy(fcastTeste,dataTeste)
   residualsARIMA<-residuals(fit)
   

    fcast <- forecast(fit, h=4)
    ts2 <- c(ts(df_trend3[, c('Toxinas.Lipofílicas')]),fcast$mean)
    time <- c(Data,NovaData)
    var1 <- ts2
    DF <- data.frame(time, var1)
    pARIMA <- ggplot(DF, aes(time, var1, colour=(time>=ultimaData)))+ xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") + 
    geom_line(aes(group=1))+
    geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'NavyBlue')+ 
    scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue"))))+ labs(color=" ") +
    scale_color_manual(labels = c("Original", "Previsão"), values = c("blue", "red")) + theme_classic() +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
    
    # print(pARIMA)
    # pARIMA<-recordPlot()

    # TESTES PARA QUALIDADE DO MODELO
    shapiro<- shapiro.test(residuals(fit)) #teste Normalidade
    BoxPierce <- Box.test(fit$residuals, lag = 1, type = c("Box-Pierce")) #Teste independencia dos residuos
    BoxLjung <- Box.test(fit$residuals, lag = 1, type = c("Ljung-Box")) #Teste independencia dos residuos
    #accuracy <- accuracy(fit)

   
      # paste(" AIC:",fit$aic,"| BIC:",fit$bic,"\n",shapiro$method,"| w:",shapiro$statistic ,"P-value: ",shapiro$p.value,"\n",
      #  BoxPierce$method, "| X-squared:", BoxPierce$statistic, "P-value: ", BoxPierce$p.value , "\n", 
      #  BoxLjung$method, "| X-squared:", BoxLjung$statistic, "P-value: ", BoxLjung$p.value, "\n", "\n", 
      #  "Medidas de precisão para o modelo de previsão:" ,"\n",
      #  "Conjunto de treino |" ,

      #  "ME:", accuracyTeste[1,1], "RMSE:",accuracyTeste[1,2],"MAE:",accuracyTeste[1,3],"\n","MPE:",accuracyTeste[1,4],"MAPE:",accuracyTeste[1,5],"\n","\n",

      #  "Conjunto de teste |",
      #   "ME:", accuracyTeste[2,1], "RMSE:",accuracyTeste[2,2],"MAE:",accuracyTeste[2,3],"\n","MPE:",accuracyTeste[2,4],"MAPE:",accuracyTeste[2,5]
      #   ,sep = " ")
    
   


     
#===========================================================================================================================================VAR

        data <- cbind(as.numeric(df4()$Toxinas.Lipofílicas), as.numeric(df4()$Produtoras.de.DSP))
          #MUDAR 20 PARA 0
            df_trend <- df()[df()$Zona.de.produção == input$Zonas_BESTMODEL, ]
            df_trend3 <- df_trend[df_trend$Espécie %in% input$Especie_BESTMODEL, ] 

            var_select <-0

       
         #Parametro ajustado automaticamente
           var_select <- VARselect(data)
             var <- VAR(data,min(var_select$selection))
             residualsVAR <- var$residuals[,1]

             pred <- VARpred(var,orig = 0, h = 4)
             ts2 <- c(df_trend3$Toxinas.Lipofílicas,pred$pred[,1])

           fitAll <-vars::VAR(data,min(var_select$selection))
           fittedVAR <-fitted(fitAll)[,1]
 


          #CRIAR CONJUNTO DE TESTE (será usado no Accuracy)
         dataTeste <- cbind(as.numeric(df4()$Toxinas.Lipofílicas[(length(df4()$Toxinas.Lipofílicas)-3):length(df4()$Toxinas.Lipofílicas)]), 
         as.numeric(df4()$Produtoras.de.DSP[(length(df4()$Produtoras.de.DSP)-3):length(df4()$Produtoras.de.DSP)]))
         dataTrain <- cbind(as.numeric(df4()$Toxinas.Lipofílicas[1:(length(df4()$Toxinas.Lipofílicas)-4)]), as.numeric(df4()$Produtoras.de.DSP[1:(length(df4()$Produtoras.de.DSP)-4)]))
          
         
           fitTrainAll <-vars::VAR(dataTrain,min(var_select$selection))
           fitted_var <-fitted(fitTrainAll)[,1]

           fitTeste <-VAR(dataTrain,min(var_select$selection))
           pred <- VARpred(fitTeste,orig = 0, h = 4)
            
          accuracyTesteFitted <-accuracy(dataTrain[,1],fitted_var)
          accuracyVAR<- accuracy(pred$pred[,1],dataTeste[,1])
        

        time <- c(Data,NovaData)
        var1 <- ts2
        DF <- data.frame(time, var1)
       pVAR<- ggplot(DF, aes(time, var1, colour=(time>=ultimaData))) + xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") + geom_line(aes(group=1))+ geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'NavyBlue')+ 
           scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue"))))+labs(color=" ") + scale_color_manual(labels = c("Original", "Previsão"), values = c("blue", "red"))  + theme_classic() +
       theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
       # print(pVAR)
       # pVAR<-recordPlot()

  
       # TESTES PARA QUALIDADE DO MODELO
       shapiro<- shapiro.test(var$residuals[,1]) #teste Normalidade
       BoxPierce <- Box.test(var$residuals[,1], lag = 1, type = c("Box-Pierce")) #Teste independencia dos residuos
       BoxLjung <- Box.test(var$residuals[,1], lag = 1, type = c("Ljung-Box")) #Teste independencia dos residuos
       #accuracy <- accuracy(var)


       
       #         paste(" AIC:",var$aic,"| BIC:",var$bic,"\n",shapiro$method,"| w:",shapiro$statistic ,"P-value: ",shapiro$p.value,"\n",
       #         BoxPierce$method, "| X-squared:", BoxPierce$statistic, "P-value: ", BoxPierce$p.value , "\n",
       #          BoxLjung$method, "| X-squared:", BoxLjung$statistic, "P-value: ", BoxLjung$p.value,"\n", "\n", 
       #         "Medidas de precisão para o modelo de previsão:" ,"\n",
       # "Conjunto de treino |" ,

       # "ME:", accuracyTesteFitted[1], "RMSE:",accuracyTesteFitted[2],"MAE:",accuracyTesteFitted[3],"\n","MPE:",accuracyTesteFitted[4],"MAPE:",accuracyTesteFitted[5],"\n","\n",

       # "Conjunto de teste |",
       #  "ME:", accuracyTeste[1], "RMSE:",accuracyTeste[2],"MAE:",accuracyTeste[3],"\n","MPE:",accuracyTeste[4],"MAPE:",accuracyTeste[5]

       #         ,sep = " ")
          
#===========================================================================================================================================VARMA


     

            #Ljung-Box statistics.  SELECIONAR O MODELO USANDO ECCM
            data <- cbind(as.numeric(df4()$Toxinas.Lipofílicas), as.numeric(df4()$Produtoras.de.DSP))
            df_trend <- df()[df()$Zona.de.produção == input$Zonas_BESTMODEL, ]
            df_trend3 <- df_trend[df_trend$Espécie %in% input$Especie_BESTMODEL, ] 
            Bestp=0
            Bestq=0

         SelectVarma=Eccm(data)
            SizeSelectVarma <- dim(SelectVarma$pEccm)
            min=10
            

              for(i in 1:SizeSelectVarma[1]){
                  for(j in 1:SizeSelectVarma[2]){
                      
                      if(SelectVarma$pEccm[i,j]>=0.05){
                          sum<-i+j
                          if(sum<min){
                              min<-sum
                              Bestp=i-1
                              Bestq=j-1
                          }
                      }
                  }
              }

              
           
 
            
         #CRIAR CONJUNTO DE TESTE (será usado no Accuracy)
         dataTeste <- cbind(as.numeric(df4()$Toxinas.Lipofílicas[(length(df4()$Toxinas.Lipofílicas)-3):length(df4()$Toxinas.Lipofílicas)]), 
         as.numeric(df4()$Produtoras.de.DSP[(length(df4()$Produtoras.de.DSP)-3):length(df4()$Produtoras.de.DSP)]))
         dataTrain <- cbind(as.numeric(df4()$Toxinas.Lipofílicas[1:(length(df4()$Toxinas.Lipofílicas)-4)]), as.numeric(df4()$Produtoras.de.DSP[1:(length(df4()$Produtoras.de.DSP)-4)]))
        
         fitTeste <-VARMA(dataTrain, p=Bestp,q=Bestq)
         pred <- VARMApred(fitTeste,orig = 0, h = 4)
         accuracyVARMA<- accuracy(pred$pred[,1],dataTeste[,1])
         varma <- VARMA(data, p=Bestp,q=Bestq)

         residualsVARMA<-varma$residuals[,1]

         pred <- VARMApred(varma,orig = 0, h = 4)
         ts2 <- c(df_trend3$Toxinas.Lipofílicas,pred$pred[,1])
          

        time <- c(Data,NovaData)
        varma1 <- ts2
        DF <- data.frame(time, varma1)
      
              pVARMA<-ggplot(DF, aes(time, varma1, colour=(time>=ultimaData))) + xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") + geom_line(aes(group=1))+ geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'NavyBlue')+ 
             scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue"))))+labs(color=" ") + scale_color_manual(labels = c("Original", "Previsão"), values = c("blue", "red"))  + theme_classic() +
             theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
             # print(pVARMA)
             # pVARMA<-recordPlot()

       #TESTES PARA QUALIDADE DO MODELO
       shapiro<- shapiro.test(varma$residuals[,1]) #teste Normalidade
       BoxPierce <- Box.test(varma$residuals[,1], lag = 1, type = c("Box-Pierce")) #Teste independencia dos residuos
       BoxLjung <- Box.test(varma$residuals[,1], lag = 1, type = c("Ljung-Box")) #Teste independencia dos residuos



#===================================================SELECÇÃO DO MELHOR===========================================

          WAR<-0
          WVAR<-0
          WVARMA<-0
          BESTMODEL<-0

          for(i in 1:5){
              if(abs(accuracyARIMA[2,i])<=abs(accuracyVAR[i])){
                  WAR<-WAR+1
              }else{
                WVAR<-WVAR+1
              }
          }

          if(WAR>=WVAR){
            BESTMODEL<-WAR
            MODEL<-"ARIMA"
          }else{
            BESTMODEL<-WVAR
            MODEL<-"VAR"
          }
          
         if(MODEL=="ARIMA"){
            WAR<-0
            WVARMA<-0

              for(i in 1:5){
              if(abs(accuracyARIMA[2,i])<=abs(accuracyVARMA[i])){
                  WAR<-WAR+1
              }else{
                WVARMA<-WVARMA+1
              }
          }

          if(WAR>=WVARMA){
            BESTMODEL<-WAR
            MODEL<-"ARIMA"
          }else{
            BESTMODEL<-WVARMA
            MODEL<-"VARMA"
          }

         }else{
            WVAR<-0
            WVARMA<-0
            
            for(i in 1:5){
              if(abs(accuracyVAR[i])<=abs(accuracyVARMA[i])){
                  WVAR<-WVAR+1
              }else{
                WVARMA<-WVARMA+1
              }
          }

          if(WVAR>=WVARMA){
            BESTMODEL<-WVAR
            MODEL<-"VAR"
          }else{
            BESTMODEL<-WVARMA
            MODEL<-"VARMA"
          }

         }
 

     output$txtoutBESTMODEL<- renderText({
                paste(
                 "Conjunto de teste ARIMA |" ,

                 "ME:", accuracyARIMA[2,1], "RMSE:",accuracyARIMA[2,2],"MAE:",accuracyARIMA[2,3],"\n","MPE:",accuracyARIMA[2,4],"MAPE:",accuracyARIMA[2,5],"\n", "\n", 


                 "Conjunto de teste VAR |" ,

                 "ME:", accuracyVAR[1], "RMSE:",accuracyVAR[2],"MAE:",accuracyVAR[3],"\n","MPE:",accuracyVAR[4],"MAPE:",accuracyVAR[5],"\n", "\n", 

               "Conjunto de teste VARMA |" ,

                 "ME:", accuracyVARMA[1], "RMSE:",accuracyVARMA[2],"MAE:",accuracyVARMA[3],"\n","MPE:",accuracyVARMA[4],"MAPE:",accuracyVARMA[5],"\n", "\n", 

                 "MELHOR MODELO:",MODEL,"PESO:",BESTMODEL

               ,sep = " ")
         })



     if(MODEL =="ARIMA"){
       print(pARIMA)
     

     }
      if(MODEL =="VAR"){
      print(pVAR)
      
      #print(p)
    }

    if(MODEL =="VARMA"){
      print(pVARMA)
      
    }
  


   output$plotForecastingBESTMODELRESIDUAL<- renderPlot({

    if(MODEL =="ARIMA"){
       pResidual<-tsdisplay(residualsARIMA, main='Resíduos')
       pResidual

     }

      if(MODEL =="VAR"){
      pResidual<-tsdisplay(residualsVAR, main='Resíduos')
      pResidual
     
    }

    if(MODEL =="VARMA"){
      pResidual<-tsdisplay(residualsVARMA, main='Resíduos')
      pResidual

     }

               
  })
            
   
   

   

  })

#==============================================================================================================================================




output$plotForecastingBESTMODEL_ANN <-renderPlot({




      df_trend <- df()[df()$Zona.de.produção == input$Zonas_BESTMODEL, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$Especie_BESTMODEL, ] 
      Data <-  as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y")
      ultimaData <- max(Data)

     NovaData <- ultimaData + 7
     for(i in 1:3){
       NovaData <- c(NovaData,max(as.Date(NovaData))+7)
      }


lag_transform <- function(x, k= 1){
    
      lagged =  c(rep(NA, k), x[1:(length(x)-k)])
      DF = as.data.frame(cbind(lagged, x))
      colnames(DF) <- c( paste0('x-', k), 'x')
      DF[is.na(DF)] <- 0
      return(DF)
}
scale_data = function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  
}

invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for( i in 1:t){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}

#================================================================================ANN==============================================================
        Bio_ts <- ts(df_trend3[, c('Toxinas.Lipofílicas')])

  #CRIAR CONJUNTO DE TESTE (será usado no Accuracy)
   dataTeste <- Bio_ts[(length(Bio_ts)-3):length(Bio_ts)]
   dataTrain <- Bio_ts[1:(length(Bio_ts)-4)]

  #  if(input$ajustar1=="Manual"){  
  #  fitTeste <- nnetar(dataTrain,p=input$numPNNAR)
  
  # }else{
      fitTeste <- nnetar(dataTrain)
  # }

   fcastTeste <- forecast(fitTeste,h=4)
   accuracyTeste<- accuracy(fcastTeste,dataTeste)
   accuracyNNAR <<- accuracyTeste


 

 # if(input$ajustar1=="Manual"){  
 #  fit <- nnetar(Bio_ts,p=input$numPNNAR)
 #  }else{
      fit <- nnetar(Bio_ts)
  # }

    residualsNNAR<-residuals(fit)
 

  #  output$plotForecastingFITTED_ANN  <- renderPlot({  
  #       qq<- ggplot(df_trend3,aes(x=as.Date(df_trend3$Data.da.colheita,"%d/%m/%Y"),y=as.numeric(df_trend3$Toxinas.Lipofílicas),colour="Dados originais"))+geom_line() +
  #     xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)")+ 
  #     geom_line(aes(y=fit$fitted, linetype = "Dados ajustados"), colour= 'DeepSkyBlue') + 
  #     scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue"))))+labs(color=" ") + 
  #     theme_classic() + theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
  #     qq
       
  # })



    
    fcast <- forecast(fit, h=4)

    #Mudar 20 Para 0
      df_trend <- df()[df()$Zona.de.produção == input$Zonas_BESTMODEL, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$Especie_BESTMODEL, ] 




    Bio_ts <- ts(df_trend3[, c('Toxinas.Lipofílicas')])
    ts2 <- c(Bio_ts,fcast$mean)

    time <- c(Data,NovaData)
    var1 <- ts2
    DF <- data.frame(time, var1)
    pNNAR <- ggplot(DF, aes(time, var1, colour=(time>=ultimaData)))+ xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") + geom_line(aes(group=1))+ geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'NavyBlue')+ 
    scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue")))) +labs(color=" ") + scale_color_manual(labels = c("Original", "Previsão"), values = c("blue", "red")) + theme_classic() +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
    # print(pNNAR)
    # pNNAR<-recordPlot()
    

     # TESTES PARA QUALIDADE DO MODELO
    shapiro<- shapiro.test(residuals(fit)) #teste Normalidade
    BoxPierce <- Box.test(fit$residuals, lag = 1, type = c("Box-Pierce")) #Teste independencia dos residuos
    BoxLjung <- Box.test(fit$residuals, lag = 1, type = c("Ljung-Box")) #Teste independencia dos residuos


    # output$txtoutBESTMODEL_ANN <- renderText({
    #   paste("Shapiro-Wilk teste de normalidade","| w:",shapiro$statistic ,"P-value: ",shapiro$p.value,"\n",
    #    BoxPierce$method, "| X-squared:", BoxPierce$statistic, "P-value: ", BoxPierce$p.value , "\n", 
    #    BoxLjung$method, "| X-squared:", BoxLjung$statistic, "P-value: ", BoxLjung$p.value, "\n", "\n", 
    #    "Medidas de precisão para o modelo de previsão:" ,"\n",
    #    "Conjunto de treino |" ,
    #    "ME:", accuracyTeste[1,1], "RMSE:",accuracyTeste[1,2],"MAE:",accuracyTeste[1,3],"\n","MPE:",accuracyTeste[1,4],"MAPE:",accuracyTeste[1,5],"\n","\n",

    #    "Conjunto de teste |",
    #     "ME:", accuracyTeste[2,1], "RMSE:",accuracyTeste[2,2],"MAE:",accuracyTeste[2,3],"\n","MPE:",accuracyTeste[2,4],"MAPE:",accuracyTeste[2,5]
    #     ,sep = " ")
    # })

   
    # p<-tsdisplay(residuals(fit), main='Resíduos NNAR')
   
   
    
   

    #==========================RNN====================================================================================================================
       Bio_ts <- ts(df_trend3[, c('Toxinas.Lipofílicas')])
       diffed = diff(Bio_ts, differences = 1)
       supervised = lag_transform(diffed, 1)
       N = nrow(supervised)
         n = round(N *0.9, digits = 0)
         train = supervised[1:n, ]
         test  = supervised[(n+1):N,  ]
        
        Scaled = scale_data(train, test, c(-1, 1))

    y_train = Scaled$scaled_train[, 2]
    x_train = Scaled$scaled_train[, 1]

    y_test = Scaled$scaled_test[, 2]
    x_test = Scaled$scaled_test[, 1]

    dim(x_train) <- c(length(x_train), 1, 1)
    dim(y_train) <- c(length(y_train), 1, 1)


    
      

        model <- trainr(Y=y_train,X=x_train,learningrate = 0.5,hidden_dim = 10,numepochs=100)
      


    
        Eperror <- model$error



    scaler = Scaled$scaler
    predictions = numeric(4)
    X = x_test[length(x_test)]

      #Previsão de 5 semanas      
    for(i in 1:4){
         dim(X) = c(1,1,1)
         yhat = model %>% predictr(X)
         X = yhat
         # invert scaling
         yhat = invert_scaling(yhat, scaler,  c(-1, 1))
         # invert differencing
         yhat  = yhat + Bio_ts[(n+i)]

         # store
         predictions[i] <- yhat
     }


         #TESTAR FITTED VALUES train
     predictionsTRAIN = numeric(length(x_train))

     for(i in 1:length(x_train)){

       Y = x_train[i]

             dim(Y) = c(1,1,1)

         yhat2 = model %>% predictr(Y)
         # invert scaling
         yhat2 = invert_scaling(yhat2, scaler,  c(-1, 1))
         # invert differencing
         yhat2  = yhat2 + Bio_ts[(i)]
         # store
         predictionsTRAIN[i] <- yhat2
     }

     #TESTAR FITTED VALUES teste
     predictionsTESTE = numeric(4)
     for(i in 1:4){

       Z = x_test[i]

             dim(Z) = c(1,1,1)

         yhat3 = model %>% predictr(Z)
         # invert scaling
         yhat3 = invert_scaling(yhat3, scaler,  c(-1, 1))
         # invert differencing
         yhat3  = yhat3 + Bio_ts[(i+2)]
         # store
         predictionsTESTE[i] <- yhat3
     }
          
        
          accuracyTesteFitted <-accuracy(Bio_ts[1:n],predictionsTRAIN)
          accuracyTeste<- accuracy(Bio_ts[n+1:n+3],predictionsTESTE)   
          accuracyRNN<-accuracyTeste

        #Mudar 20 Para 0
      df_trend <- df()[df()$Zona.de.produção == input$Zonas_BESTMODEL, ]
      df_trend3 <- df_trend[df_trend$Espécie %in% input$Especie_BESTMODEL, ] 

    Bio_ts <- ts(df_trend3[, c('Toxinas.Lipofílicas')])

    ts2 <- c(Bio_ts,predictions)
    time <- c(Data,NovaData)
    var1 <- ts2
    DF <- data.frame(time, var1)
    pRNN <- ggplot(DF, aes(time, var1, colour=(time>=ultimaData)))+ xlab("Data") + ylab("Toxinas AO+DTXs (Microgramas AO equiv/kg)") + geom_line(aes(group=1))+ geom_hline(aes(yintercept= 160, linetype = "Limite regulamentar"), colour= 'NavyBlue')+ 
    scale_linetype_manual(name = " ", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("NavyBlue")))) + labs(color=" ") + scale_color_manual(labels = c("Original", "Previsão"), values = c("blue", "red")) + theme_classic() +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + theme(legend.text=element_text(size=12))
    # print(pRNN)
    # # pRNN<-recordPlot()





#===================================================SELECÇÃO DO MELHOR===========================================

          WNNAR<-0
          WRNN<-0
          BESTMODEL<-0

          for(i in 1:5){
              if(abs(accuracyNNAR[2,i])<=abs(accuracyRNN[i])){
                  WNNAR<-WNNAR+1
              }else{
                WRNN<-WRNN+1
              }
          }

          if(WNNAR>=WRNN){
            BESTMODEL<-WNNAR
            MODEL<-"NNAR"
          }else{
            BESTMODEL<-WRNN
            MODEL<-"RNN"
          }
          
         


      output$txtoutBESTMODEL_ANN <- renderText({

       paste("Medidas de precisão para o modelo de previsão:" ,"\n",

        "Conjunto de teste NNAR|",
         "ME:", accuracyNNAR[2,1], "RMSE:",accuracyNNAR[2,2],"MAE:",accuracyNNAR[2,3],"\n","MPE:",accuracyNNAR[2,4],"MAPE:",accuracyNNAR[2,5],"\n","\n",
       
  

       "Conjunto de teste RNN|",
        "ME:", accuracyRNN[1], "RMSE:",accuracyRNN[2],"MAE:",accuracyRNN[3],"\n","MPE:",accuracyRNN[4],"MAPE:",accuracyRNN[5],"\n","\n",

        "MELHOR MODELO:",MODEL,"PESO:",BESTMODEL

               ,sep = " ")

        })

     

     if(MODEL == "NNAR"){
       print(pNNAR)
     }
      if(MODEL == "RNN"){
      print(pRNN)
    }

  


   output$plotForecastingBESTMODEL_ANN_RESIDUAL<- renderPlot({

    if(MODEL =="NNAR"){
       pResidual<-tsdisplay(residualsNNAR, main='Resíduos')
       pResidual

     }

      if(MODEL =="RNN"){
      pError<-plot(colMeans(Eperror),type = "l",xlab="Época",ylab = "Erro",main='Erros ao longo das épocas')
      pError
     
    }
 })

    
    

  



 })



#==============================================================================================================================================














 output$trendPlot3 <- renderPlotly({


validate(
      need(length(input$show_vars4) != 0, "Por favor, selecione pelo menos uma espécie.")
      )

    if (length(input$show_vars3) == 0) {
      print("Selecione uma zona")
    } else {
      
      df_trend1 <- ideal[ideal$Zona.de.produção == input$show_vars3, ]

            df_trend <- df()[df()$Capitania == input$select, ]
            df_trend <- df_trend[df_trend$Zona.de.produção == input$show_vars3, ]
            df_trend <- df_trend[df_trend$Espécie %in% input$show_vars4, ]


            aux1<-c(min(as.Date(df_trend$Data.da.colheita,"%d/%m/%Y")),min(as.Date(df_trend1$Data.da.colheita,"%d/%m/%Y")))
            aux2<-c (max(as.Date(df_trend$Data.da.colheita,"%d/%m/%Y")) , max(as.Date(df_trend1$Data.da.colheita,"%d/%m/%Y")))
            datas_dia <- seq(from=as.Date(min(aux1)), to=as.Date(max(aux2)), by="day")   


  
          ay <- list(
          
          overlaying = "y",
          side = "right",
          title = "Causadoras de DSP (células/l)"

          )
        

          if(input$VerLimite2==TRUE){

          p <- plot_ly() %>%
          add_lines(datas_dia,y=160,name="Limite regulamentar") %>%
          add_lines(as.Date(df_trend$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df_trend$Toxinas.Lipofílicas),by = df_trend$Espécie, color = df_trend$Espécie) %>%
          add_lines(as.Date(df_trend1$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df_trend1$Produtoras.de.DSP), name = "Causadoras de DSP", yaxis = "y2") %>%
          layout(
          title = "", yaxis2 = ay,
          xaxis = list(title="Data"),
          legend=list(x=1.1,y=1.1) ,
          yaxis=list(title="Toxinas AO+DTXs (μg AO equiv/kg)",size = 12)
        ) 

      }else{

         p <- plot_ly() %>%
          add_lines(as.Date(df_trend$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df_trend$Toxinas.Lipofílicas),by = df_trend$Espécie, color = df_trend$Espécie) %>%
          add_lines(as.Date(df_trend1$Data.da.colheita,"%d/%m/%Y"), y = as.numeric(df_trend1$Produtoras.de.DSP), name = "Causadoras de DSP", yaxis = "y2") %>%
          layout(
          title = "", yaxis2 = ay,
          xaxis = list(title="Data"),
          legend=list(x=1.1,y=1.1),
          yaxis=list(title="Toxinas AO+DTXs (μg AO equiv/kg",size = 12)
        ) 



      }

   SAVEPLOT_BF<<-p
    }

  })




output$txtout <- renderText({
      paste("Média:",mean(df()$Toxinas.Lipofílicas),"| Mediana:",median(df()$Toxinas.Lipofílicas),"| Desvio padrão:",sd(df()$Toxinas.Lipofílicas),"| Variância:",var(df()$Toxinas.Lipofílicas), sep = " ")
    })


output$txtout2 <- renderText({
       paste("Média: ","| Mediana: ","| Desvio padrão: ","| Variância: ", sep = " ")
    })

output$txtoutStationary <- renderText({
     df_trend <- df()[df()$Zona.de.produção == input$show_vars_zonas, ]
     df_trend3 <- df_trend[df_trend$Espécie %in% input$show_especie, ] 
     Stationary <- adf.test(df_trend3$Toxinas.Lipofílicas)

      paste("*Augmented Dickey-Fuller Test*", "| Dickey-Fuller:",Stationary$statistic," | P-value:",Stationary$p.value,"| Alternative hypothesis:",Stationary$alternative)
    })

output$txtoutStationary2 <- renderText({
      Stationary <- adf.test(df2()$Produtoras.de.DSP)

      paste("*Augmented Dickey-Fuller Test*", "| Dickey-Fuller:",Stationary$statistic," | P-value:",Stationary$p.value,"| Alternative hypothesis:",Stationary$alternative)
    })




  }

shinyApp(ui, server)




# ggplot(result, aes(x=as.numeric(result$step))) + geom_line(aes(y = ARIMA, colour="ARIMA"),size=0.8) + 
# geom_line(aes(y = VAR,colour="VAR"),size=0.8) + geom_line(aes(y = VARMA,colour="VARMA"),size=0.8) +  
# geom_line(aes(y = NNAR,colour="NNAR"),size=0.8) +
#  geom_line(aes(y = RNN,colour="RNN"),size=0.8) + xlab("Step") + ylab("Error") + theme_classic() +labs(color=" ") + theme(axis.text=element_text(size=12),axis.title=element_text(size=12)) + theme(legend.position="top")  + scale_x_continuous(limits = c(1, 11))
