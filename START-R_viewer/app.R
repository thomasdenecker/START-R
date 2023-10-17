# START-R viewer: a Simple tool to visualize START-R output
# 2018 - CNRS - Institut Jacques Monod
# Thomas DENECKER - PhD studient - Computational biology
# thomas.denecker@gmail.com

################################################################################
# Library
################################################################################

library(shiny)
library(plotly)
library(colourpicker)
library("shinyjs")
library(stringr)

################################################################################
# UI
################################################################################

ui <- shinyUI(fluidPage(shinyjs::useShinyjs(),
                        HTML('<link rel="stylesheet" type="text/css" href="style.css" />'),
                        tags$head(tags$title("START-R viewer")),
                        tags$head(tags$link(href = "PCNA.ico", rel ="shortcut icon")),
                        img(src = "logo_Viewer.svg", class = "logo"),
                        
                        #/////////////////////////////////////////////////////////////////////////////
                        # Area to load data and print informations
                        #/////////////////////////////////////////////////////////////////////////////
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            fileInput("file", multiple = TRUE, "Choose CSV File(s) (maximum : 10 experiments)"),
                            HTML("<div class = 'error'>"),
                            textOutput("erreur"),
                            HTML("</div>"),
                            actionButton("action", label = "Load data")
                          ),
                          
                          mainPanel(
                            h3("Data informations"),
                            fluidRow(
                              column(6,
                                     textOutput("organism"),
                                     textOutput("chrom"),
                                     textOutput("Mdif"),
                                     textOutput("nbAdv"),
                                     textOutput('nbDel')
                              ),
                              column(6,
                                     textOutput("SM"),
                                     textOutput("NIntra"),
                                     textOutput("NInterR"),
                                     textOutput("NInterE")
                              )
                              
                            )
                            
                          )
                        ),
                        
                        #/////////////////////////////////////////////////////////////////////////////
                        # Area with general plot
                        #/////////////////////////////////////////////////////////////////////////////
                        
                        fluidRow(
                          plotlyOutput("TRPlot")
                        ),
                        
                        #/////////////////////////////////////////////////////////////////////////////
                        # Area to change the view window
                        #/////////////////////////////////////////////////////////////////////////////
                        
                        fluidRow(
                          column(1),
                          column(2,h5("Start of the viewing window")),
                          column(2,numericInput("min", label = NA, value = NULL)),
                          column(2,h5("End of the viewing window")),
                          column(2,numericInput("max", label = NA, value = NULL)),
                          column(2,actionButton("action2", label = "Change")),
                          column(1)
                        ),
                        
                        #/////////////////////////////////////////////////////////////////////////////
                        # Area to customize plot
                        #/////////////////////////////////////////////////////////////////////////////
                        
                        wellPanel(
                          fluidRow(
                            # Inputs to change color of elements
                            h3("Customization"),
                            column(2,
                                   tags$b(textOutput("text_col1")),
                                   colourpicker::colourInput("col1", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col2")),
                                   colourpicker::colourInput("col2", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col3")),
                                   colourpicker::colourInput("col3", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col4")),
                                   colourpicker::colourInput("col4", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col5")),
                                   colourpicker::colourInput("col5", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col6")),
                                   colourpicker::colourInput("col6", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col7")),
                                   colourpicker::colourInput("col7", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col8")),
                                   colourpicker::colourInput("col8", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col9")),
                                   colourpicker::colourInput("col9", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col10")),
                                   colourpicker::colourInput("col10", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col11")),
                                   colourpicker::colourInput("col11", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col12")),
                                   colourpicker::colourInput("col12", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col13")),
                                   colourpicker::colourInput("col13", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col14")),
                                   colourpicker::colourInput("col14", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col15")),
                                   colourpicker::colourInput("col15", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col16")),
                                   colourpicker::colourInput("col16", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col17")),
                                   colourpicker::colourInput("col17", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col18")),
                                   colourpicker::colourInput("col18", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col19")),
                                   colourpicker::colourInput("col19", NA)
                            ),
                            column(2,
                                   tags$b(textOutput("text_col20")),
                                   colourpicker::colourInput("col20", NA)
                            ),
                            column(12,actionButton("change_color", label = "Change"))
                          )
                          
                        ),
                        #/////////////////////////////////////////////////////////////////////////////
                        # Area to plot general informations
                        #/////////////////////////////////////////////////////////////////////////////
                        
                        h3(textOutput("InfoG"), class = "center"),
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons("Select", "Select general information to plot:",
                                         c("None" = "None"), selected = "None"
                            )
                          ),
                          
                          mainPanel(
                            plotlyOutput("general")
                          )
                        )
))

################################################################################
# SERVER
################################################################################

server <- shinyServer(function(input, output, session) {
  
  # Reactive value
  rv <- reactiveValues()
  
  #/////////////////////////////////////////////////////////////////////////////
  # Erreur message import file
  #/////////////////////////////////////////////////////////////////////////////
  
  output$erreur <- renderText({
    if( length(input$file$name) != 0){
      # format = unlist(strsplit(input$file$name, "\\."))[length(unlist(strsplit(input$file$name, "\\.")))]
      format = unlist(strsplit(input$file$name, "\\."))[length(unlist(strsplit(input$file[[1, "name"]], "\\.")))]
      if(format != "SRV"){
        "The file is not an output of START-R (.SRV)"
      }
    }
  })
  
  observe({
    
    if(is.null(input$file)){
      shinyjs::disable("action")
    }else{
      # format = unlist(strsplit(input$file$name, "\\."))[length(unlist(strsplit(input$file$name, "\\.")))]
      format = unlist(strsplit(input$file$name, "\\."))[length(unlist(strsplit(input$file[[1, "name"]], "\\.")))]
      if(format != "SRV"){
        shinyjs::disable("action")
      }else{
        shinyjs::enable("action")
      }
    }
  })
  
  #/////////////////////////////////////////////////////////////////////////////
  # Output data informations
  #/////////////////////////////////////////////////////////////////////////////
  
  output$NInterR <- renderText({
    paste("Normalisation inter replicas :", rv$infos[1,2])
  })
  
  output$NInterE <- renderText({
    if( length(rv$infos[1,3]) != 0 ){
      if(rv$infos[1,3] == "n0"){
        "Normalisation inter experiences : without normalization"
      }else if(rv$infos[1,3] == "n1"){
        "Normalisation inter experiences : standardization"
      } else if(rv$infos[1,3] == "n3"){
        "Normalisation inter experiences : unitization "
      }
    }else{
      "Normalisation inter experiences :"
    }
  })
  
  output$Mdif <- renderText({
    paste("Method differential :", rv$infos[1,4])
  })
  
  output$nbDel <- renderText({
    if(!is.null(rv$de)){
      paste("# of delayed :", nrow(rv$de))
    } else {
      "# of delayed : 0"
    }
  })
  
  output$nbAdv <- renderText({
    if(!is.null(rv$ad)){
      paste("# of advanced :", nrow(rv$ad))
    } else {
      "# of advanced : 0"
    }
  })
  
  output$organism <- renderText({
    paste("Organism :", rv$infos[1,5])
  })
  
  output$SM <- renderText({
    paste("Smooth method :", rv$infos[1,6])
  })
  
  output$chrom <- renderText({
    paste("Chromosome :", rv$chromosome)
  })
  
  output$NIntra <- renderText({
    paste("Normalisation intra :", rv$infos[1,1])
  })
  
  #/////////////////////////////////////////////////////////////////////////////
  # Output customization
  #/////////////////////////////////////////////////////////////////////////////
  
  output$text_col1 <- renderText({
    if(length(rv$plotdif) == 0){
      "Element 1"
    } else if(rv$plotdif == TRUE){
      "Experiment 1"
    }else if(rv$plotdif == FALSE){
      "Data"
    }
  })
  
  output$text_col2 <- renderText({
    if(length(rv$plotdif) == 0){
      "Element 2"
    }else if(rv$plotdif == TRUE){
      "Experiment 2"
    }else if(rv$plotdif == FALSE){
      "Smooth"
    }
  })
  
  output$text_col3 <- renderText({
    if(length(rv$plotdif) == 0){
      "Element 3"
    }else if(rv$plotdif == TRUE){
      "Smooth exp. 1"
    }else if(rv$plotdif == FALSE){
      "CTR : late"
    }
  })
  
  output$text_col4 <- renderText({
    if(length(rv$plotdif) == 0){
      "Element 4"
    }else if(rv$plotdif == TRUE){
      "Smooth exp. 2"
    }else if(rv$plotdif == FALSE){
      "CTR : early"
    }
  })
  
  output$text_col5 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$filesfromdiff == FALSE && rv$nbrcond < 3)){
      "Element 5"
    }else if(rv$plotdif == TRUE && rv$filesfromdiff == TRUE && rv$nbrcond < 3){
      "Advanced"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 3)){
      "Experiment 3"
    }else if(rv$plotdif == FALSE){
      "TTR"
    }
  })
  
  output$text_col6 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$filesfromdiff == FALSE && rv$nbrcond < 3)){
      "Element 6"
    }else if(rv$plotdif == TRUE && rv$filesfromdiff == TRUE && rv$nbrcond < 3){
      "Delayed"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 3)){
      "Smooth exp. 3"
    }else if(rv$plotdif == FALSE){
      "Segmentation"
    }
  })
  
  output$text_col7 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 4) || (rv$plotdif == FALSE)){
      "Element 7"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 4)){
      "Experiment 4"
    }
  })
  
  output$text_col8 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 4) || (rv$plotdif == FALSE)){
      "Element 8"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 4)){
      "Smooth exp. 4"
    }
  })
  
  output$text_col9 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 5) || (rv$plotdif == FALSE)){
      "Element 9"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 5)){
      "Experiment 5"
    }
  })
  
  output$text_col10 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 5) || (rv$plotdif == FALSE)){
      "Element 10"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 5)){
      "Smooth exp. 5"
    }
  })
  
  output$text_col11 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 6) || (rv$plotdif == FALSE)){
      "Element 11"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 6)){
      "Experiment 6"
    }
  })
  
  output$text_col12 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 6) || (rv$plotdif == FALSE)){
      "Element 12"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 6)){
      "Smooth exp. 6"
    }
  })
  
  output$text_col13 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 7) || (rv$plotdif == FALSE)){
      "Element 13"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 7)){
      "Experiment 7"
    }
  })
  
  output$text_col14 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 7) || (rv$plotdif == FALSE)){
      "Element 14"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 7)){
      "Smooth exp. 7"
    }
  })
  
  output$text_col15 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 8) || (rv$plotdif == FALSE)){
      "Element 15"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 8)){
      "Experiment 8"
    }
  })
  
  output$text_col16 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 8) || (rv$plotdif == FALSE)){
      "Element 16"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 8)){
      "Smooth exp. 8"
    }
  })
  
  output$text_col17 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 9) || (rv$plotdif == FALSE)){
      "Element 17"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 9)){
      "Experiment 9"
    }
  })
  
  output$text_col18 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 9) || (rv$plotdif == FALSE)){
      "Element 18"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 9)){
      "Smooth exp. 9"
    }
  })
  
  output$text_col19 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 10) || (rv$plotdif == FALSE)){
      "Element 19"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 10)){
      "Experiment 10"
    }
  })
  
  output$text_col20 <- renderText({
    if(length(rv$plotdif) == 0 || (rv$plotdif == TRUE && rv$nbrcond < 10) || (rv$plotdif == FALSE)){
      "Element 20"
    }else if((rv$plotdif == TRUE) && (rv$nbrcond >= 10)){
      "Smooth exp. 10"
    }
  })
  
  #/////////////////////////////////////////////////////////////////////////////
  # LOAD FILE
  #/////////////////////////////////////////////////////////////////////////////
  
  observeEvent(input$action, {
    
    # d <- read.table(input$file$datapath, sep = "\t", header = T , skip = 1)
    nbr.files <- dim(input$file)[1]
    list.df <- as.list(1:nbr.files)
    for (i in 1:nbr.files){
      df <- read.table(input$file[[i, "datapath"]], sep = "\t", header = T , skip = 1)
      list.df[i] <- list(df)
    }
    if (nbr.files == 1){
      d <- list.df[[1]]
      if ("Exp 1" %in% d$Name){
        rv$filesfromdiff <- TRUE
      }else if ("Data" %in% d$Name){
        rv$filesfromdiff <- FALSE
      }
      # Else, if the user choose multiple files
    }else if (nbr.files > 1){
      d <- data.frame()
      k <- 1
      for (df in list.df){
        if ("Exp 1" %in% df$Name){
          rv$filesfromdiff <- TRUE
          if (k == 1){
            d <- list.df[[1]]
            k <- k + 1
          }else if (k > 1){
            df$Name <- str_replace(df$Name, "Exp 1", str_glue("Exp {k+1}"))
            df$Name <- str_replace(df$Name, "Exp 2", str_glue("Exp {k+2}"))
            df$Name <- str_replace(df$Name, "Smooth Exp 1", str_glue("Smooth Exp {k+1}"))
            df$Name <- str_replace(df$Name, "Smooth Exp 2", str_glue("Smooth Exp {k+2}"))
            d <- rbind(d, df)
            k <- k + 2
          }
        }else if ("Data" %in% df$Name){
          rv$filesfromdiff <- FALSE
          df$Name <- str_replace(df$Name, "Data", str_glue("Exp {k}"))
          df$Name <- str_replace(df$Name, "Loess", str_glue("Smooth Exp {k}"))
          d <- rbind(d, df)
          k <- k + 1
        }
      }
    }
    
    if("Exp 1" %in% d$Name){
      # Add values in reactive variable
      rv$nbrcond <- 2
      rv$plotdif <- TRUE
      rv$exp1 <- subset(d, d[,"Name"] == "Exp 1")
      rv$exp2 <- subset(d, d[,"Name"] == "Exp 2")
      rv$Sexp1 <- subset(d, d[,"Name"] == "Smooth Exp 1")
      rv$Sexp2 <- subset(d, d[,"Name"] == "Smooth Exp 2")
      rv$ad <- subset(d, d[,"Name"] == "Advanced")
      rv$de <- subset(d, d[,"Name"] == "Delayed")
      # Add experiments if the user choose multiple files
      if ("Exp 3" %in% d$Name){
        rv$nbrcond <- 3
        rv$exp3 <- subset(d, d[,"Name"] == "Exp 3")
        rv$Sexp3 <- subset(d, d[,"Name"] == "Smooth Exp 3")
        rv$ad <- NULL
        rv$de <- NULL
      }
      if ("Exp 4" %in% d$Name){
        rv$nbrcond <- 4
        rv$exp4 <- subset(d, d[,"Name"] == "Exp 4")
        rv$Sexp4 <- subset(d, d[,"Name"] == "Smooth Exp 4")
        rv$ad <- NULL
        rv$de <- NULL
      }
      if ("Exp 5" %in% d$Name){
        rv$nbrcond <- 5
        rv$exp5 <- subset(d, d[,"Name"] == "Exp 5")
        rv$Sexp5 <- subset(d, d[,"Name"] == "Smooth Exp 5")
        rv$ad <- NULL
        rv$de <- NULL
      }
      if ("Exp 6" %in% d$Name){
        rv$nbrcond <- 6
        rv$exp6 <- subset(d, d[,"Name"] == "Exp 6")
        rv$Sexp6 <- subset(d, d[,"Name"] == "Smooth Exp 6")
        rv$ad <- NULL
        rv$de <- NULL
      }
      if ("Exp 7" %in% d$Name){
        rv$nbrcond <- 7
        rv$exp7 <- subset(d, d[,"Name"] == "Exp 7")
        rv$Sexp7 <- subset(d, d[,"Name"] == "Smooth Exp 7")
        rv$ad <- NULL
        rv$de <- NULL
      }
      if ("Exp 8" %in% d$Name){
        rv$nbrcond <- 8
        rv$exp8 <- subset(d, d[,"Name"] == "Exp 8")
        rv$Sexp8 <- subset(d, d[,"Name"] == "Smooth Exp 8")
        rv$ad <- NULL
        rv$de <- NULL
      }
      if ("Exp 9" %in% d$Name){
        rv$nbrcond <- 9
        rv$exp9 <- subset(d, d[,"Name"] == "Exp 9")
        rv$Sexp9 <- subset(d, d[,"Name"] == "Smooth Exp 9")
        rv$ad <- NULL
        rv$de <- NULL
      }
      if ("Exp 10" %in% d$Name){
        rv$nbrcond <- 10
        rv$exp10 <- subset(d, d[,"Name"] == "Exp 10")
        rv$Sexp10 <- subset(d, d[,"Name"] == "Smooth Exp 10")
        rv$ad <- NULL
        rv$de <- NULL
      }
      #######################
      
      # Empty the potentially used variables
      rv$data <- NULL
      rv$Loess <- NULL
      rv$CTR_L <- NULL
      rv$CTR_E <- NULL
      rv$TTR <- NULL
      rv$Seg <- NULL
      
      # Add color
      rv$c1 <- "gray80"
      rv$c2 <- "gray"
      rv$c3 <- "blue"
      rv$c4 <- "red"
      rv$c5 <- "forestgreen"
      rv$c6 <- "magenta"
      if (rv$nbrcond >= 3){
        rv$c5 <- "#148f32"
        rv$c6 <- "#148f32"
      }
      rv$c7 <- "#00fbff"
      rv$c8 <- "#00fbff"
      rv$c9 <- "#00ff68"
      rv$c10 <- "#00ff68"
      rv$c11 <- "#ff00dc"
      rv$c12 <- "#ff00dc"
      rv$c13 <- "#ff007c"
      rv$c14 <- "#ff007c"
      rv$c15 <- "#0083ff"
      rv$c16 <- "#0083ff"
      rv$c17 <- "#f0ff00"
      rv$c18 <- "#f0ff00"
      rv$c19 <- "#ff9b00"
      rv$c20 <- "#ff9b00"
      
      # Choices general informations
      updateRadioButtons(session = session, inputId = "Select",
                         choices = c("None" = "None",
                                     "Exp. 1 intensity" = "EXP1",
                                     "Exp. 2 intensity" = "EXP2",
                                     "Exp. 3 intensity" = "EXP3",
                                     "Exp. 4 intensity" = "EXP4",
                                     "Exp. 5 intensity" = "EXP5",
                                     "Exp. 6 intensity" = "EXP6",
                                     "Exp. 7 intensity" = "EXP7",
                                     "Exp. 8 intensity" = "EXP8",
                                     "Exp. 9 intensity" = "EXP9",
                                     "Exp. 10 intensity" = "EXP10"),
                         selected = "None"
      )
      
    } else {
      # Add values in reactive variable
      rv$plotdif <- FALSE
      rv$data <- subset(d, d[,"Name"] == "Data")
      rv$Loess <- subset(d, d[,"Name"] == "Loess")
      rv$CTR_L <- subset(d, d[,"Name"] == "CTR : late")
      rv$CTR_E <- subset(d, d[,"Name"] == "CTR : early")
      rv$TTR <- subset(d, d[,"Name"] == "TTR")
      rv$Seg <- subset(d, d[,"Name"] == "Segmentation")
      
      # Empty the potentially used variables
      rv$exp1 <- NULL
      rv$exp2 <- NULL
      rv$Sexp1 <- NULL
      rv$Sexp2 <- NULL
      rv$ad <- NULL
      rv$de <- NULL
      
      # Add color
      rv$c1 <- "gray80"
      rv$c2 <- "blue"
      rv$c3 <- "green"
      rv$c4 <- "red"
      rv$c5 <- "gold"
      rv$c6 <- "darkmagenta"
      
      # Choices general informations
      
      updateRadioButtons(session = session, inputId = "Select",
                         choices =  c("None" = "None",
                                      "Data intensity" = "DI",
                                      "CTR/TTR boxplots" = "BoxplotCTTR"),
                         selected = "None")
    }
    
    # Get informations about experience(s)
    # infos <- read.table(input$file$datapath, sep = "\t", header = F , nrows = 1)
    infos <- read.table(input$file[[1, "datapath"]], sep = "\t", header = F , nrows = 1)
    rv$infos = infos
    
    # Generate variable with chromosome
    chro = unlist(strsplit(as.character(rv$infos[1,7]),"chr"))
    chro = chro[chro != ""]
    rv$chromosome <- paste("chromosome",chro)
    
    # add in reactive variable plot limits
    rv$min <- min(as.numeric(d[,"Position"]), na.rm = T)
    rv$max <- max(as.numeric(d[,"Position"]), na.rm = T)
    
    # Change plot limits in inputs
    updateNumericInput(session, "min", value = rv$min)
    updateNumericInput(session, "max", value = rv$max)
    
    # Update Customization
    colourpicker::updateColourInput(session, "col1",value = rv$c1)
    colourpicker::updateColourInput(session, "col2",value = rv$c2)
    colourpicker::updateColourInput(session, "col3",value = rv$c3)
    colourpicker::updateColourInput(session, "col4",value = rv$c4)
    colourpicker::updateColourInput(session, "col5",value = rv$c5)
    colourpicker::updateColourInput(session, "col6",value = rv$c6)
    colourpicker::updateColourInput(session, "col7",value = rv$c7)
    colourpicker::updateColourInput(session, "col8",value = rv$c8)
    colourpicker::updateColourInput(session, "col9",value = rv$c9)
    colourpicker::updateColourInput(session, "col10",value = rv$c10)
    colourpicker::updateColourInput(session, "col11",value = rv$c11)
    colourpicker::updateColourInput(session, "col12",value = rv$c12)
    colourpicker::updateColourInput(session, "col13",value = rv$c13)
    colourpicker::updateColourInput(session, "col14",value = rv$c14)
    colourpicker::updateColourInput(session, "col15",value = rv$c15)
    colourpicker::updateColourInput(session, "col16",value = rv$c16)
    colourpicker::updateColourInput(session, "col17",value = rv$c17)
    colourpicker::updateColourInput(session, "col18",value = rv$c18)
    colourpicker::updateColourInput(session, "col19",value = rv$c19)
    colourpicker::updateColourInput(session, "col20",value = rv$c20)
  })
  
  #/////////////////////////////////////////////////////////////////////////////
  # Change plot limits
  #/////////////////////////////////////////////////////////////////////////////
  
  observeEvent(input$action2, {
    rv$min <- input$min
    rv$max <- input$max
  })
  
  #/////////////////////////////////////////////////////////////////////////////
  # change colors
  #/////////////////////////////////////////////////////////////////////////////
  observeEvent(input$change_color, {
    rv$c1 <- input$col1
    rv$c2 <- input$col2
    rv$c3 <- input$col3
    rv$c4 <- input$col4
    rv$c5 <- input$col5
    rv$c6 <- input$col6
    rv$c7 <- input$col7
    rv$c8 <- input$col8
    rv$c9 <- input$col9
    rv$c10 <- input$col10
    rv$c11 <- input$col11
    rv$c12 <- input$col12
    rv$c13 <- input$col13
    rv$c14 <- input$col14
    rv$c15 <- input$col15
    rv$c16 <- input$col16
    rv$c17 <- input$col17
    rv$c18 <- input$col18
    rv$c19 <- input$col19
    rv$c20 <- input$col20
  })
  
  #/////////////////////////////////////////////////////////////////////////////
  # TR plot
  #/////////////////////////////////////////////////////////////////////////////
  
  output$TRPlot <- renderPlotly({
    
    if(length(rv$data[,"Position"]) != 0 && rv$plotdif == FALSE){
      plot_ly(x = rv$data[,"Position"],
              y = rv$data[,"Intensity"],
              name = 'Data', type = 'scatter', mode = 'markers', color = I(rv$c1),
              opacity = 0.2) %>%
        add_lines(x = rv$Loess[,"Position"],
                  y = rv$Loess[,"Intensity"],
                  line = list(color = rv$c2, width = 2), name ="Loess", opacity = 1) %>%
        add_markers(x = rv$CTR_L[,"Position"],
                    y = rv$CTR_L[,"Intensity"],
                    marker = list(color = rv$c3), name ="CTR : late", opacity = 1) %>%
        add_markers(x = rv$CTR_E[,"Position"],
                    y = rv$CTR_E[,"Intensity"],
                    marker = list(color = rv$c4), name ="CTR : early", opacity = 1) %>%
        add_markers(x = rv$TTR[,"Position"],
                    y = rv$TTR[,"Intensity"],
                    marker = list(color = rv$c5), name ="TTR", opacity = 1) %>%
        add_lines(x = rv$Seg[,"Position"],
                  y = rv$Seg[,"Intensity"],
                  line = list(color = rv$c6, width = 3), name ="Segmentation",
                  opacity = 1) %>%
        layout(title = paste('Timing replication study for',rv$chromosome),
               yaxis = list(title = 'Intensity' ),
               xaxis = list(title = 'Position (bp)' , range = c(rv$min, rv$max)))
      
    } else if((length(rv$exp1[,"Position"]) != 0) && (rv$plotdif == TRUE)){# && (input$nbrcond == "two")){
      g <- plot_ly(x = rv$exp1[,"Position"],
                   y = rv$exp1[,"Intensity"],
                   name = 'Exp 1', type = 'scatter', mode = 'markers',
                   symbols = 'x',
                   color = I(rv$c1),
                   opacity = 0.2) %>%
        layout(title = paste('Differential study for',rv$chromosome),
               yaxis = list(title = 'Intensity' ),
               xaxis = list(title = 'Position (bp)' , range = c(rv$min, rv$max))) %>%
        add_markers(x = rv$exp2[,"Position"],
                    y = rv$exp2[,"Intensity"],
                    marker = list(color = rv$c2 ), name ="Exp 2", opacity = 0.2) %>%
        add_lines(x = rv$Sexp1[,"Position"],
                  y = rv$Sexp1[,"Intensity"],
                  line = list(color = rv$c3, width = 2), name ="Smooth Exp 1", opacity = 1) %>%
        add_lines(x = rv$Sexp2[,"Position"],
                  y = rv$Sexp2[,"Intensity"],
                  line = list(color = rv$c4, width = 2), name ="Smooth Exp 2", opacity = 1)
      
      if(!is.null(rv$ad) && nrow(rv$ad) != 0 && rv$nbrcond < 3){
        g <- add_lines(g, x = rv$ad[,"Position"],
                       y = rv$ad[,"Intensity"],
                       line = list(color = rv$c5, width = 5), name ="Advanced", opacity = 1)
      }
      
      if(!is.null(rv$de) && nrow(rv$de) != 0 && rv$nbrcond < 3){
        g <- add_lines(g, x = rv$de[,"Position"],
                       y = rv$de[,"Intensity"],
                       line = list(color = rv$c6, width = 5), name ="Delayed", opacity = 1)
      }
      
      if (rv$nbrcond >= 3){
        g <- add_markers(g, x = rv$exp3[,"Position"],
                         y = rv$exp3[,"Intensity"],
                         marker = list(color = rv$c5 ), name ="Exp 3", opacity = 0.2)
        g <- add_lines(g, x = rv$Sexp3[,"Position"],
                       y = rv$Sexp3[,"Intensity"],
                       line = list(color = rv$c6, width = 2), name ="Smooth Exp 3", opacity = 1)
      }
      if (rv$nbrcond >= 4){
        g <- add_markers(g, x = rv$exp4[,"Position"],
                         y = rv$exp4[,"Intensity"],
                         marker = list(color = rv$c7 ), name ="Exp 4", opacity = 0.2)
        g <- add_lines(g, x = rv$Sexp4[,"Position"],
                       y = rv$Sexp4[,"Intensity"],
                       line = list(color = rv$c8, width = 2), name ="Smooth Exp 4", opacity = 1)
      }
      if (rv$nbrcond >= 5){
        g <- add_markers(g, x = rv$exp5[,"Position"],
                         y = rv$exp5[,"Intensity"],
                         marker = list(color = rv$c9 ), name ="Exp 5", opacity = 0.2)
        g <- add_lines(g, x = rv$Sexp5[,"Position"],
                       y = rv$Sexp5[,"Intensity"],
                       line = list(color = rv$c10, width = 2), name ="Smooth Exp 5", opacity = 1)
      }
      if (rv$nbrcond >= 6){
        g <- add_markers(g, x = rv$exp6[,"Position"],
                         y = rv$exp6[,"Intensity"],
                         marker = list(color = rv$c11 ), name ="Exp 6", opacity = 0.2)
        g <- add_lines(g, x = rv$Sexp6[,"Position"],
                       y = rv$Sexp6[,"Intensity"],
                       line = list(color = rv$c12, width = 2), name ="Smooth Exp 6", opacity = 1)
      }
      if (rv$nbrcond >= 7){
        g <- add_markers(g, x = rv$exp7[,"Position"],
                         y = rv$exp7[,"Intensity"],
                         marker = list(color = rv$c13 ), name ="Exp 7", opacity = 0.2)
        g <- add_lines(g, x = rv$Sexp7[,"Position"],
                       y = rv$Sexp7[,"Intensity"],
                       line = list(color = rv$c14, width = 2), name ="Smooth Exp 7", opacity = 1)
      }
      if (rv$nbrcond >= 8){
        g <- add_markers(g, x = rv$exp8[,"Position"],
                         y = rv$exp8[,"Intensity"],
                         marker = list(color = rv$c15 ), name ="Exp 8", opacity = 0.2)
        g <- add_lines(g, x = rv$Sexp8[,"Position"],
                       y = rv$Sexp8[,"Intensity"],
                       line = list(color = rv$c16, width = 2), name ="Smooth Exp 8", opacity = 1)
      }
      if (rv$nbrcond >= 9){
        g <- add_markers(g, x = rv$exp9[,"Position"],
                         y = rv$exp9[,"Intensity"],
                         marker = list(color = rv$c17 ), name ="Exp 9", opacity = 0.2)
        g <- add_lines(g, x = rv$Sexp9[,"Position"],
                       y = rv$Sexp9[,"Intensity"],
                       line = list(color = rv$c18, width = 2), name ="Smooth Exp 9", opacity = 1)
      }
      if (rv$nbrcond >= 10){
        g <- add_markers(g, x = rv$exp10[,"Position"],
                         y = rv$exp10[,"Intensity"],
                         marker = list(color = rv$c19 ), name ="Exp 10", opacity = 0.2)
        g <- add_lines(g, x = rv$Sexp10[,"Position"],
                       y = rv$Sexp10[,"Intensity"],
                       line = list(color = rv$c20, width = 2), name ="Smooth Exp 10", opacity = 1)
      }
      
      g
      
    }else {
      return()
    }
    
  })
  
  #/////////////////////////////////////////////////////////////////////////////
  # General informations plots
  #/////////////////////////////////////////////////////////////////////////////
  output$InfoG <- renderText({
    paste("General informations about", rv$chromosome)
  })
  
  output$general <- renderPlotly({
    if( length(rv$data) != 0){
      if(input$Select == "None"){
        return()
      } else if(input$Select == "DI"){
        plot_ly(x = rv$data[,"Intensity"], type = "histogram") %>%
          layout(title = "Histogram of intensity",
                 yaxis = list(title = 'Occurence' ),
                 xaxis = list(title = 'Intensity'))
      }else if(input$Select == "BoxplotCTTR"){
        plot_ly(y = rv$CTR_E[,"Intensity"], type = "box",name = "Early CTR",
                color = I("red")) %>%
          add_boxplot(y = rv$CTR_L[,"Intensity"], name = "Late CTR",
                      marker = list(color = 'green'),
                      line = list(color = 'green')) %>%
          add_boxplot(y = rv$TTR[,"Intensity"], name = "TTR",
                      marker = list(color = 'gold'),
                      line = list(color = 'gold')) %>%
          layout(title = "Boxplot of CTR/TTR intensity",
                 yaxis = list(title = 'Intensity' )
          )
      }
    }else if(input$Select == "EXP1"){
      plot_ly(x = rv$exp1[,"Intensity"], type = "histogram") %>%
        layout(title = "Histogram of intensity",
               yaxis = list(title = 'Occurence' ),
               xaxis = list(title = 'Intensity'))
    } else if(input$Select == "EXP2"){
      plot_ly(x = rv$exp2[,"Intensity"], type = "histogram") %>%
        layout(title = "Histogram of intensity",
               yaxis = list(title = 'Occurence' ),
               xaxis = list(title = 'Intensity'))
    } else if(input$Select == "EXP3"){
      plot_ly(x = rv$exp3[,"Intensity"], type = "histogram") %>%
        layout(title = "Histogram of intensity",
               yaxis = list(title = 'Occurence' ),
               xaxis = list(title = 'Intensity'))
    } else if(input$Select == "EXP4"){
      plot_ly(x = rv$exp4[,"Intensity"], type = "histogram") %>%
        layout(title = "Histogram of intensity",
               yaxis = list(title = 'Occurence' ),
               xaxis = list(title = 'Intensity'))
    } else if(input$Select == "EXP5"){
      plot_ly(x = rv$exp5[,"Intensity"], type = "histogram") %>%
        layout(title = "Histogram of intensity",
               yaxis = list(title = 'Occurence' ),
               xaxis = list(title = 'Intensity'))
    } else if(input$Select == "EXP6"){
      plot_ly(x = rv$exp6[,"Intensity"], type = "histogram") %>%
        layout(title = "Histogram of intensity",
               yaxis = list(title = 'Occurence' ),
               xaxis = list(title = 'Intensity'))
    } else if(input$Select == "EXP7"){
      plot_ly(x = rv$exp7[,"Intensity"], type = "histogram") %>%
        layout(title = "Histogram of intensity",
               yaxis = list(title = 'Occurence' ),
               xaxis = list(title = 'Intensity'))
    } else if(input$Select == "EXP8"){
      plot_ly(x = rv$exp8[,"Intensity"], type = "histogram") %>%
        layout(title = "Histogram of intensity",
               yaxis = list(title = 'Occurence' ),
               xaxis = list(title = 'Intensity'))
    } else if(input$Select == "EXP9"){
      plot_ly(x = rv$exp9[,"Intensity"], type = "histogram") %>%
        layout(title = "Histogram of intensity",
               yaxis = list(title = 'Occurence' ),
               xaxis = list(title = 'Intensity'))
    } else if(input$Select == "EXP10"){
      plot_ly(x = rv$exp10[,"Intensity"], type = "histogram") %>%
        layout(title = "Histogram of intensity",
               yaxis = list(title = 'Occurence' ),
               xaxis = list(title = 'Intensity'))
    } else{
      return()
    }
  })
  
})

shinyApp(ui, server)
