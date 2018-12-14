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
      fileInput("file", "Choose CSV File"),
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
      format = unlist(strsplit(input$file$name, "\\."))[length(unlist(strsplit(input$file$name, "\\.")))]
      if(format != "SRV"){
        "The file is not an output of START-R (. SVR)"
      }
    }
  })

  observe({

    if(is.null(input$file)){
      shinyjs::disable("action")
    }else{
      format = unlist(strsplit(input$file$name, "\\."))[length(unlist(strsplit(input$file$name, "\\.")))]
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
      paste("# of advanced :", nrow(rv$de))
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
      "Experience 1"
    }else if(rv$plotdif == F){
      "Data"
    }
  })

  output$text_col2 <- renderText({
    if(length(rv$plotdif) == 0){
      "Element 2"
    }else if(rv$plotdif == TRUE){
      "Experience 2"
    }else if(rv$plotdif == F){
      "Smooth"
    }
  })

  output$text_col3 <- renderText({
    if(length(rv$plotdif) == 0){
      "Element 3"
    }else if(rv$plotdif == TRUE){
      "Smooth exp. 1"
    }else if(rv$plotdif == F){
      "CTR : late"
    }
  })

  output$text_col4 <- renderText({
    if(length(rv$plotdif) == 0){
      "Element 4"
    }else if(rv$plotdif == TRUE){
      "Smooth exp. 2"
    }else if(rv$plotdif == F){
      "CTR : early"
    }
  })

  output$text_col5 <- renderText({
    if(length(rv$plotdif) == 0){
      "Element 5"
    }else if(rv$plotdif == TRUE){
      "Advanced"
    }else if(rv$plotdif == F){
      "TTR"
    }
  })

  output$text_col6 <- renderText({
    if(length(rv$plotdif) == 0){
      "Element 6"
    }else if(rv$plotdif == TRUE){
      "Delayed"
    }else if(rv$plotdif == F){
      "Segmentation"
    }
  })

  #/////////////////////////////////////////////////////////////////////////////
  # LOAD FILE
  #/////////////////////////////////////////////////////////////////////////////

  observeEvent(input$action, {

    d <- read.table(input$file$datapath, sep = "\t", header = T , skip = 1)

    if("Exp 1" %in% d$Name){
      # Add values in reactive variable
      rv$plotdif <- TRUE
      rv$exp1 <- subset(d, d[,"Name"] == "Exp 1")
      rv$exp2 <- subset(d, d[,"Name"] == "Exp 2")
      rv$Sexp1 <- subset(d, d[,"Name"] == "Smooth Exp 1")
      rv$Sexp2 <- subset(d, d[,"Name"] == "Smooth Exp 2")
      rv$ad <- subset(d, d[,"Name"] == "Advanced")
      rv$de <- subset(d, d[,"Name"] == "Delayed")

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

      # Choices general informations
      updateRadioButtons(session = session, inputId = "Select",
                         choices = c("None" = "None",
                                     "Exp. 1 intensity" = "EXP1",
                                     "Exp. 2 intensity" = "EXP2"),
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
    infos <- read.table(input$file$datapath, sep = "\t", header = F , nrows = 1)
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
               xaxis = list(title = 'Position (pb)' , range = c(rv$min, rv$max)))

    } else if(length(rv$exp1[,"Position"]) != 0 && rv$plotdif == TRUE){
      g <- plot_ly(x = rv$exp1[,"Position"],
              y = rv$exp1[,"Intensity"],
              name = 'Exp 1', type = 'scatter', mode = 'markers',
              symbols = 'x',
              color = I(rv$c1),
              opacity = 0.2) %>% 
        layout(title = paste('Differential study for',rv$chromosome),
                       yaxis = list(title = 'Intensity' ),
                       xaxis = list(title = 'Position (pb)' , range = c(rv$min, rv$max))) %>%
        add_markers(x = rv$exp2[,"Position"],
                    y = rv$exp2[,"Intensity"],
                    marker = list(color = rv$c2 ), name ="Exp 2", opacity = 0.2) %>%
        add_lines(x = rv$Sexp1[,"Position"],
                  y = rv$Sexp1[,"Intensity"],
                  line = list(color = rv$c3, width = 2), name ="Smooth Exp 1", opacity = 1) %>%
        add_lines(x = rv$Sexp2[,"Position"],
                  y = rv$Sexp2[,"Intensity"],
                  line = list(color = rv$c4, width = 2), name ="Smooth Exp 2", opacity = 1)
      
      if(!is.null(rv$ad) && nrow(rv$ad) != 0){
        g <- add_lines(g, x = rv$ad[,"Position"],
                    y = rv$ad[,"Intensity"],
                    line = list(color = rv$c5, width = 5), name ="Advanced", opacity = 1)
      }
      
      if(!is.null(rv$de) && nrow(rv$de) != 0){
        g <- add_lines(g, x = rv$de[,"Position"],
                    y = rv$de[,"Intensity"],
                    line = list(color = rv$c6, width = 5), name ="Delayed", opacity = 1)
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
    } else{
      return()
    }
  })

})

shinyApp(ui, server)
