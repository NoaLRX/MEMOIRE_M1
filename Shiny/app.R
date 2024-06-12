library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(htmlTable)

prev = read.csv2("DATA/prev_df.csv",sep=",")
prev <- subset(prev, select = -c(X, LM.1, id))
prev <- as.data.frame(lapply(prev, as.numeric))
tableau = read.csv2("DATA/tableau.csv",sep=",")
tableau <- tableau[tableau$X != "LM.1", ]
colnames(tableau)[colnames(tableau) == "CCSED"] <- "CSSED"

tableau[tableau$X == "LSTM_CNN", "CSSED"] <- 45.31
prev <- rename(prev, FAO = y_real)


# Define the mapping between 'prev' and 'tableau' model names
model_mapping <- data.frame(
  prev_name = c("CSPEarmax", "CSPEarx", "CSPElm", "CSPEnaive", "CSPEarxget", "CSPEar1", "CSPEGAM", "CSPEMLP", 
                "CSPEMARS", "CSPESVM", "CSPERF", "CSPEXGB", "CSPEkNN", "CSPELSTM", "CSPELSTMCNN", "CSPETDNN"),
  tableau_name = c("ARMAX", "ARX", "LM", "NAIVE", "ARX_GET", "AR1", "GAM", "MLP", 
                   "MARS", "SVM", "RF", "XGB", "kNN", "LSTM", "LSTM_CNN", "TDNN"),
  stringsAsFactors = FALSE
)


tableau$MSE <- as.numeric(as.character(tableau$MSE))
tableau$RMSE <- as.numeric(as.character(tableau$RMSE))
tableau$CSSED <- as.numeric(as.character(tableau$CSSED))
tableau$R2_OOS <- as.numeric(as.character(tableau$R2_OOS))
prev$CSPEnaive <- cumsum((prev$FAO - prev$NAIVE)^2)
prev$CSPEarmax <- cumsum((prev$FAO - prev$ARMAX)^2)
prev$CSPEarx <- cumsum((prev$FAO - prev$ARX)^2)
prev$CSPElm <- cumsum((prev$FAO - prev$LM)^2)
prev$CSPEarxget <- cumsum((prev$FAO - prev$ARX_GET)^2)
prev$CSPEar1 <- cumsum((prev$FAO - prev$AR1)^2)
prev$CSPEGAM <- cumsum((prev$FAO - prev$GAM)^2)
prev$CSPEMLP <- cumsum((prev$FAO - prev$MLP)^2)
prev$CSPEMARS <- cumsum((prev$FAO - prev$MARS)^2)
prev$CSPESVM <- cumsum((prev$FAO - prev$SVM)^2)
prev$CSPERF <- cumsum((prev$FAO - prev$RF)^2)
prev$CSPEXGB <- cumsum((prev$FAO - prev$XGB)^2)
prev$CSPEkNN <- cumsum((prev$FAO - prev$kNN)^2)
prev$CSPELSTM <- cumsum((prev$FAO - prev$LSTM)^2)
prev$CSPELSTMCNN <- cumsum((prev$FAO - prev$LSTM_CNN)^2)
prev$CSPETDNN <- cumsum((prev$FAO - prev$TDNN)^2)

source_data <- data.frame(
  Variable = c("Food", "Oil", "VIX", "OVX", "GPR", "Fertil", "Metals", "Temp", "Raw", "Sugar", "Wheat", "Vege Oil"),
  Nom = c("Fao Food Price Index", "Crude Oil Price WTI", "CBOE Volatility Index", "CBOE Crude Oil Volatility Index", "Geopolitical Risk (GPR) Index", "Monthly Indices - Fertilizers", "Monthly Indices - Metals", "Temperature Anomalies", "Raw Materials", "ICE - No. 11 Sugar Futures", "US Wheat Futures", "US Soybean Oil Futures"),
  Source = c("https://www.fao.org/worldfoodsituation/foodpricesindex/en/", "https://fred.stlouisfed.org/series/MCOILWTICO", "https://finance.yahoo.com/quote/%5EVIX/history?period1=633830400&period2=1669852800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true", "https://finance.yahoo.com/quote/%5EOVX/history?p=%5EOVX", "https://www.matteoiacoviello.com/gpr.htm", "https://www.worldbank.org/en/research/commodity-markets#1", "https://www.worldbank.org/en/research/commodity-markets#1", "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series/globe/ocean/all/12/1880-2022", "https://www.worldbank.org/en/research/commodity-markets#1", "https://fr.investing.com/commodities/us-sugar-no11-historical-data", "https://www.investing.com/commodities/us-wheat-historical-data", "https://www.investing.com/commodities/us-soybean-oil-historical-data")
)


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .center_plot {
        display: flex;
        justify-content: center;
      }
      .control_panel {
        margin-right: 50px; // ajustez cette valeur en fonction de vos besoins
      }
    "))
  ),
  tabsetPanel(
    tabPanel("Accueil",
             fluidRow(
               column(8, offset = 2, # réduire la largeur à 8 et centrer la colonne
                      div(class = "center_text", # ajouter la classe pour centrer le texte
                          h2("Présentation des modèles de prévision sur le FAO Food Price Index",style = "font-size: 50px"),
                          p("Dans  le  contexte  d’instabilité  croissante  des  prix  des  denrées  alimentaires,  cette  étude  vise  à 
élaborer  divers  modèles  de  prévision,  tant  économétriques  que  basés  sur  le  Machine 
Learning,  afin  de  faire  des  prévisions  sur  le  prix  des  denrées  alimentaires  (Fao  Food  Price 
Index).  Ces  modèles  seront  établis  à  partir  de  six  variables  explicatives,  recourant  à  des 
données  mensuelles  couvrant  la  période  allant  de  janvier  1999  à  décembre  2022.  Les 
prévisions  sont  effectuées  en  allouant  80%  des  données  à  l’entraînement  et  20%  aux  tests, 
en  utilisant  un  échantillonnage  non  aléatoire.  Les  conclusions  de  notre  analyse  indiquent  que le  modèle  économétrique  ARX  Gets  surpasse  les  autres,  bien  que  les  modèles  XGB  Boost  et LSTM  ne  soient  pas  loin  derrière  en  termes  de  performances.",style = "font-size: 20px"),
                          h2("Source des données",style = "font-size: 40px")
                      ),
                      uiOutput("sourceData")
               )
             )
    ),
    tabPanel("Prévision",
             fluidRow(
               column(9, plotlyOutput("modelPlot", height = 1000)),
               column(3, wellPanel(
                 checkboxInput("ml_models", "Modèles ML", value = TRUE),
                 checkboxInput("econ_models", "Modèles Économétriques", value = TRUE),
                 sliderInput("rmse", "Seuil RMSE", min = 1.5, max = 2.50, value = 2.40, step = 0.05),
                 sliderInput("cssed", "Seuil CSSED", min = 0, max = 200, value = 100, step = 20),
                 sliderInput("r2_oos", "Seuil R2_OOS", min = 0, max = 0.9, value = 0.9, step = 0.10)
               ))
             )
    ),
    tabPanel("Évaluation",
             div(class = "center_plot", 
                 fluidRow(
                   column(9, plotlyOutput("evaluationPlot", height = 1000, width = 1000 )),
                   column(3, div(class = "control_panel", 
                                 wellPanel(
                                   checkboxInput("ml_models_eval", "Modèles ML", value = TRUE),
                                   checkboxInput("econ_models_eval", "Modèles Économétriques", value = TRUE),
                                   sliderInput("rmse_eval", "Seuil RMSE", min = 1.5, max = 2.50, value = 2.40, step = 0.05),
                                   sliderInput("cssed_eval", "Seuil CSSED", min = 0, max = 200, value = 100, step = 20),
                                   sliderInput("r2_oos_eval", "Seuil R2_OOS", min = 0, max = 0.9, value = 0.9, step = 0.10),
                                   sliderInput("cspe_eval", "Seuil CSPE", min = 0, max = max(prev), value = max(prev), step = 0.10)
                                 ))
                   )
                 )
             )
    ),
    tabPanel("Résumé",
             DT::dataTableOutput("summaryTable")
    )
  )
)



server <- function(input, output, session) {
  output$sourceData <- renderUI({
    source_data$Source <- sapply(source_data$Source, function(x) paste0('<a href="', x, '">', x, '</a>'))
    htmlTable(source_data, rnames=FALSE, align = 'l', 
              css.cell = "padding-left: .5em; padding-right: .2em; padding-top: .2em; padding-bottom: .2em; border: 1px solid #dddddd;",
              css.rgroup = "border-top: 1px solid; padding-top: .5em; padding-bottom: .3em;")
  })
  output$modelPlot <- renderPlotly({
    
    # Initialize empty vector for selected models
    selected_models <- c()
    
    # Include ML models if checkbox is checked
    if (input$ml_models) {
      ml_models <- c("kNN", "LSTM", "LSTM_CNN", "MARS", "MLP", "RF", "SVM", "TDNN", "XGB")
      selected_models <- union(selected_models, ml_models)
    }
    
    # Include econometric models if checkbox is checked
    if (input$econ_models) {
      econ_models <- c("AR1", "ARMAX", "ARX_GET", "GAM", "LM", "NAIVE")
      selected_models <- union(selected_models, econ_models)
    }
    
    # Filter models based on RMSE, CSSED, R2_OOS values
    filtered_models <- tableau %>%
      filter(RMSE <= input$rmse) %>%
      filter(CSSED <= input$cssed) %>%
      filter(R2_OOS <= input$r2_oos) %>%
      pull(X)
    
    # Keep only the models that have passed both the checkbox and slider conditions
    selected_models <- intersect(selected_models, filtered_models)
    
    # Always include "FAO" model
    selected_models <- union(selected_models, "FAO")
    
    # Prepare data for plotting
    data_to_plot <- prev %>%
      select(all_of(selected_models))
    
    data_to_plot <- data_to_plot %>%
      mutate(Time = row_number())
    
    data_to_plot <- pivot_longer(data = data_to_plot, cols = -Time, names_to = "Model", values_to = "Value")
    
    # Create a plotly object
    p <- plot_ly()
    
    # Define color palette
    model_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF", "#FF8000", "#008000",
                      "#800080", "#FFC0CB", "#00CED1", "#8B0000", "#8A2BE2", "#ADFF2F", "#FFA500", "#008080")
    
    # Create named list of model colors
    model_color_list <- setNames(model_colors, c("ARMAX", "ARX", "LM", "NAIVE", "ARX_GET", "AR1", "GAM", "MLP", 
                                                 "MARS", "SVM", "RF", "XGB", "kNN", "LSTM", "LSTM_CNN", "TDNN"))
    
    # Add a trace for each model
    for (i in 1:length(unique(data_to_plot$Model))) {
      model <- unique(data_to_plot$Model)[i]
      model_data <- subset(data_to_plot, Model == model)
      
      if (model == "FAO") {
        color <- "black"
        width <- 9
        opacity <- 1
      } else {
        # Convert color from hexadecimal to RGB
        rgb_color <- col2rgb(model_color_list[model])
        # Convert color from RGB to hexadecimal with opacity
        color <- rgb(rgb_color[1,]/255, rgb_color[2,]/255, rgb_color[3,]/255, alpha = 0.7, maxColorValue = 1)
        width <- 4 
      }
      
      p <- add_trace(
        p,
        x = model_data$Time, 
        y = model_data$Value, 
        name = model, 
        type = "scatter", 
        mode = "lines",
        line = list(color = color, width = width),
        hoverinfo = 'name+y',
        hoverlabel = list(bgcolor = color)
      )
    }
    
    
    
    # Add layout information
    p <- layout(p,
                title = list(text = 'Modèles de prévisions', 
                             font = list(size = 24, color = 'black', family = 'Arial', bold = TRUE)),
                xaxis = list(title = "Time"),
                yaxis = list(title = "Value"),
                margin = list(t = 80),
                legend = list(orientation = "v", 
                              x = 1, 
                              y = 1, 
                              traceorder = "normal",
                              font = list(family = "sans-serif", 
                                          size = 12, 
                                          color = "black"),
                              bgcolor = "white",
                              bordercolor = "Black",
                              borderwidth = 2),
                plot_bgcolor = 'rgb(242, 242, 242)')  # Set plot background color to light gray
    
    p
    
  })
  
  output$evaluationPlot <- renderPlotly({
    # Select the CSPE columns based on checkbox inputs
    selected_models <- c()
    if(input$ml_models_eval){
      ml_models <- c("CSPEMLP", "CSPEMARS", "CSPESVM", "CSPERF", "CSPEXGB", "CSPEkNN", "CSPELSTM", "CSPELSTMCNN", "CSPETDNN")
      selected_models <- union(selected_models, ml_models)
    }
    if(input$econ_models_eval){
      econ_models <- c("CSPEnaive", "CSPEarmax", "CSPEarx", "CSPElm", "CSPEarxget", "CSPEar1", "CSPEGAM")
      selected_models <- union(selected_models, econ_models)
    }
    
    # Filter models based on RMSE, CSSED, R2_OOS and CSPE values
    filtered_models <- tableau %>%
      filter(RMSE <= input$rmse_eval) %>%
      filter(CSSED <= input$cssed_eval) %>%
      filter(R2_OOS <= input$r2_oos_eval) %>%
      pull(X)
    
    # Map 'prev' model names to 'tableau' model names using the mapping table
    filtered_models_prev <- model_mapping$prev_name[match(filtered_models, model_mapping$tableau_name)]
    
    # Filter models based on CSPE value
    filtered_models_cspe <- colnames(prev)[apply(prev, 2, max) <= input$cspe_eval]
    
    # Keep only the models that have passed both the checkbox and slider conditions
    selected_models <- intersect(selected_models, filtered_models_prev)
    selected_models <- intersect(selected_models, filtered_models_cspe)
    
    # Filter based on CSPE threshold
    cspe_filtered_models <- prev %>%
      filter_all(all_vars(. <= input$cspe_eval)) %>%
      colnames()
    
    # Keep only the models that have passed all the conditions
    selected_models <- intersect(selected_models, cspe_filtered_models)
    
    # Prepare data for plotting
    data_to_plot <- prev %>%
      select(all_of(selected_models))
    
    data_to_plot <- data_to_plot %>%
      mutate(Time = row_number())
    
    data_to_plot <- pivot_longer(data = data_to_plot, cols = -Time, names_to = "Model", values_to = "CSPE")
    
    # Create a plotly object
    p <- plot_ly()
    
    # Define color palette
    model_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF", "#FF8000", "#008000",
                      "#800080", "#FFC0CB", "#00CED1", "#8B0000", "#8A2BE2", "#ADFF2F", "#FFA500", "#008080")
    
    # Create named list of model colors
    model_color_list <- setNames(model_colors, c("CSPEarmax", "CSPEarx", "CSPElm", "CSPEnaive", "CSPEarxget", "CSPEar1", "CSPEGAM", "CSPEMLP", 
                                                 "CSPEMARS", "CSPESVM", "CSPERF", "CSPEXGB", "CSPEkNN", "CSPELSTM", "CSPELSTMCNN", "CSPETDNN"))
    
    # Add a trace for each model
    for (i in 1:length(unique(data_to_plot$Model))) {
      model <- unique(data_to_plot$Model)[i]
      model_data <- subset(data_to_plot, Model == model)
      color <- model_color_list[model]  # Use color list
      width <- 4  # Set line width to 4
      p <- add_trace(
        p,
        x = model_data$Time, 
        y = model_data$CSPE, 
        name = model, 
        type = "scatter", 
        mode = "lines",
        line = list(color = color, width = width)
      )
    }
    
    # Add layout information
    p <- layout(p,
                title = list(text = 'Évolution du CSPE des modèles', 
                             font = list(size = 24, color = 'black', family = 'Arial', bold = TRUE)),
                xaxis = list(title = "Time"),
                yaxis = list(title = "CSPE"),
                margin = list(t = 80),
                legend = list(orientation = "v", 
                              x = 1, 
                              y = 1, 
                              traceorder = "normal",
                              font = list(family = "sans-serif", 
                                          size = 12, 
                                          color = "black"),
                              bgcolor = "white",
                              bordercolor = "Black",
                              borderwidth = 2))
    
    p
  })
  
  rownames(tableau) <- tableau$X
  output$summaryTable <- DT::renderDataTable({
    DT::datatable(tableau[, -1], # Remove the X column, it's now rownames
                  rownames = TRUE, 
                  extensions = 'Buttons',
                  options = list(pageLength = 25, 
                                 autoWidth = FALSE, 
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
                  class = 'stripe hover', # For striped rows
                  caption = htmltools::tags$caption(style = "caption-side: top; font-size: 150%; color: black; text-align: center; font-weight: bold;", 
                                                    "Tableau résumé des indicateurs de qualité de prévision des différents modèles"))
  })
  
  
}




shinyApp(ui, server)
