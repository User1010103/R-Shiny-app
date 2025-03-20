# Define server logic required to draw the plots####################################################################################
server <- function(input,output, session){
  ### Welcome page ###################################################################################################################
  # Data reactive expression
  data <- reactive({
    req(input$upload)  # Ensure a file is uploaded
    
    # Get file extension
    ext <- tools::file_ext(input$upload$name)
    
    # Read the file based on its extension
    if (ext == "csv") {
      df <- read.csv(input$upload$datapath, stringsAsFactors = TRUE)
    } else if (ext %in% c("xls", "xlsx")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        install.packages("readxl")
        library(readxl)
      }
      df <- readxl::read_excel(input$upload$datapath) %>% as.data.frame()
    } else if (ext %in% c("tsv", "txt")) {
      df <- read.delim(input$upload$datapath, stringsAsFactors = TRUE)
    } else {
      stop("Unsupported file format")
    }
    
    # Convert character columns to factors if they have few unique values
    for (col in names(df)) {
      if (is.character(df[[col]]) && length(unique(df[[col]])) <= 50) {
        df[[col]] <- as.factor(df[[col]])
      }
    }
    
    return(df)
  })
  
  # Data separation functions
  categorical <- reactive({
    req(data())
    df <- data()
    df[, sapply(df, function(x) is.factor(x) | is.character(x)), drop = FALSE]
  })
  
  numerical <- reactive({
    req(data())
    df <- data()
    df[, sapply(df, is.numeric), drop = FALSE]
  })
  
  # Display data preview
  output$files <- renderTable({
    req(data())
    head(data(), 10)
  })
  
  # Update inputs when data changes
  observe({
    req(data())
    
    # Get column names
    num_cols <- names(numerical())
    cat_cols <- names(categorical())
    all_cols <- names(data())
    
    # Update single variable inputs
    if (length(num_cols) > 0) {
      updateSelectInput(session, "single_var_num", choices = num_cols, selected = num_cols[1])
    }
    
    if (length(cat_cols) > 0) {
      updateSelectInput(session, "single_var_cat", choices = cat_cols, selected = cat_cols[1])
    }
    
    # Update two-variable inputs
    updateSelectInput(session, "x_var", choices = all_cols, 
                      selected = if(length(num_cols) > 0) num_cols[1] else all_cols[1])
    updateSelectInput(session, "y_var", choices = all_cols, 
                      selected = if(length(num_cols) > 1) num_cols[2] else all_cols[min(2, length(all_cols))])
    
    # Update multi-variable inputs
    updateSelectInput(session, "x_multi", choices = all_cols, 
                      selected = if(length(num_cols) > 0) num_cols[1] else all_cols[1])
    updateSelectInput(session, "y_multi", choices = all_cols, 
                      selected = if(length(num_cols) > 1) num_cols[2] else all_cols[min(2, length(all_cols))])
    updateSelectInput(session, "third_var", choices = all_cols, 
                      selected = if(length(num_cols) > 2) num_cols[3] else all_cols[min(3, length(all_cols))])
  })
  ### 1 variable plots ###############################################################################################################
  output$single_var_num_plot <- renderPlot({
    plotData1 <- ggplot(data = data(), mapping = aes_string(input$single_var_num))
    if (input$num_plot_type == "Histogram") {
      plotData1 +
        geom_histogram(bins = input$bins , fill = input$color1 , color = "black") +
        theme_bw() +
        ggtitle(input$title1) +
        theme(plot.title=element_text(hjust=0.5, face = "bold"), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
    } else if (input$num_plot_type == "Boxplot") {
      plotData1 +
        geom_boxplot(fill = input$color1, color = "black") +
        theme_bw() +
        ggtitle(inpu$title1) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        coord_flip()
    } else if (input$num_plot_type == "Density Plot") {
      plotData1 +
        geom_density(fill = input$color1, alpha = 0.5) +
        ggtitle(inpu$title1) +
        theme_bw() +
        theme(plot.title = element_text(hjust=0.5, face = "bold"), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
    } # End if-loop
  }) # End output single_var_num_plot
  
  output$single_var_cat_plot <- renderPlot({
    if (input$cat_plot_type == "Pie Chart"){
      slices <- table(data()[[input$single_var_cat]])
      pie(slices,
          main = input$title_cat,
          col = rainbow(length(slices))
      )
    } # End if-loop
  })# End output single_var_cat_plot 
  ### 2 variable plots ##############################################################################################################
  output$two_var_plot <- renderPlot({
    plotData2 <- ggplot(data = data(), mapping = aes_string(x = input$x_var, y = input$y_var))
    if (input$plot_type2 == "Scatterplot") {
      scatplot <- plotData2 +
        geom_point(color = input$color2, alpha = 0.6) +
        ggtitle(input$title2) +
        theme_bw() + 
        theme(plot.title=element_text(hjust=0.5, face = "bold"))
      
      if (input$show_regression) {
        scatplot + geom_smooth(method = "lm", color = "red", linewidth = 2)
      } else {
        scatplot
      } # End if-loop
    } else if (input$plot_type2 =="Scatterplot with Boxplots") {
      # Scatterplot
      scatter_plot <- plotData2 +
        geom_point(color = input$color2, alpha = 0.6) +
        ggtitle(input$title2) +
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
      if (input$show_regression) {
        scatter_plot <- scatter_plot + 
          geom_smooth(method = "lm", color = "red", linewidth = 2)
      } # End if-loop
      # X-axis boxplot
      x_boxplot <- ggplot(data = data(), mapping = aes_string(x = input$x_var)) +
        geom_boxplot(fill = input$color2, color = "black") +
        theme_bw()
      theme(plot.title = element_text(hjust=0.5, face = "bold"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
      # Y-axis boxplot
      y_boxplot <- ggplot(data = data(), mapping = aes_string(x = input$y_var)) +
        geom_boxplot(fill = input$color2, color = "black") +
        coord_flip() +
        theme_bw()
      theme(plot.title = element_text(hjust=0.5, face = "bold"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
      # Arrange plots
      grid.arrange(x_boxplot, scatter_plot, y_boxplot, 
                   ncol = 2, nrow = 2, 
                   layout_matrix = rbind(c(NA, 1), c(3, 2)),
                   widths = c(1, 3), 
                   heights = c(1, 3))
    } else if (input$plot_type2 == "Violinplot"){
      plotData2 +
        geom_violin(fill = input$color2, color="black") +
        ggtitle(input$title2) +
        theme_bw() + 
        theme(plot.title=element_text(hjust=0.5, face = "bold"))
    } else if (input$plot_type2 == "Line Chart") {
      plotData2 +
        geom_line(color = input$color2, size = 1) +
        ggtitle(input$title2) +
        theme_bw() +
        theme(plot.title=element_text(hjust=0.5, face = "bold"))
    }# End if-loop
  }) # End output two_var_plot
  ### Multiple variable plots ########################################################################################################
  output$multi_var_plot <- renderPlot({
    if (input$plot_type3 == "Bubble Chart"){
      ggplot(data = data(), mapping = aes_string(x = input$x_multi, y = input$y_multi, size = input$third_var)) +
        geom_point(color = input$color3) +
        ggtitle(input$title3) +
        theme_bw() +
        theme(plot.title=element_text(hjust=0.5, face = "bold"))
    } else if (input$plot_type3 == "Heatmap") {
      ggplot(data = data(), mapping = aes_string(x = input$x_multi, y = input$y_multi, fill = input$third_var)) + 
        geom_tile() +
        scale_fill_gradient(low = "white", high = input$color3) +
        ggtitle(input$title3) +
        theme_bw() +
        theme(plot.title=element_text(hjust=0.5, face = "bold"))
    } else if (input$plot_type3 == "Correlation Plot") {
      # Get numerical data
      num_data <- numerical()
      # Remove NaN values
      num_data_complete <- na.omit(num_data)
      # Show message if there was data removed
      if (nrow(num_data_complete) < nrow(num_data)) {
        warning("Some rows with missing (NaN) values were excluded from the correlation plot.")
      }
      # Create correlation plot
      corrplot(cor(num_data_complete), 
               method = "circle", 
               type = "upper", 
               order = "hclust",
               tl.col = "black", 
               tl.srt = 45, 
               addCoef.col = "black",
               col = colorRampPalette(c("white", input$color3))(100),
               title = input$title3,
               mar = c(0, 0, 1, 0))
    }# End if-loop
  })# End multi_var_plot
} # End server