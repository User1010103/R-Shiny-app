# Define server logic required to draw the plots####################################################################################
server <- function(input,output){
  ### Welcome page ###################################################################################################################
  output$table <- renderTable({
    head(diabetes, input$rows)
  }) # End output table
  bs_themer() # Interactive tool to chose personal theme
  ### 1 variable plots ###############################################################################################################
  output$single_var_num_plot <- renderPlot({
    if (input$num_plot_type == "Histogram") {
      ggplot(data = diabetes,
             mapping = aes_string(input$single_var_num)) +
        geom_histogram(bins = input$bins , fill = input$color1 , color = "black") +
        theme_bw() +
        ggtitle(paste0("Histogram of ", input$single_var_num)) +
        theme(plot.title=element_text(hjust=0.5, face = "bold"), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
    } else if (input$num_plot_type == "Boxplot") {
      ggplot(data = diabetes,
             mapping = aes_string(y = input$single_var_num)) +
        geom_boxplot(fill = input$color1, color = "black") +
        theme_bw() +
        ggtitle(paste0("Boxplot of ", input$single_var_num)) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        coord_flip()
    } else if (input$num_plot_type == "Density Plot") {
      ggplot(data = diabetes,
             mapping = aes_string(input$single_var_num)) +
        geom_density(fill = input$color1, alpha = 0.5) +
        ggtitle(paste0("Density plot of ", input$single_var_num)) +
        theme_bw() +
        theme(plot.title = element_text(hjust=0.5, face = "bold"), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
    } # End if-loop
  }) # End output single_var_num_plot
  
  output$single_var_cat_plot <- renderPlot({
    if (input$cat_plot_type == "Pie Chart"){
      slices <- table(diabetes[[input$single_var_cat]])
      pie(slices,
          main = paste0("Pie Chart of ", input$single_var_cat),
          col = rainbow(length(slices))
      )
    } # End if-loop
  })# End output single_var_cat_plot 
  ### 2 variable plots ##############################################################################################################
  output$two_var_plot <- renderPlot({
    plotData2 <- ggplot(data = diabetes, mapping = aes_string(x = input$x_var, y = input$y_var))
    if (input$plot_type2 == "Scatterplot") {
      scatplot <- plotData2 +
        geom_point(color = input$color2, alpha = 0.6) +
        ggtitle(paste0("Scatterplot of ", input$x_var, "vs. ", input$y_var)) +
        theme_bw() + 
        theme(plot.title=element_text(hjust=0.5, face = "bold"))
      
      if (input$show_regression) {
        scatplot + geom_smooth(method = "lm", color = "red", linewidth = 2)
      } else {
        scatplot
      } # End if-loop
    } else if (input$plot_type2 =="Scatterplot with Boxplots") {
      # Scatterplot
      scatter_plot <- ggplot(data = diabetes, mapping = aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(color = input$color2, alpha = 0.6) +
        ggtitle(paste0("Scatterplot of ", input$x_var, " vs. ", input$y_var)) +
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
      if (input$show_regression) {
        scatter_plot <- scatter_plot + 
          geom_smooth(method = "lm", color = "red", linewidth = 2)
      } # End if-loop
      # X-axis boxplot
      x_boxplot <- ggplot(data = diabetes, mapping = aes_string(x = input$x_var)) +
        geom_boxplot(fill = input$color2, color = "black") +
        theme_bw()
      theme(plot.title = element_text(hjust=0.5, face = "bold"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
      # Y-axis boxplot
      y_boxplot <- ggplot(data = diabetes, mapping = aes_string(x = input$y_var)) +
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
        ggtitle(paste0("Violinplot of ", input$x_var, " vs. ", input$y_var)) +
        theme_bw() + 
        theme(plot.title=element_text(hjust=0.5, face = "bold"))
    } else if (input$plot_type2 == "Line Chart") {
      plotData2 +
        geom_line(color = input$color2, size = 1) +
        theme_bw() +
        theme(plot.title=element_text(hjust=0.5, face = "bold"))
    }# End if-loop
  }) # End output two_var_plot
  ### Multiple variable plots ########################################################################################################
  output$multi_var_plot <- renderPlot({
    if (input$plot_type3 == "Bubble Chart"){
      ggplot(data = diabetes,
             mapping = aes_string(x = input$x_multi, y = input$y_multi, size = input$third_var)) +
        geom_point(color = input$color3) +
        ggtitle(paste0("Bubble chart of ", input$x_multi, " vs. ", input$y_multi, " with bubble size of ", input$third_var)) +
        theme_bw() +
        theme(plot.title=element_text(hjust=0.5, face = "bold"))
    } else if (input$plot_type3 == "Heatmap") {
      ggplot(data = diabetes, 
             mapping = aes_string(x = input$x_multi, y = input$y_multi, fill = input$third_var)) + 
        geom_tile() +
        ggtitle(paste0("Heatmap: Relationship Between ", input$x_multi, " and ", input$y_multi, " with ", input$third_var)) +
        theme_bw() +
        theme(plot.title=element_text(hjust=0.5, face = "bold"))
    } # End if-loop
  })# End multi_var_plot
} # End server