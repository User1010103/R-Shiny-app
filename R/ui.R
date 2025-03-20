# Import necessary libraries#########################################################################################################
library(shiny)                 # To make R shiny app
library(ggplot2)               # To make plots
library(colourpicker)          # Interactive tool for colors
library(bslib)                 # Interactive tool to set theme
library(gridExtra)             # Create plot grids for ggplot
library(corrplot)              # Correlation plot maker
library(dplyr)                 # Used to select numerical variables only for corplot

# Define UI for application that draws the plots####################################################################################
ui <- navbarPage(
  "Graph Designer",
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  #Welcome tab####################################################################################################################
  tabPanel("Welcome",
           sidebarLayout(
             sidebarPanel(
               fileInput("upload", "Upload Data File", buttonLabel = "Browse...",
                         accept = c(".csv", ".tsv", ".txt", ".xls"),
                         multiple = FALSE),
               tableOutput("files")
               ), # End sidebarPanel
             mainPanel(
               h3("Welcome to Graph Designer"),
               p("Upload your dataset to get started with data visualization."),
               p("This tool supports CSV, TSV, TXT, and Excel files."),
               tableOutput("files")
             ) # End mainPanel
           ) # End sidebarLayout
  ), # End Welcome tab
  #Single-var tab##################################################################################################################
  tabPanel("Single-variable plots", # Tab for Single-variable plots
           fluidPage(
             #Upper section
             fluidRow(
               column(4,
                wellPanel(
                   selectInput(inputId = "single_var_num", 
                               label = "Choose a numerical variable:", 
                               choices = NULL, 
                               selected = "Glucose"),
                   sliderInput(inputId = "bins",
                               label = "Number of bins",
                               min = 1, 
                               max = 50, 
                               value = 30),
                   colourInput(inputId = "color1",
                               label = "Choose color:",
                               value = "#FFC0CB"),
                   radioButtons(inputId = "num_plot_type", 
                                label = "Choose plot type:",
                                choices = c("Histogram", "Boxplot", "Density Plot"),
                                selected = "Histogram")
                ) # End Wellpanel
               ), # End column
               column(8,
                      h3("Visualizing a Single numerical variable"),
                      p("This section allows you to explore the distribution of a single numerical variable. 
                   You can generate histograms, boxplots or density plots. 
                   Adjust the number of bins for histograms and choose custom colors."),
                      plotOutput("single_var_num_plot", height = "400px")
               ) # End column
             ), # End fluidRow
             
             # Lower section
             fluidRow(
               column(4,
                wellPanel(
                   selectInput(inputId = "single_var_cat", 
                               label = "Choose a categorical variable:", 
                               choices = NULL, 
                               selected = "Age_Category"),
                   radioButtons(inputId = "cat_plot_type", 
                                label = "Choose plot type:",
                                choices = c("Pie Chart"),
                                selected = "Pie Chart")
                ), # End wellPanel
               ), # End column
               column(8,
                      h3("Visualizing a Single categorical variable"),
                      p("For categorical variables, pie charts and bar charts are available to show the proportion of each category in the dataset."),
                      plotOutput("single_var_cat_plot", height = "400px")
               ) # End column
             ), # End fluidRow
           ) # End fluidpage
  ), # End tabpanel Single-variable plots
  #two-var tab##################################################################################################################
  tabPanel("Two-variable plots", # Tab for Two-variable plots
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "x_var",
                           label =  "X-axis variable:",
                           choices = NULL,
                           selected = "Glucose"), # End input 1
               selectInput(inputId = "y_var",
                           label =  "Y-axis variable:",
                           choices = NULL,
                           selected = "BMI"), # End input 2
               colourInput(inputId = "color2",
                           label = "Choose color:",
                           value = "#FFC0CB"), # End color 2
               radioButtons(inputId = "plot_type2",
                            label = "Choose plot type:",
                            choices = c("Scatterplot", "Scatterplot with Boxplots", "Violinplot", "Line Chart"),
                            selected = "Scatterplot"), # End of radio button
               checkboxInput(inputId = "show_regression",
                             label = "Show regression line on scatterplots",
                             value = TRUE)
             ), # End sidebarPanel
             
             mainPanel(
               h3("Visualizing two variables"),
               p("This section allows you to analyze relationships between two variables."),
               tags$ul(
                 tags$li("Scatterplots help visualize trends between two numerical variables. You can optionally add a regression line to see potential correlations."),
                 tags$li("Violin plots provide insights into the distribution of a numerical variable across different categories."),
                 tags$li("Line charts are useful for analyzing trends over time or ordered data.")
               ),
               plotOutput("two_var_plot", height = "500px")
             )# End mainpanel
           )# End sidebarlayout
  ), # End tabpanel Two-variable plots
  
  #multi-var tab##################################################################################################################
  tabPanel("Multi-variable plots", # Tab for multi-variable plots
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "x_multi", 
                           label = "X-axis:", 
                           choices = NULL, 
                           selected = "Age"), # End input 1
               selectInput(inputId = "y_multi", 
                           label = "Y-axis:",
                           choices = NULL, 
                           selected = "Pregnancies"), # End input 2
               selectInput(inputId = "third_var", 
                           label =  "Third variable:", 
                           choices = NULL, 
                           selected = "Insulin"), # End input 3
               colourInput(inputId = "color3",
                           label = "Choose bubble color:",
                           value = "#FFC0CB"), # End color 3
               radioButtons(inputId = "plot_type3",
                            label = "Choose plot type:",
                            choices = c("Bubble Chart", "Heatmap"),
                            selected = "Bubble Chart") # End radio button
             ), # End sidebarPanel
             mainPanel(
               h3("Visualizing multiple variables"),
               p("This section is designed for more complex visualizations."),
               p("Bubble charts allow you to compare three numerical variables at once by encoding them into the x-axis, y-axis, and bubble size."),
               p("Heatmaps can be used to visualize relationships between two variables while incorporating a third variable as a color gradient."),
               p("Correlation plots show the relationships between all numerical variables in your dataset."),
               plotOutput("multi_var_plot", height = "500px")
             ) # End mainPanel
           )# End sidebarlayout
  ) # End tabpanel Multi-variable plots
) # End of navpage()