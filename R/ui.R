# Import necessary libraries#########################################################################################################
library(shiny)                 # To make R shiny app
library(ggplot2)               # To make plots
library(colourpicker)          # Interactive tool for colors
library(bslib)                 # Interactive tool to set theme
library(gridExtra)             # Create plot grids for ggplot
library(corrplot)              # Correlation plot maker
library(dplyr)                 # Used to select numerical variables only for corplot

# Import dataset#####################################################################################################################
df <- read.csv("xxx.csv", header = TRUE)

# Pre-processing data ###############################################################################################################
#Devide numerical and categorical variables
categorical <- diabetes[sapply(diabetes, function(x) is.factor(x) | is.character(x))]
numerical <- diabetes[, sapply(diabetes, is.numeric)]

# Define UI for application that draws the plots####################################################################################
ui <- navbarPage(
  "BIT08 Assignment",
  theme = bs_theme(),
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  #Welcome tab####################################################################################################################
  tabPanel("Welcome",
           sidebarLayout(
             sidebarPanel(img(src = "funny.png")),
             mainPanel(
               HTML(r"(
                <h2>Welcome to the BIT08 Assignment Shiny App</h2>
                <p>
                    This interactive application was developed for the BIT08 assignment to explore and visualize a diabetes dataset.
                    It allows users to generate insightful plots and examine relationships between various variables. 
                    The dataset includes clinical measurements such as glucose levels, BMI, insulin levels, and more.
                </p>
                <h3>Objective</h3>
                <p>
                    The primary goal of this app is to help users understand patterns within the dataset through visual exploration.
                    This includes:
                    <ul>
                        <li>Single-variable distributions</li>
                        <li>Relationships between two variables</li>
                        <li>Multi-variable interactions</li>
                    </ul>
                </p>
                <h3>Dataset Overview</h3>
                <p>
                    The dataset contains the following key variables:
                    <ul>
                        <li><b>Pregnancies</b> - Number of times a patient has been pregnant</li>
                        <li><b>Glucose</b> - Blood glucose concentration </li>
                        <li><b>Blood Pressure</b> - Blood pressure </li>
                        <li><b>Skin Thickness</b> - Skin Tickness </li>
                        <li><b>Insulin</b> - Insulin </li>
                        <li><b>BMI</b> - Body Mass Index </li>
                        <li><b>Diabetes Pedigree Function</b> - Likelihood of diabetes based on family history </li>
                        <li><b>Age</b> - Age in years </li>
                        <li><b>Outcome</b> - Diabetes diagnosis represented as a binary value (1 = Patient has diabetes, 0 = Patient does not have diabetes) </li>
                        <li><b>Pregnancy_Category</b> - Pregnancies devided in 5 groups:</li>
                            <ul>
                              <li>Nulliparous: 0 pregnancies</li>
                              <li>Low Parity: 0-3 pregnancies</li>
                              <li>Moderate Parity: 3-6 pregnancies</li>
                              <li>High Parity: 6-10 pregnancies</li>
                              <li>Grand Multiparity: 10-17 pregnancies</li>
                            </ul>
                        <li><b>BMI_Category</b> - </li>
                            <ul>
                              <li>Underweight: BMI < 18.5</li>
                              <li>Normal weight: BMI between 18.5 and 24.9</li>
                              <li>Overweight: BMI between 25 and 29.9</li>
                              <li>Obese: BMI > 30</li>
                            </ul>
                        <li><b>Age_Category</b> - </li>
                            <ul>
                              <li>Young Adult: 20-35 years old</li>
                              <li>Middle-aged Adult: 35-50 years old</li>
                              <li>Mature Adult: 50-65 years old</li>
                              <li>Senior: 65-81 years old</li>
                            </ul>
                        <li><b>Outcome2</b> - Diabetes Diagnosis (Yes = Patient has diabetes, No = Patient does not have diabetes)</li>
                    </ul>
                </p>
                <p>
                    A few categorical variables were manually created for better visualization. Unusual values, such as a BMI of 0, 
                    were removed before designing the Shiny app. These modifications can be found within the app folder as "pre-procesisingDiabetes.R".
                </p>
            )"), # End of HTML
            numericInput(inputId = "rows",
                         label = "Number of rows to display:",
                         value = 10,
                         min = 1,
                         max = nrow(diabetes)),
            tableOutput("table")
             ) # End mainPanel
           ) # End sidebarLayout
  ), # End Welcome tab
  #Single-var tab##################################################################################################################
  tabPanel("Single-variable plots", # Tab for Single-variable plots
           layout_columns(
             #Upper section
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "single_var_num", 
                             label = "Choose a numerical variable:", 
                             choices = names(numerical), 
                             selected = "Glucose"), # End input
                 sliderInput(inputId = "bins",
                             label = "Number of bins",
                             min = 1, 
                             max = 50, 
                             value = 30), # End slider
                 colourInput(inputId = "color1",
                             label = "Choose color:",
                             value = "#FFC0CB"), # End color
                 radioButtons(inputId = "num_plot_type", 
                              label = "Choose plot type:",
                              choices = c("Histogram", "Boxplot", "Density Plot"),
                              selected = "Histogram"),  # End radio button
               ), # End sidebar panel
               mainPanel(
                 h3("Visualizing a Single numerical variable"),
                 HTML("<p>This section allows you to explore the distribution of a single numerical variable. 
                  You can generate histograms, boxplots or density plots. 
                  Adjust the number of bins for histograms and choose custom colors.</p>"),
                 plotOutput("single_var_num_plot")
               ) # End mainpanel
             ), # End sidebarlayout
             
             # Lower section
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "single_var_cat", 
                             label = "Choose a categorical variable:", 
                             choices = names(categorical), 
                             selected = "Age_Category"), # End input
                 radioButtons(inputId = "cat_plot_type", 
                              label = "Choose plot type:",
                              choices = c("Pie Chart"),
                              selected = "Pie Chart"),  # End radio button
               ), # End sidebar panel
               mainPanel(
                 h3("Visualizing a Single categorical variable"),
                 HTML("<p>For the categorical variables, a pie chart is available to show the proportion of each category in the dataset.</p>"),
                 plotOutput("single_var_cat_plot")
               ) # End mainpanel
             ), # End sidebarlayout
             col_widths = c(12,12)
           ), # End layout columns
  ), # End tabpanel Single-variable plots
  #two-var tab##################################################################################################################
  tabPanel("Two-variable plots", # Tab for Two-variable plots
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "x_var",
                           label =  "X-axis variable:",
                           choices = names(diabetes),
                           selected = "Glucose"), # End input 1
               selectInput(inputId = "y_var",
                           label =  "Y-axis variable:",
                           choices = names(diabetes),
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
             
             mainPanel(h3("Visualizing two variables"),
                       HTML(r"(<p>This section allows you to analyze relationships between two variables.
                             <ul>
                                  <li>Scatterplots help visualize trends between two numerical variables. You can optionally add a regression line to see potential correlations.</li>
                                  <li>Violin plots provide insights into the distribution of a numerical variable across different categories.</li>
                                  <li>Line charts are useful for analyzing trends over time or ordered data.</li>
                            </ul>
                            </p>
                            )"),
                       plotOutput("two_var_plot"),
             )# End mainpanel
           )# End sidebarlayout
  ), # End tabpanel Two-variable plots
  
  #multi-var tab##################################################################################################################
  tabPanel("Multi-variable plots", # Tab for multi-variable plots
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "x_multi", 
                           label = "X-axis:", 
                           choices = names(diabetes), 
                           selected = "Age"), # End input 1
               selectInput(inputId = "y_multi", 
                           label = "Y-axis:",
                           choices = names(diabetes), 
                           selected = "Pregnancies"), # End input 2
               selectInput(inputId = "third_var", 
                           label =  "Third variable:", 
                           choices = names(diabetes), 
                           selected = "Insulin"), # End input 3
               colourInput(inputId = "color3",
                           label = "Choose bubble color:",
                           value = "#FFC0CB"), # End color 3
               radioButtons(inputId = "plot_type3",
                            label = "Choose plot type:",
                            choices = c("Bubble Chart", "Heatmap"),
                            selected = "Bubble Chart") # End radio button
             ), # End sidebarPanel
             mainPanel(h3("Visualizing multiple variables"),
                       HTML(r"(
                              <p>This section is designed for more complex visualizations. 
                              The Bubble charts allow you to compare three numerical variables at once by encoding them into the x-axis, y-axis, and bubble size.
                              Whereas the heatmaps can be used to visualize relationships between two variables while incorporating a third variable as a color gradient.</p>
                         )"), # End HTML
                       plotOutput("multi_var_plot"))
           )# End sidebarlayout
  ) # End tabpanel Multi-variable plots
) # End of navpage()