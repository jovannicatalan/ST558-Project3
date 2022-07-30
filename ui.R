#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# 
shinyUI(fluidPage(
    tabsetPanel(
      # About Page
      tabPanel("About", 
               h1("About Page"),
               "The purpose of this app is to allow the user to interact with the data through
               the use of widgets.They will be able to perform some data analysis, interact with
               the data and obtain predictions.",
               br(),
               br(),
               "In the data exploration page the user is able to perform data exploration such
               as numerical and graphical summaries.",
               "On the Modeling page the user is able to fit some models based on some configurations
               and is able to obtain some performance metrics and statistics for each model. In
               addition the user is able to perform some predictions based on data of their choice.",
               "Lastly on the Data page the user is able to see the data table and filter the rows
               by treatment, remove the columns they choose and save the table to a .csv file",
               br(),
               br(),
               "The data is about the carbon dioxide updake in grass plants from an experiment on the cold tolerance of the grass species Echinochloa
               crus-galli. The dataset has 84 observations and 5 variables. The 'plant' variable is a unique
               idetifier for each plant. The 'Type' variable provides the origin of the plant. The 'Treatment'
               variable indicates whether the plant was chilled or not. The 'conc' variable provides information
               on ambient CO2 concentrations(mL/L). The 'uptake' variable is the response, and provides information
               on the CO2 updake rates of the plants.",
               br(),
               br(),
               "The data being used is a dataset built in to R, see link below for more info.",
               br(),
               a("R CO2 Dataset", href="https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/CO2"),
               br(),
               img(src="RLogo.jpeg", alt="R Logo")),
      
      # Data Exploration Page
      tabPanel("Data Exploration", 
               titlePanel("Let's Explore The Data!"),
               sidebarLayout(
                 sidebarPanel("Select Variable For Numerical Summaries",
                 actionButton("plot", "Plot Summary")
                 ),
                 mainPanel(
                   h2("Numerical/Graphicall Summaries"),
                   br(),
                   br(),
                   plotOutput("summary"))
               )),
      
      # Modeling Page
      tabPanel("Modeling", 
               tabsetPanel(
                 tabPanel("Model Info", 
                          h1("Supervised Learning Models"),
                          "In this modeling section we will use three types of supervised learning models.
                          The first one is a multiple linear regression model. This method minimizes the sum
                          of squared errors and uses maximum likelihood to estimate the betas. This model is 
                          said to be linear in the betas and not necessarily in the predictors. This is an
                          extension of simple linear regression where more than one variable can be used as
                          predictors. It can include higher order terms and interaction terms. This is the easiest
                          and simplest methods and usually where you want to start first. This is not as variable
                          or flexible as the other two methods. You would this method if ease of implementation was
                          the main concern but generally the other two provide better predictions.",
                          br(),
                          br(),
                          "The second one is regression tree. This method
                          splits up the predictor space into regions and uses the average(in our case) of the
                          specific region as the prediction. This approach is more flexible but has a higher
                          variance the MLR model. A drawback to trees is that they require pruning in order to
                          avoid overfitting to the training data. This increases bias but decreases variance in
                          hopes of improving predictions. Other benefits are that trees are easy to interpret,
                          no scaling is needed in the predictors, no assumptions are needed and contains built in
                          variable selection. Some more cons are that you can very different trees based on minor
                          changes, and that there is no optimal algorithm to choose your tree.",
                          br(),
                          br(),
                          "The third and last one is random forest model. This method builds on top of trees and
                          averages across many fitted trees. This leads to a decreased variance compared to a single
                          tree. It uses bootstrap samples to fit on. With this method you lose interpretation though.
                          It also uses a random subset of predictors for each bootstrap/ sample fitted tree. This decreases
                          correlation in case of strong predictors used in every tree fit, leading to a greater reduction in
                          variance. This method is usually the best at predicting"),
                 
                 tabPanel("Model Fitting", 
                          titlePanel("Fit 3 Different Models"),
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("vars", label = "Select Variables For Model Fitting (Use all by default)", choices = c("Plant", "Type", "Treatment", "conc"), inline = TRUE),
                              radioButtons("p", "Proportion For Training", choices = seq(.1,.9,by=.1), inline = TRUE),
                              radioButtons("cvNumFolds", "Specify CV Fold Number", choices = c(2, 3, 4, 5), inline = TRUE),
                              actionButton("fit", label = "Fit Models")
                              ),
                            mainPanel(
                              h4("Metrics RMSE"),
                              "Linear Model: ",
                              textOutput("metricsLM"),
                              "Tree Model: ",
                              textOutput("metricsTree"),
                              "Random Forest Model: ",
                              textOutput("metricsRF"),
                              br(),
                              br(),
                              h4("Linear Model Fit Summary"),
                              uiOutput("lmSummary"),
                              br(),
                              h4("Tree CP v. RMSE"),
                              plotOutput("treeSplits"),
                              br(),
                              h4("Random Forest Variable Importance"),
                              plotOutput("rfVarImport")
                            )
                          )),
                 
                 tabPanel("Prediction",
                          sidebarLayout(
                            sidebarPanel(
                              h1("Predict With Random Forest Model"),
                              br(),
                              br(),
                              h4("Select or Input Predictor Values"),
                              br(),
                              radioButtons("plantValue", "Plant", choices = c("Qn1", "Qn2", "Qn3", "Qc1", "Qc3", "Qc2", "Mn3", "Mn2", "Mn1", "Mc2", "Mc3", "Mc1"), inline = TRUE),
                              radioButtons("treatmentValue", "Treatment", choices = c("nonchilled", "chilled")),
                              radioButtons("typeValue", "Type", choices = c( "Quebec", "Mississippi")),
                              numericInput("concValue", "Concentration", value = 10),
                              br(),
                              actionButton("predict", "Predict!")
                            ),
                            mainPanel(
                              h4("Predicted Response Based on Provided Values"),
                              "- Only variables used for model fitting will apply",
                              br(),
                              br(),
                              strong("Predicted uptake: "),
                              br(),
                              textOutput("predResponse")
                            )
                          )
                          )
               )),
      
      # Data Page
      tabPanel("Data", 
               
               # Application title
               titlePanel("Data"),
               
               # Sidebar
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("colsToRemove", label = "Remove Columns", choices = c("Plant", "Type", "Treatment", "conc", "uptake")),
                   checkboxInput("filterByTreatment", label = strong("Check to filter by Treatment")),
                   conditionalPanel("input.filterByTreatment==1",
                                    radioButtons("treatmentFilter", label = "Filter By Treatment", choices = c("chilled", "nonchilled"))),
                   actionButton("save", "Save Table To File")
                 ),
                 
                 # 
                 mainPanel(
                   dataTableOutput("table")
                 )
               )),
    )
))
