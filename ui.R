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
                 sidebarPanel(
                             h3("Numerical Summaries"),
                             radioButtons("summVars", label = "Select Variables For Summaries", choiceValues = c("Plant", "Type", "Treatment", "conc", "uptake"), choiceNames = c("Plant", "Type", "Treatment", "conc", "uptake (Response)"), inline = TRUE),
                             br(),
                             conditionalPanel("input.summVars=='Plant' || input.summVars=='Type' || input.summVars=='Treatment'", 
                                               checkboxGroupInput("selectedNumSumm1", "Select Numerical Summary To Display", choices = c("contingency"), inline = TRUE, selected = NULL ),
                             ),
                             conditionalPanel("input.summVars=='conc' || input.summVars=='uptake'", 
                                              checkboxGroupInput("selectedNumSumm2", "Select Numerical Summary To Display (Correlation and Covariance is Between conc. and uptake)", choices = c("mean", "median", "IQR", "sd", "variance","correlation", "covariance"), inline = TRUE),
                             ),
                             actionButton("numType", "Get Numerical Summaries"),
                             br(),
                             h3("Graphical Summaries"),
                             conditionalPanel("input.summVars=='Plant' || input.summVars=='Type' || input.summVars=='Treatment' || input.summVars=='conc'", 
                                              radioButtons("plotType1", "Select Plot Type To Display", choices = c("BarPlot")),
                             ),
                             conditionalPanel("input.summVars=='uptake'", 
                                              radioButtons("plotType2", "Select Plot Type To Display (Scatterplot is between conc. v. uptake)", choices = c("Histogram", "BoxPlot", "ScatterPlot")),
                             ),
                             conditionalPanel("input.summVars=='uptake'", 
                                              radioButtons("fillBy", "Select Categorical Variable For Groupings", choices = c("Plant", "Type", "Treatment")),
                             ),
                             actionButton("plot", "Plot Summary"), 
                             br(),
                             br(),
                             h3("Data Table Selections"),
                             checkboxGroupInput("tableVars", "Select Variables For Table", choices = c("Plant", "Type", "Treatment", "conc", "uptake")),
                             radioButtons("groupbyVars", "Select Variables to Group By", choices = c("Plant", "Type", "Treatment", "conc")),
                             checkboxGroupInput("filterBy", "Select Variable to Filter By (Select ONLY 1)", choices = c("Treatment", "Type", "Plant")),
                             conditionalPanel("input.filterBy =='Treatment'",
                                              radioButtons("filterByVarsTreat", "Select Treatment Values To Filter By", choices = c("chilled", "nonchilled")),
                             ),
                             conditionalPanel("input.filterBy=='Type'",
                                              radioButtons("filterByVarsType", "Select Type Values To Filter By", choices = c("Quebec", "Mississippi")),
                             ),
                             conditionalPanel("input.filterBy=='Plant'",
                                              radioButtons("filterByVarsPlant", "Select Plant Values To Filter By", choices = c("Qn1", "Qn2", "Qn3", "Qc1", "Qc3", "Qc2", "Mn3", "Mn2", "Mn1", "Mc2", "Mc3", "Mc1")),
                             ),
                             actionButton("getTable", "Return Table")
                 ),
                 mainPanel(
                             h2("Numerical Summaries"),
                             br(),
                             tableOutput("numSumm"),
                             br(),
                             h2("Graphical Summaries"),
                             br(),
                             plotOutput("graphSumm"),
                             br(),
                             br(), 
                             h2("Data Table To Subset/Filter"),
                             tableOutput("tableSummary")
                   )
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
                              h5("Response = uptake"),
                              br(),
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
                              h1("Predict With Random Forest Model (Fit Model First!)"),
                              br(),
                              h4("-Select or Input Predictor Values"),
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
                              strong("Predicted Response(uptake): "),
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
                   checkboxInput("filterBy2", label = strong("Check to filter table by selection")),
                   conditionalPanel("input.filterBy2 == 1",
                                    radioButtons("selectedFilterBy", "Select Variable to Filter By", choices = c("Treatment", "Type", "Plant"), selected = NULL),
                                    conditionalPanel("input.selectedFilterBy =='Treatment'",
                                                     radioButtons("filterByVarsTreat2", "Select Treatment Values To Filter By", choices = c("chilled", "nonchilled"), selected = NULL),
                                    ),
                                    conditionalPanel("input.selectedFilterBy=='Type'",
                                                     radioButtons("filterByVarsType2", "Select Type Values To Filter By", choices = c("Quebec", "Mississippi"), selected = NULL),
                                    ),
                                    conditionalPanel("input.selectedFilterBy=='Plant'",
                                                     radioButtons("filterByVarsPlant2", "Select Plant Values To Filter By", choices = c("Qn1", "Qn2", "Qn3", "Qc1", "Qc3", "Qc2", "Mn3", "Mn2", "Mn1", "Mc2", "Mc3", "Mc1"), selected = NULL),
                                    )
                                    )
                   ,
                   actionButton("save", "Save Table To File")
                 ),
                 
                 # 
                 mainPanel(
                   dataTableOutput("table")
                 )
               )),
    )
))
