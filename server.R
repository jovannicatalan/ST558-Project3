#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(randomForest)
library(rpart)
library(caret)
library(sjPlot)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
 dataTable <- reactive({
   table <- as_tibble(CO2)
   if(input$filterBy2){
     if(input$selectedFilterBy == "Treatment"){
       table <- as_tibble(CO2) %>%
         select(-input$colsToRemove) %>%
         filter(Treatment == input$filterByVarsTreat2)
     }
     if(input$selectedFilterBy == "Type"){
       table <- as_tibble(CO2) %>%
         select(-input$colsToRemove) %>%
         filter(Type == input$filterByVarsType2)
     }
     if(input$selectedFilterBy == "Plant"){
       table <- as_tibble(CO2) %>%
         select(-input$colsToRemove) %>%
         filter(Plant == input$filterByVarsPlant2)
     }
   } else {
      table <- as_tibble(CO2) %>% 
     select(-input$colsToRemove)
   }
   table
 })
 
 
 output$table <- renderDataTable(
   dataTable()
 )
 
 observeEvent(input$save, {
   write.csv(dataTable(), "./CO2data.csv", row.names = FALSE)
 })
 
 
 CO2Train <- reactive({
   vars <- c(input$vars, "uptake")
   
   if (length(vars) > 1) {
     CO2_sub <- as_tibble(CO2) %>% select(vars)
   } else {
     CO2_sub <- as_tibble(CO2)
   }
   
   # Train/Test Splits
   trainIndex <- createDataPartition(CO2$uptake, p = as.numeric(input$p), list = FALSE)
   CO2Train <- CO2_sub[trainIndex, ]
   CO2Train
 })
 
 lmFit <- eventReactive(input$fit, {
   #Fits
   lm <- lm(uptake ~ ., data = CO2Train())
 })
 treeFit <- eventReactive(input$fit, {
   #Fits
   tree <- train(uptake ~ ., data = CO2Train(),
                 method = "rpart",
                 preProcess = c("center", "scale"),
                 trControl = trainControl(method = "cv", number =as.numeric(input$cvNumFolds)),
                 tuneGrid = data.frame(cp = seq(.01, 1, by=.01)))
 })
 rfFit <- eventReactive(input$fit,{
   #Fits
   rf <- train(uptake ~ ., data = CO2Train(),
               method = "rf",
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "cv", number =as.numeric(input$cvNumFolds)),
               tuneGrid = data.frame(mtry = 1:7))
 })
 
 # Calculate RMSEs
 lmRMSE <- eventReactive(input$fit, {
   sqrt(mean(lmFit()$residuals^2))
 })
 output$metricsLM <- renderText(
   lmRMSE()
 )
 treeRMSE <- eventReactive(input$fit, {
   as.character(postResample(predict(treeFit(), CO2Train()), CO2Train()$uptake)[1])
 })
 output$metricsTree <- renderText(
   treeRMSE()
 )
 rfRMSE <- eventReactive(input$fit, {
   as.character(postResample(predict(rfFit(), CO2Train()), CO2Train()$uptake)[1])
 })
 
 # Render RMSEs
 output$metricsRF <- renderText(
   rfRMSE()
 )
 # Render Model Summaries/Plots
 output$lmSummary <- renderUI({
   withMathJax(
     paste0(
       "Adj. \\( R^2 = \\) ", round(summary(lmFit())$adj.r.squared, 3),
       ", \\( \\beta_0 = \\) ", round(lmFit()$coef[[1]], 3),
       ", \\( \\beta_1 = \\) ", round(lmFit()$coef[[2]], 3),
       ", P-value ", "\\( = \\) ", signif(summary(lmFit())$coef[2, 4], 3)
     )
   )
 })
 
 output$treeSplits <- renderPlot({
   tree1 <- treeFit()
   base::plot(tree1)
 })
 
 output$rfVarImport <- renderPlot({
   rf1 <- varImp(rfFit())
   base::plot(rf1)
 })
 
 
 # Predict and render Response
 predictedResponse <- eventReactive(input$predict, {
   newData <- data.frame("Plant"=input$plantValue, "Type"=input$typeValue, "Treatment"=input$treatmentValue, "conc"=input$concValue)
   predict(rfFit(), newData)
 })
 
 output$predResponse <- renderText(
   predictedResponse()
 )
 
 
 ## Numerical Summaries
 numSumm <- eventReactive(input$numType ,{
   # Center
   df <- data.frame(index = 1)
   if (sum(grepl( "mean", input$selectedNumSumm2, fixed = TRUE)) == 1) {
     df$mean <- mean(CO2[ , input$summVars])
   }
   if (sum(grepl( "median", input$selectedNumSumm2, fixed = TRUE)) == 1) {
     df$median <- median(CO2[, input$summVars])
   }
   if (sum(grepl( "IQR", input$selectedNumSumm2, fixed = TRUE)) == 1) {
     df$IQR <- IQR(CO2[, input$summVars])
   }
   if (sum(grepl( "sd", input$selectedNumSumm2, fixed = TRUE)) == 1) {
     df$sd <- sd(CO2[, input$summVars])
   }
   if (sum(grepl( "variance", input$selectedNumSumm2, fixed = TRUE)) == 1) {
     df$variance <- var(CO2[, input$summVars])
   }
   if (sum(grepl( "correlation", input$selectedNumSumm2, fixed = TRUE)) == 1) {
     df$correlation <- cor(CO2$uptake, CO2$conc)
   }
   if (sum(grepl( "covariance", input$selectedNumSumm2, fixed = TRUE)) == 1) {
     df$covariance <- cov(CO2$uptake, CO2$conc)
   }
   if (sum(grepl( "contingency", input$selectedNumSumm1, fixed = TRUE)) == 1) {
     updateCheckboxGroupInput(session, "selectedNumSumm1", label = NULL, choices = NULL,
                              selected = character(0), inline = FALSE)
     df <- table(CO2[input$summVars])
   } else if(!is.null(input$selectedNumSumm2)){
     updateCheckboxGroupInput(session, "selectedNumSumm2", label = NULL, choices = NULL,
                              selected = character(0), inline = FALSE)
     df <- df[-1]
   }
 })
 
 output$numSumm <- renderTable({
   numSumm()
 })
 
 # Graphical Summaries
 plot <- eventReactive(input$plot, {
   g <- NULL
   if(!is.null(input$plotType2) && input$plotType2 == "Histogram"){
     # Histogram
     g <- ggplot(CO2, aes_string(x=input$summVars)) +
       geom_histogram(fill= "darkseagreen") +
       labs(title = paste0(input$summVars, " Histogram"), y = input$summVars) +
       coord_flip()
   }
   
   if(!is.null(input$plotType1) && input$plotType1 == "BarPlot"){
    # Bar Plot
     g <- ggplot(CO2, aes_string(x=(input$summVars))) +
       geom_bar(fill= "darkseagreen") +
       labs(title = paste0(input$summVars, " BarPlot"), x = input$summVars) +
       coord_flip()
   }
   if(!is.null(input$plotType2) && input$plotType2 == "BoxPlot"){
     # Box Plot
     g <- ggplot(CO2, aes_string(y=input$summVars)) +
       geom_boxplot(color ="green", fill="darkseagreen") +
       labs(title = paste0(input$summVars, "BoxPlot"), x = input$summVars) +
       theme(axis.ticks.x = element_blank(),
             axis.text.x = element_blank())
   }
   
    if(!is.null(input$plotType2) && input$plotType2 == "ScatterPlot") {
     # CO2 uptake vs. conc. scatter plot
     g <- ggplot(CO2, aes_string(x = "conc", y="uptake")) +
       geom_point(color = "darkorange") +
       labs(title = "Concentration v. Uptake", x = "conc", y = "uptake")
    }
   updateRadioButtons(session, "plotType1", selected = character(0))
   updateRadioButtons(session, "plotType2", selected = character(0))
   if(!is.null(g)){
     g
   }
 })
 
 output$graphSumm <- renderPlot({
   plot()
 })
 
 filteredTable <- eventReactive(input$getTable, {
   tab <- ""
   if(!is.null(input$tableVars)){
     tab <- as_tibble(CO2) %>%
              select(input$tableVars)
     
     if(!is.null(input$groupbyVars)){
       tab <- as_tibble(CO2) %>%
               select(input$tableVars) %>%
               group_by(input$groupbyVars)
       if(!is.null(input$filterBy)){
           if(input$filterBy == "Treatment"){
             tab <- as_tibble(CO2) %>%
                     select(input$tableVars) %>%
                     group_by(input$groupbyVars) %>%
                     filter(Treatment == input$filterByVarsTreat)
           }
           if(input$filterBy == "Type"){
             tab <- as_tibble(CO2) %>%
                     select(input$tableVars) %>%
                     group_by(input$groupbyVars) %>%
                     filter(Type == input$filterByVarsType)
           }
           if(input$filterBy == "Plant"){
             tab <- as_tibble(CO2) %>%
                     select(input$tableVars) %>%
                     group_by(input$groupbyVars) %>%
                     filter(Plant == input$filterByVarsPlant)
           }
       }
     } else {
       if(!is.null(input$filterBy)){
         if(input$filterBy == "Treatment"){
           tab <- as_tibble(CO2) %>%
             select(input$tableVars) %>%
             group_by(input$groupbyVars) %>%
             filter(Treatment == input$filterByVarsTreat)
         }
         if(input$filterBy == "Type"){
           tab <- as_tibble(CO2) %>%
             select(input$tableVars) %>%
             group_by(input$groupbyVars) %>%
             filter(Type == input$filterByVarsType)
         }
         if(input$filterBy == "Plant"){
           tab <- as_tibble(CO2) %>%
             select(input$tableVars) %>%
             group_by(input$groupbyVars) %>%
             filter(Plant == input$filterByVarsPlant)
         }
       }
     }
   }
   # Clear selections and return table
   updateRadioButtons(session, "groupbyVars", selected = NULL)
   updateRadioButtons(session, "filterBy", selected = NULL)
   tab
 })
 
 output$tableSummary <- renderTable({
   filteredTable()
 })
 

})
