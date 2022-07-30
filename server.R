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
   if(input$filterByTreatment == 1){
     table <- as_tibble(CO2) %>% 
       select(-input$colsToRemove) %>%
       filter(Treatment == input$treatmentFilter)
    } else {
      table <- as_tibble(CO2) %>% 
     select(-input$colsToRemove)
    }
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
   plot(treeFit())
 })
 output$rfVarImport <- renderPlot({
   plot(varImp(rfFit()))
 })
 
 
 # Predict and render Response
 predictedResponse <- eventReactive(input$predict, {
   newData <- data.frame("Plant"=input$plantValue, "Type"=input$typeValue, "Treatment"=input$treatmentValue, "conc"=input$concValue)
   predict(rfFit(), newData)
 })
 
 output$predResponse <- renderText({
   predictedResponse()
 })
 
 
 ## Numerical Summaries
 output$numSumm <- renderText({
   # Center
   mean <-mean(CO2$uptake)
   median(CO2$uptake)
   summary(CO2$uptake)
   # Spread
   sd(CO2$uptake)
   var(CO2$uptake)
   # 2 var.s
   cor(CO2$uptake, CO2$conc)
   cov(CO2$uptake, CO2$conc)
 })
 
 # Graphical Summaries
 output$graphSumm <- renderPlot({
   # Histogram
   ggplot(CO2, aes(y=uptake)) +
     geom_histogram() +
     coord_flip()
   ggplot(CO2, aes(y=uptake)) +
     geom_histogram(aes(color = Plant)) +
     coord_flip()
   
   # Bar Plot
   ggplot(CO2, aes(y=uptake)) +
     geom_bar() +
     coord_flip()
   ggplot(CO2, aes(y=uptake)) +
     geom_bar(aes(color = Plant)) +
     coord_flip()
   
   # Box Plot
   ggplot(CO2, aes(y=uptake)) +
     geom_boxplot(color ="green", fill="darkseagreen") +
     theme(axis.ticks.x = element_blank(),
           axis.text.x = element_blank()) 
            
   # CO2 uptake vs. conc. scatter plot
   ggplot(CO2, aes(x=conc, y=uptake)) +
     geom_point()
 })
 

})
