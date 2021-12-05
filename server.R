#load in packages
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(rattle)
library(caret)
library(readr)
library(plotly)
library(tidyverse)

#Read in our data and save it
forestfdata <- read.csv("./Data/forestfires.csv")

# Set up the back-end.
shinyServer(function(input, output, session) {
    
    ###Data tab
    output$tab <- renderDataTable({
        #Create datatable output filter
        selectedmonth <- unlist(input$selectedmonth)
        selectedday <- unlist(input$selectedday)
        selectedcolumns <- unlist(input$selectedcolumns)
        
        # Filter the data based on user input.
        forestfdata %>%
            filter(month %in% selectedmonth,
                   day %in% selectedday) %>%
            select(selectedcolumns)
    })
    
    #Let the subsetted data be downloadable
    output$downloadeddata <- downloadHandler(
        filename = function() {
            paste("yourdata.csv")
        },
        content = function(file){
            write.csv(
                forestfdata %>%
                    filter( day %in% input$selectedday,
                            month %in% input$selectedmonth) %>%
                    select(input$selectedcolumns),
                file,
                row.names = F
            
            )
        }
        
    )
    ###Data exploration tab
    
    output$histogram <- renderPlotly({
        #create histogram output
        
        #Extract input variables associated with histogram
        histVar <- input$histVar
        bins <- input$bins
        histLogScale <- input$histLogScale
        
        selecteddayDE <- unlist(input$selecteddayDE)
        selectedmonthDE <- unlist(input$selectedmonthDE)
        
        #Filter the data based on user input
        filterforestfdata <- forestfdata %>% 
            filter(month %in% selectedmonthDE,
                   day %in% selecteddayDE)
            
            # Create the histogram for the variable.
            histogram <- ggplot(filterforestfdata, aes_string(histVar)) + 
                geom_histogram(bins=bins, fill="orange", color="black") + 
                # Change the y-axis label and a title.
                scale_y_continuous("Frequency") + 
                ggtitle(paste0("Frequency of forest fires by ", histVar))
        
        
        # Display the plot.
        ggplotly(histogram)
        
    })
    output$scatter <- renderPlotly({
        
        ###
        # Create a scatterplot output.
        ###
        
        # Extract the input variables associated with the scatter plot.
        varXLogScale <- input$varXLogScale 
        varYLogScale <- input$varYLogScale
        varX <- input$varX 
        varY <- input$varY
        addRegression <- input$addRegression
        
        # Extract the selected day, month, and columns.
        selectedmonthDE <- unlist(input$selectedmonthDE)
        selecteddayDE <- unlist(input$selecteddayDE)
        
        # Filter the data based on user input.
        filterforestfdata <- forestfdata %>%
            filter(month %in% selectedmonthDE,
                   day %in% selecteddayDE)
        
        
            
            scatterPlot <- filterforestfdata %>%
                # Create the scatter plot with semi-opaque purple dots.
                ggplot(aes_string(x=varX, y=varY)) + 
                geom_point(color="purple", alpha=0.33) + 
                # Add axis labels and a title.
                scale_x_continuous(varX) +
                scale_y_continuous(varY) + 
                ggtitle(paste0("Scatter Plot of ", varX, " vs. ", varY))
        
        
        # Add a smoothed regression line if the user wants it.
        if (addRegression) {
            # Make the smoothed regression line dark gray.
            scatterPlot <- scatterPlot +
                geom_smooth(color="grey20")
        }
        
        # Display the plotly plot.
        ggplotly(scatterPlot)
        
    })
    output$numericSummaryTable <- renderDT({
        
        ###
        # Output a numeric summary table of the variables of interest.
        ###
        
        # Extract the selected states, winner, and columns.
        selectedmonthDE <- unlist(input$selectedmonthDE)
        selecteddayDE <- unlist(input$selecteddayDE)
        # Extract the variables to show.
        numericVars <- unlist(input$numericVars)
        
        # Filter for the rows of interest.
        filterforestfdata <- forestfdata %>%
            filter(
                month %in% selectedmonthDE,
                day %in% selecteddayDE
            ) %>%
            select(numericVars)
        
        numericSummary <- do.call(cbind, lapply(filterforestfdata, summary))
        
        as.data.frame(t(numericSummary))
        
    })
    
    #Modeling-info
    ###
    # Modeling - Training
    ###
    output$MulRegEx <- renderUI({
    withMathJax(
        helpText(
            "$$Y= \\beta_0 + \\beta_1x_i + \\beta_2x_i + ... + \\beta_nx_n + E_i$$"
        )
    )
})
    
    output$minCpInput <- renderUI({
        
        ###
        # Create the input box for the min Cp for the tree.
        ###
        
        numericInput(
            inputId = "minCp",
            label = "Min.", 
            min = 0.001, 
            max = 0.05, 
            value = 0.001
        )
    })
    output$maxCpInput <- renderUI({
        
        ###
        # Create the input box for the max number of Cp in the tree.
        ###
        
        # Find the user's min Cp.
        minCp <- input$minCp
        
        # Start at 1.
        value <- 0.06
        
        # If the minCp is greater than 1, move it up.
        numericInput(
            inputId = "maxCp",
            label = "Max.", 
            min = minCp, 
            max = 0.07, 
            value = value)
    })
    observeEvent(input$trainStart, {
        
        ###
        # Test the performance of the three models.
        ###
        
        # Create a Progress object
        progress <- Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error.
        on.exit(progress$close())
        # Set the message to the user while cross-validation is running.
        progress$set(message = "Performing Cross-Validation", value = 0)
        
        # Get the variables to use for each model.
        MultRegVars <- unlist(input$MultRegVars)
        treeVars <- unlist(input$treeVars)
        randForVars <- unlist(input$randForVars)
        
        
        # Get the random seed, proportion of testing, and k-folds params.
        randseedgen <- input$randseedgen
        propTes <- input$propTes
        numFolds <- input$numFolds
        numFolds <- unlist(input$numFolds)
        # Get the Cps to try.
        minCp <- input$minCp
        maxCp <- input$maxCp
        numCps <- input$numCps
        Cps <- seq(minCp, maxCp, length.out=numCps)
        
        # Get the random forest mtrys.
        randForMtry <- as.numeric(input$randForMtry)
        
        # Set the random seed.
        set.seed(randseedgen)
        
        # Get the testing indexes.
        testInd <- sample(
            seq_len(nrow(forestfdata)), 
            size=floor(nrow(forestfdata)*propTes)
        )
        
        # Split into training and testing sets.
        train <- forestfdata[-testInd, ]
        test <- forestfdata[testInd, ]
        
        # Suppress any warning in the fitting process.
        suppressWarnings(library(caret))
        
        # Set the repeated CV params.
        TrControl <- trainControl(
            method="cv",
            number=numFolds
        )
    
        # Increment the progress bar, and update the detail text.
        progress$inc(0.2, detail = "Multiple Regression")
        
        # Evaluate the multiple regression through CV.
        MulRegModel = train(
            area ~ ., 
            data=train[,c("area", MultRegVars)],
            method = "lm",
            metric = "RMSE",
            trControl=TrControl
    )
        
        # Increment the progress bar, and update the detail text.
        progress$inc(0.4, detail = "Regression Tree")
        
        # Let caret choose the best tree through CV.
        treeModel = train(
            area ~ ., 
            data=train[, c(c("area"), treeVars)],
            method="rpart", 
            metric="RMSE",
            preProcess = c("center","scale"),
            tuneGrid=expand.grid(cp = Cps),
            trControl=TrControl
        )
        
        # Increment the progress bar, and update the detail text.
        progress$inc(0.6, detail = "Random Forest")
        
        # Let caret choose the best random forest through CV.
        rfModel = train(
            area ~ ., 
            data=train[, c(c("area"), randForVars)],
            method="rf", 
            metric="RMSE",
            tuneGrid=expand.grid(mtry = randForMtry),
            trControl=TrControl
        )
        
        # Increment the progress bar, and update the detail text.
        progress$inc(0.8, detail = "Evaluating Test Set Performance")
        
        # Get test set predictions.
        MulRegPreds <- predict(MulRegModel, test)
        treePreds <- predict(treeModel, test)
        randForPreds <- predict(rfModel, test)
        
        
        # Create the findMode function.
        findMode <- function(x) {
            # From https://www.tutorialspoint.com/r/r_mean_median_mode.htm
            uniqueX <- unique(x)
            uniqueX[which.max(tabulate(match(x, uniqueX)))]
        }
        
        
        # Find the no-info rate.
        noInfoRate <- mean(findMode(test$area) == test$area)
        # Find the test set performances.
        accVec <- c(
            noInfoRate,
            mean(MulRegPreds == test$area, na.rm=TRUE), 
            mean(treePreds == test$area, na.rm=TRUE), 
            mean(randForPreds == test$area, na.rm=TRUE)
        )
        
        # Convert to a matrix and percentages.
        accMatrix <- t(as.matrix(accVec)) * 100
        # Add informative column names.
        colnames(accMatrix) <- c(
            "No Information Rate",
            "Multiple Regression",
            paste0("Tree (Cp = ", treeModel$bestTune$cp, ")"),
            paste0("Random Forest (mtry = ", rfModel$bestTune$mtry, ")")
        )
    
        # Convert the matrix to a dataframe.
        accTable <- as.data.frame(accMatrix) %>%
            mutate_all(
                round, digits = 3
            ) %>%
            mutate_all(
                paste0, sep="%"
            )
        
        # Create the output for the accuracy table.
        output$accTableOutput <- renderDataTable({
            datatable(accTable)
        })
        
        # Create an output for the logistic regression model rounding to 4 decimals.
        output$MulRegSummary <- renderDataTable({
            round(as.data.frame(summary(MulRegModel)$coef), 4)
        })
        
        # Create a nice tree diagram.
        output$treeSummary <- renderPlot({
            fancyRpartPlot(treeModel$finalModel)
        })
        
        # Create an output for the feature importance plot for random forest model.
        output$rfVarImpPlot <- renderPlot({
            ggplot(varImp(rfModel, type=2)) + 
                geom_col(fill="blue") + 
                ggtitle("Most Important Features by Decrease in Gini Impurity")
        })
        
        # Save the fitted models in a folder.
        saveRDS(MulRegModel, "./Fitted Models/MulRegModel.rds")
        saveRDS(treeModel, "./Fitted Models/treeModel.rds")
        saveRDS(rfModel, "./Fitted Models/rfModel.rds")
        
    })
    
    
    ###
    # Modeling - Prediction
    ###
    
    output$MulRegPredInputs <- renderUI({
        
        ###
        # Create a UI that lets the user input values for the multiple regression 
        # and get a prediction.
        ###
        
        # Get the variables to use for each model.
        MultRegVars <- input$MultRegVars
        
        # Loop through the variables and create numeric input boxes for them. Use
        # the median of the variable for the default value.
        tags$ul(tagList(
            lapply(MultRegVars, function(variable) {
                numericInput(
                    inputId = paste0(variable, "Value"),
                    label = paste0("Input ", variable, " Value"),
                    value = median((forestfdata[, variable]), na.rm=TRUE),
                    step = 0.1
                )
            })
        ))
    })
    
    output$treePredInputs <- renderUI({
        
        ###
        # Create a UI that lets the user input values for the tree model and get a 
        # prediction.
        ###
        
        # Get the variables to use for each model.
        treeVars <- input$treeVars
        
        # Loop through the variables and create numeric input boxes for them. Use
        # the median of the variable for the default value.
        tags$ul(tagList(
            lapply(treeVars, function(variable) {
                numericInput(
                    inputId = paste0(variable, "Value"),
                    label = paste0("Input ", variable, " Value"),
                    value = min((forestfdata[, variable]), na.rm=TRUE),
                    step = 0.1
                )
            })
        ))
    })
    
    output$randForPredInputs <- renderUI({
        
        ###
        # Create a UI that lets the user input values for the random forest model  
        # and get a prediction.
        ###
        
        # Get the variables to use for each model.
        randForVars <- input$randForVars
        
        # Loop through the variables and create numeric input boxes for them. Use
        # the median of the variable for the default value.
        tags$ul(tagList(
            lapply(randForVars, function(variable) {
                numericInput(
                    inputId = paste0(variable, "Value"),
                    label = paste0("Input ", variable, " Value"),
                    value = min((forestfdata[, variable]), na.rm=TRUE),
                    step = 0.1
                )
            })
        ))
    })
    
    observeEvent(input$predStart, {
        
        ###
        # Return predictions when the user wants them.
        ###
        
        # Retrieve the model to use for prediction.
        modelType <- input$modelType
        
        # Load the appropriate model based on user input.
        if (modelType == "MultReg"){
            
            # Get the names of the user inputs for the multiple regression model.
            varsOfInterest <- unlist(lapply(input$MultRegVars, paste0, sep="Value"))
            # Load in the logistic regression model.
            myModel <- readRDS("./Fitted Models/MulRegModel.rds")
            
        } else if (modelType == "tree"){
            
            # Get the names of the user inputs for the tree model.
            varsOfInterest <- unlist(lapply(input$treeVars, paste0, sep="Value"))
            # Load in the tree model.
            myModel <- readRDS("./Fitted Models/treeModel.rds")
            
        } else {
            
            # Get the names of the user inputs for the random forest model.
            varsOfInterest <- unlist(lapply(input$randForVars, paste0, sep="Value"))
            # Load in the random forest model.
            myModel <- readRDS("./Fitted Models/rfModel.rds")
            
        }
        
        # Loop through the user inputs adding them to a vector because you cannot
        # access the variables by simply passing the vector of list elements to 
        # input.
        inputCopy <- c()
        for (variable in varsOfInterest){
            inputCopy <- c(inputCopy, input[[variable]])
        }
        
        # Create a 1-row matrix of the user inputs.
        inputCopy <- t(matrix(inputCopy))
        # Strip "Value" from the variable names to match the column names in 
        # forestfdata.
        colnames(inputCopy) <- str_remove_all(varsOfInterest, pattern="Value")
        
        # Create a data.frame from the user inputs.
        userInputs <- as.data.frame(inputCopy)
        
        # Get predictions based on user inputs
        Prediction <- predict(myModel, userInputs)
        # Combine them into a single matrix and round probabilities to 5 decimals.
        Predictionmatrix <- cbind(round(Prediction, 10))
        # Add informative column names.
        
        
        # Convert the preds matrix to a data.frame.
        preds <- as.data.frame(Predictionmatrix)
        
        # Return the predictions.
        output$preds <- renderDataTable({
            preds
        })
    })
    return(output) 
    })
    
    

    
    
   

    

 
    
