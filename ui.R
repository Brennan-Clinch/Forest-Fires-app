
#load in packages
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(plotly)
library(rattle)
library(caret)
library(readr)
library(tidyverse)

#Read in our data and save it
forestfdata <- read.csv("./Data/forestfires.csv")
image <- paste0("forestfire.jpg")

Datalink <- "https://archive.ics.uci.edu/ml/datasets/Forest+Fires"


# Define UI 
shinyUI(navbarPage(

    # Add title
    title= "Forest fire data and area prediction" ,
    
    # Add theme
    theme = shinytheme("cosmo"),
    
    # Create tabs for the different sections
    
    tabsetPanel(
        
        #Create the About Section
        
        tabPanel(
            title = "About",
            
            #Create panel for About section
            mainPanel(
                
                img(
                    src = image, 
                    height = '748px', 
                    width = '663px'
                ),
                
                #add panel telling the purpose of the app
                h3("The purpose of this App"),
                "This app explores the relationship between ",
                "factors of forest fires in northeast Portugal ",
                "and the burned area of forest fires,",
                
                
                #Add a section that discusses the data
                h3("The Data"),
                "This data is from a study in 2007 done by Paulo Cortez ",
                "and Anibal Morais where they used machine learning to ",
                "predict the burned area from forest fires in northeastern ",
                "Portugal. I retrieved it ",
                "from the UCL machine learning repository at ",
                a(href= Datalink, "here"),
                ". From there you can find more info on the variables.",
                
                #Add line break
                br(),
                
                #Add section discussing the tabs
                h3("The Tabs"),
                tags$ul(
                    tags$li(
                        "Data: Shows the raw data and allows a lot of customization that is user friendly."
                         ),
                    tags$li(
                        "Data Exploration: Visualizes and summarizes our ",
                        "data"
                         ),
                    tags$li(
                        "Modeling: Gives useful information on 3 different models, ",
                        "fits 3 different models based on what we input, and ",
                        "let's the user predict the  model for the area burned ",
                        "based on their inputs"
                    )
                    ),
                #Add line breaks 
                br(),
                br(),
                br()
                    
                
        )
        ),
        
        #Create the Data page
        tabPanel(
            # Add title
            title = "Data",
            # Create side panel
              sidebarPanel(
                  selectInput(
                      inputId = "selectedmonth",
                      label = "Filter by month(s)",
                      choices = unique(forestfdata$month),
                      selected = unique(forestfdata$month),
                      multiple = TRUE,
                      selectize = TRUE
                  ),
                  selectInput(
                      inputId = "selectedday",
                      label = "Filter by day(s)",
                      choices = unique(forestfdata$day),
                      selected = unique(forestfdata$day),
                      multiple = TRUE,
                      selectize = TRUE
                  ),
                  selectInput(
                      inputId = "selectedcolumns",
                      label = "Filter columns you want",
                      choices = colnames(forestfdata),
                      selected = colnames(forestfdata),
                      multiple = TRUE,
                      selectize = TRUE
                  ),
                  sidebarPanel(downloadButton("downloadeddata", "Download"))
                      
                  ),
            mainPanel(
                dataTableOutput(outputId = "tab")
            )
              ),
        # Create data exploration page
        tabPanel(
            #Title
            title = "Data Exploration",
            #Create sidebar for user inputs
            sidebarPanel(
                #set the width
                width = 5,
                #Add header
                h3("Interactive parameters"),
                #Filter for months of interest
                selectInput(
                    inputId = "selectedmonthDE",
                    label = "Filter by month(s)",
                    choices = unique(forestfdata$month),
                    selected = unique(forestfdata$month),
                    multiple = TRUE,
                    selectize = TRUE
                ),
                #Filter for days of interest
                selectInput(
                    inputId = "selecteddayDE",
                    label = "Filter by day(s)",
                    choices = unique(forestfdata$day),
                    selected = unique(forestfdata$day),
                    multiple = TRUE,
                    selectize = TRUE
                ),
                # Add a header for this sidebar portion for visualization.
                h3("Visualization Parameters"),
                # Create buttons to choose the plot type.
                radioButtons(
                    inputId = "plotType",
                    label = "Plot Type",
                    choiceValues = c("histogram", "scatterPlot"),
                    choiceNames = c("Histogram", "Scatter Plot"),
                    selected = "histogram",
                    inline = TRUE
                ),
                conditionalPanel(
                    condition = "input.plotType == 'histogram'",
                     #Change the variable of interest Note we are not including X and Y.
                    selectInput(
                        inputId = "histVar",
                        label = "Variable",
                        choices = colnames(forestfdata)[5:13]
                    ),
                # Change the number of bins in our histogram.
                numericInput(
                    inputId = "bins",
                    label = "Number of Bins",
                    value = 10,
                    min = 15,
                    max = 100,
                    step = 1
                ),
            ),
            # Only show this panel if the plot type is a scatter plot.
            conditionalPanel(
                condition = "input.plotType == 'scatterPlot'",
                # Select the X variable.
                selectInput(
                    inputId = "varX",
                    label = "X Variable",
                    choices = colnames(forestfdata)[5:13]
                ),
               
                # Select the Y variable.
                selectInput(
                    inputId = "varY",
                    label = "Y Variable",
                    choices = colnames(forestfdata)[5:13],
                    selected = "area"
                ),
                
                # Let the user add a smoothed regression line if they
                # want.
                radioButtons(
                    inputId = "addRegression",
                    label = "Add Smoothed Regression Line",
                    choiceValues = c(TRUE, FALSE),
                    choiceNames = c("Yes", "No"),
                    selected = FALSE,
                    inline = TRUE
                ),
                
                # Add a header for this sidebar portion for the summaries.
                h3("Summary Table Parameters"),
                # Choose whether what summary to report.
                radioButtons(
                    inputId = "summaryType",
                    label = "Summary Type",
                    choiceValues = c("numeric"),
                    choiceNames = c("Numeric"),
                    selected = "numeric",
                    inline = TRUE
                ),
                # Only show this panel if the summary type is numeric.
                conditionalPanel(
                    condition = "input.summaryType == 'numeric'",
                    # Let the user choose what to summarize.
                    selectInput(
                        inputId = "numericVars",
                        label = "Variable(s) to Summarize",
                        choices = colnames(forestfdata)[5:13],
                        selected = colnames(forestfdata)[5:13],
                        multiple = TRUE,
                        selectize = TRUE
                    )
                ),
            ),
            # Create the main panel for the data exploration.
            mainPanel(
                # Use the top portion for visualization.
                h3("Visualization"),
                # Use user input to determine which plot to show.
                conditionalPanel(
                    condition = "input.plotType == 'histogram'",
                    plotlyOutput("histogram")
                ),                     
                conditionalPanel(
                    condition = "input.plotType == 'scatterPlot'",
                    plotlyOutput("scatter")
                ),
                
                # Use the bottom portion for the summary.
                h3("Summarization"),
                # Use user input to determine which summary to show.
                conditionalPanel(
                    condition = "input.summaryType == 'numeric'",
                    dataTableOutput("numericSummaryTable")
                )
            )
        )
        ),
        #Create modeling tab with 3 sub tabs
            navbarMenu(
                #add title
                title = "Modeling",
            
            #Add modeling info tab
            tabPanel(
                #Give it a title
                title = "Modeling Info",
                mainPanel(fluidPage(
                    #Give overview of the type of modeling exercise
                    br(),
                    h4("Goals of the modeling section"),
                    "The goal of the modeling section is to predict burned",
                    "area from forest fires based on different characteristics",
                    "such as rain, wind, etc. We will use 3 different types of models",
                    ": Multiple Linear Regression, regression tree, and Random Forest.",
                    br(),
                    br(),
                    
                    h4("Multiple Linear Regression"),
                    "Multiple Linear Regression is a regression model technique that",
                    "uses more than one explanatory variable (usually at least several) ",
                    "to predict the outcome of a response variable. It is an extension of",
                    " OLS regression that uses just one explanatory variable.",
                    " The equation is of the general form: ",
                    br(),
                    uiOutput("MulRegEx"),
                    br(),
                    "Note that you can also include higher order polynomial terms as well as",
                    " interaction terms to the model as well which can improve it usually. This is one key benefit of Multiple Regression. The downside of Multiple Regression is that",
                    " we cannot have non-linear response variables and we must also have a lot of assumptions met, such as normality.",
                    #Give an overview of regression trees
                    h4("Regression Trees"),
                    "A regression tree, also known as just tree, is an algorithm ",
                    "which recursuvely splits a feature space to create ",
                    "regions where observations are classified by the most ",
                    "dominant class in a region. Probabilities are the relative ",
                    "frequency of each class in a terminal node.",
                    br(),
                    br(),
                    "The main thing that is different for regression trees vs it's ",
                    "counterpart, classification trees, is that classification trees ",
                    "are built with unordered values with dependent variables whereas ",
                    "regression trees take ordered values with continuous variables.",
                    "Since burn area is a continuous response variable, we will be using",
                    " a regression tree.",
                    
                    br(),
                    br(),
                    br(),
                    br(),
                    #Give an overview of random forests
                    h4("Random Forests"),
                    "Random forests create bootstrap samples of the training ",
                    "data and grow classification or regression trees(our case) ",
                    "on each sample. At each split, the trees are restricted to a ",
                    "subset of the features. For classification, the trees take ",
                    "a majority vote on the class of the new data. For ",
                    "regression, their predictions are averaged.",
                    br(),
                    br(),
                    "Only considering a subset of features at each split ",
                    "prevents a handful of features from dominating the early ",
                    "splits in each tree and makes each tree more independent ",
                    "(hopefully). By aggregating the predictions of the ",
                    "independent trees, we reduce the variance of the ",
                    "predictions. Random forests are typically good out-of-the",
                    "-box predictive models, but unfortunately lose the ",
                    "interpretability that stand-alone trees have.",
                    br(),
                    br(),
                    br()
                ))
            ),
            #Add a tab for fitting models
            tabPanel(
                #Add title for sub-tab
                title = "Model fitting",
                #Allow user to set seed
                sidebarPanel(
                    h3("Train-Test Split"),
                    numericInput(
                        inputId = "randseedgen",
                        label = "Set Random Seed",
                        value = 1,
                        min = 1,
                        max = 1000,
                        step = 1
                    ),
                    #Create user friendly widget to select what proportion to use
                    #For training and testing the data
                      numericInput(
                        inputId = "propTes",
                        label = "Proportion of data to use for test set",
                        value = 0.2,
                        min = 0.1,
                        max = 0.4,
                        step = 0.05
                    ),
                    #Create section for cross validation parameters
                    h3("Cross validation"),
                    #Set up the number of folds
                    div(
                        numericInput(
                            inputId = "numFolds",
                           label = "Number of Folds",
                            value = 3,
                            min = 3,
                            max = 10,
                            step = 1
                        ),
                       style = "display:inline-block"
                    ),
                    
                    #Create a section for the multiple regression parameters
                    h3("Multiple regression parameters"),
                    #Let user select which parameters to use
                    selectInput(
                        inputId = "MultRegVars",
                        label = "Variables to include:",
                        choices = colnames(forestfdata)[5:12],
                        selected = c(
                            
                            "FFMC",
                            "DMC",
                            "DC",
                            "ISI",
                            "temp",
                            "RH",
                            "wind",
                            "rain"
                        ),
                        multiple = TRUE,
                        selectize = TRUE
                    ),
                    #Create section for tree parameters
                    h3("Tree parameters"),
                    selectInput(
                        inputId = "treeVars",
                        label = "Variables to Include:",
                        choices = colnames(forestfdata)[5:12],
                        selected = c(
                            
                            "FFMC",
                            "DMC",
                            "DC",
                            "ISI",
                            "temp",
                            "RH",
                            "wind",
                            "rain"
                        ),
                        multiple = TRUE,
                        selectize = TRUE
                    ),
                    # Add side-by-side inputs to take in the tree parameters.
                    h4(tags$b("Complexity Parameter (for splits in tree, try to keep cp extremely low):")),
                    div(
                        uiOutput("minCpInput"),  
                        style="display:inline-block"
                    ),
                    div(
                        uiOutput("maxCpInput"),  
                        style="display:inline-block"
                    ),
                    div(
                        numericInput(
                            inputId = "numCps",
                            label = "# of Values", 
                            min = 1, 
                            max = 10, 
                            value = 5,
                            step = 1
                        ),  
                        style="display:inline-block"
                    ),
                    # Create a section for the random forest parameters.
                    h3("Random Forest Parameters"),
                    # Let the user select which variables to use.
                    selectInput(
                        inputId = "randForVars",
                        label = "Variables to Include:",
                        choices = colnames(forestfdata)[5:12],
                        selected = c(
                            
                            "FFMC",
                            "DMC",
                            "DC",
                            "ISI",
                            "temp",
                            "RH",
                            "wind",
                            "rain"
                        ),
                        multiple = TRUE,
                        selectize = TRUE
                    ),
                    # Let the user select the number of variables to consider
                    # at each split.
                    selectizeInput(
                        inputId = "randForMtry", 
                        label = "Select up to 5 values for mtry:", 
                        choices = 1:length(colnames(forestfdata)[5:12]),
                        multiple = TRUE,
                        selected = c(2, 6, 10),
                        options = list(maxItems = 5)
                    ),
                    # Add a button for fitting models.
                    actionButton(
                        inputId = "trainStart",
                        label = "Fit Models"
                    )
                ),
                # Create the main panel to hold model performances and 
                # summaries.
                mainPanel(
                    
                    h3("Results from testing the models on the test data. Original model results can be found in rds files in Fitted Models folder"),
                    
                    dataTableOutput("testTableOutput"),
                    # Show the coefficients of the Logistic Regression Model.
                    h3("Summary of Multiple Regression Model"),
                    dataTableOutput("MulRegSummary"),
                    br(),
                    # Show the final tree diagram.
                    h3("Tree Diagram"),
                    plotOutput("treeSummary"),
                    br(),
                    h3("Random Forest Feature Importances"),
                    plotOutput("rfVarImpPlot")
                ),
            ),
            # Create the prediction tab.
            tabPanel(
                # Add a title.
                title = "Prediction",
                # Create a sidebar for the user to play with inputs.
                sidebarPanel(
                    # Add buttons to select which model to use.
                    radioButtons(
                        inputId = "modelType",
                        label = "Choose a Model",
                        inline = TRUE,
                        choiceNames = c(
                            "Multiple Regression", 
                            "Regression Tree", 
                            "Random Forest"
                        ),
                        choiceValues = c("MultReg", "tree", "randFor"),
                        selected = "MultReg"
                    ),
                    # Add a button for fitting models.
                    actionButton(
                        inputId = "predStart",
                        label = "Predict"
                    ),
                    # Depending on which model to use for prediction, change the
                    # variables shown to match the fitted models.
                    conditionalPanel(
                        condition = "input.modelType == 'MultReg'",
                        uiOutput("MulRegPredInputs")
                    ),
                    conditionalPanel(
                        condition = "input.modelType == 'tree'",
                        uiOutput("treePredInputs")
                    ),
                    conditionalPanel(
                        condition = "input.modelType == 'randFor'",
                        uiOutput("randForPredInputs")
                    )
                    
                        
                    
                    
                ),
                # Create the main panel to show predictions.
                mainPanel(
                    h3("Predicted area with Your Inputs"),
                    dataTableOutput("preds")
                )
                    )
                    )
                )
            ))
                    
                    
                
                
            
        
        
        
        
