#print("ui code being executed...")
source("uisrvrinit.R")

shinyUI(pageWithSidebar(

    headerPanel(title=HTML("DiamondsApp:Plotly"),
                windowTitle="Diamonds Price Predictor"),

    sidebarPanel(
        h4("Enter Test Diamond Features:"),
        numericInput('predict.carat', 'carat', median_diamonds_df$carat,
                     min=min(diamonds_df$carat), max=max(diamonds_df$carat),
                     step=0.1),

        selectInput('predict.cut', 'cut', levels(diamonds_df$cut),
                    selected=median_diamonds_df$cut, multiple=FALSE),

        h4('Enter Plot Parameters:'),
        sliderInput('plot.sampleSize', 'Sample Size',
                    min=1, max=nrow(diamonds_df), step=500,
                    value=min(1000, nrow(diamonds_df)), round=0
                   ),

        selectInput('plot.x', 'X-axis', features_lst),
        #selectInput('plot.y', 'Y', names(diamonds_df), names(diamonds_df)[[2]]),
        selectInput('plot.color', 'Color', c('None', features_lst)),

        checkboxInput('plot.jitter', 'Jitter'),
        checkboxInput('plot.smooth', 'Smooth'),

        selectInput('plot.facet_row', 'Facet Row', c(None='.', features_lst)),
        selectInput('plot.facet_col', 'Facet Column', c(None='.', features_lst)),

        h5('Notes:'),
        helpText("1. While the plot shows only the ",
                 "specified number of observations,",
                 "the prediction is based on the ",
                 "full dataset."),
        helpText("2. The test diamond is overlaid on ",
                 "the training data with a red marker ",
                 "and the confidence interval of the ",
                 "predicted price is displayed as an ",
                 "error bar."),
        helpText("3. The median of the diamond feature ",
                 "selected for the x-axis on the plot ",
                 "is highlighted to display the defaults ",
                 "selected for non specified features ",
                 "of the test diamond."),

        h5(" ")#, actionButton("goButton", "Go!")
    ),

    mainPanel(
        textOutput('debug_str'),

        tabsetPanel(
            tabPanel("Prediction",
                h4('Result:'),
                textOutput('price_predict_str'),

                h4('Data Plot:'),

                tabsetPanel(
                    tabPanel("ggplot",
                             plotOutput('plot'),

                             h5(' ')    # Placeholder so that all other commands have a comma
                    ),

                    tabPanel("plotly",
                             uiOutput('iplot'),

                             h5(' ')    # Placeholder so that all other commands have a comma
                    ),

                id="plot.tab")
            ),

            tabPanel("Overview",
                uiOutput('overview'),

                h5(' ')    # Placeholder so that all other commands have a comma
            ),

        id="header.tab"),

        h5(' ')    # Placeholder so that all other commands have a comma
    )
))
