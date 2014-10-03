#print("server code being executed...")
#rm(list=ls())
#setwd()

source("config.R")      # Ensure config.R is in .gitignore
source("uisrvrinit.R")

# before runApp() run the following

# debugging options
#runApp(display.mode='showcase')
#cat() -> displays output to stdout (so R console)
#browser() -> can interupt execution and can be called conditionally
#http://shiny.rstudio.com/articles/debugging.html

# to deploy
# authorize computer with token from https://www.shinyapps.io/tokens
#require(shinyapps)
#deployApp()

# server initilization stuff

# Run regression
prediction_mdl <- lm(reformulate(features_lst, response="price"),
                     data = diamonds_df)
#print(summary(prediction_mdl))

## Create test set & get predictions on the test set
predict_price <- function(df) {
    prediction <- predict(prediction_mdl, df, interval="confidence")
    df$price.predict.fit <- prediction[, "fit"]
    df$price.predict.lwr <- prediction[, "lwr"]
    df$price.predict.upr <- prediction[, "upr"]
    return(df)
}
test_diamonds_df <- predict_price(median_diamonds_df)
#print(test_diamonds_df)


py <- plotly(username=plotly_username, key=plotly_key)

mycheck_validarg <- function(value) {
         if (is.null(value)) 	return(FALSE)
    else if (is.na(value))   	return(FALSE)
    else if (value == "None")	return(FALSE)	# for shiny inputs
    else return(TRUE)
}

myplot_scatter <- function(df, xcol_name, ycol_name,
                           colorcol_name=NULL, jitter=FALSE, smooth=FALSE,
                           facet_rowcol_name=".", facet_colcol_name=".",
                           ylabel=NULL,
                           stats_df=NULL, predict_df=NULL, i_pkg=NULL) {

    #cat("\nentering myplot_scatter:")
    if (!missing(i_pkg) & (i_pkg == "plotly") & is.factor(df[, xcol_name]))
        stop("plotly rendering of xvar as factor crashes")

    p <- ggplot(df, aes_string(x=xcol_name, y=ycol_name))

    if (!missing(colorcol_name) & mycheck_validarg(colorcol_name))
        p <- p + geom_point() + aes_string(color=colorcol_name)
    else
        p <- p + geom_point(color="grey")

    facets <- paste(facet_rowcol_name, '~', facet_colcol_name)
    if (facets != '. ~ .')
        p <- p + facet_grid(facets)

    if (jitter)
        p <- p + geom_jitter()
    if (smooth)
        p <- p + geom_smooth()

    # Format y-axis
    if (!missing(ylabel))
        p <- p + ylab(ylabel)
    if (is.numeric(df[, ycol_name]))
        p <- p + scale_y_continuous(labels=myformat_number)

    if (!missing(stats_df)) {
        # Display stats of x-axis feature
        aes_str <- paste0("linetype=\"dotted\", xintercept=as.numeric(", xcol_name, ")")
        aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
        p <- p + geom_vline(mapping=aes_mapping,
                            data=stats_df, show_guide=TRUE)
        p <- p + scale_linetype_identity(guide="legend", name="Stats", labels=rownames(stats_df))
    }

    if (!missing(stats_df)) {
        # Plot the prediction point & conf. interval
        aes_str <- paste0("y=", ycol_name, ".predict.fit, x=", xcol_name)
        aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
        if (missing(i_pkg) | (i_pkg != "plotly"))
            p <- p + geom_point(aes_mapping,
                                data=predict_df,
                                color="red", pch=7, size=5)

        aes_str <- paste0(
            "ymax=", ycol_name, ".predict.upr, ymin=", ycol_name, ".predict.lwr, x=", xcol_name)
        aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
        if (missing(i_pkg) | i_pkg != "plotly")
            p <- p + geom_errorbar(aes_mapping,
                                   data=predict_df,
                                   color="red", width=0.1)
    }

    # Plot the regression line
    p <- p + geom_smooth(method="lm")

    # linetype legend messes up the fill legend
    p <- p + guides(color=guide_legend(override.aes=list(linetype=0)))

    #cat("\nexiting myplot_scatter:")
    return(p)
}

shinyServer(
    function(input, output) {

        diamonds_smp_df_fn <- reactive({
            diamonds_df[sample(nrow(diamonds_df), input$plot.sampleSize), ]
        })

        test_diamonds_df_fn <- reactive({
            if (!is.na(input$predict.carat))
                test_diamonds_df$carat <- input$predict.carat
            else
                test_diamonds_df$carat <- median_diamonds_df$carat

            if (!is.na(input$predict.cut))
                test_diamonds_df$cut <- input$predict.cut
            else
                test_diamonds_df$cut <- median_diamonds_df$cut

            predict_price(test_diamonds_df)
        })

        create_ggplot_reactive_fn <- reactive({
            #cat("\nin create_ggplot_reactive_fn:")
            myplot_scatter (diamonds_smp_df_fn(), input$plot.x, "price",
                            colorcol_name=input$plot.color,
                            jitter=input$plot.jitter, smooth=input$plot.smooth,
                            facet_rowcol_name=input$plot.facet_row,
                            facet_colcol_name=input$plot.facet_col,
                            ylabel="price ($)",
                            stats_df=median_diamonds_df,
                            predict_df=test_diamonds_df_fn()
                            , i_pkg="plotly"
                            )
        })

#         output$debug_str <- renderText(sprintf("test_diamonds_df:\n%s",
#                                                test_diamonds_df_fn()))

        output$price_predict_str <- renderText(
            paste(sprintf("price: $%s; 95%% conf interval: [$%s, $%s]",
                    format(test_diamonds_df_fn()$price.predict.fit, big.mark=","),
                    format(test_diamonds_df_fn()$price.predict.lwr, big.mark=","),
                    format(test_diamonds_df_fn()$price.predict.upr, big.mark=","))
                 ,sprintf("\n for a diamond with specified features: %s",
# Error: Single-bracket indexing of reactivevalues object is not allowed.
#                           paste(sapply(c("predict.carat", "predict.cut"),
#                                        function(feat) paste0(feat, "=",
#                                                              print(input[feat]))),
#                                 collapse="; "))
                            paste(paste0("carat = ", print(input$predict.carat)),
                                  paste0("cut = ",   print(input$predict.cut)),
                                  sep="; "))
                 ,sprintf("\n and other features default to training data medians")
            )
        )

#         output$plot <- renderPlot({
#             gp <- create_ggplot_reactive_fn()
#             print(gp)
#         }, height=700)

        output$iplot <- renderUI({
            pyout <- py$ggplotly(create_ggplot_reactive_fn(),
                                 kwargs=list(filename="DiamondsApp: iplot",
                                             fileopt="overwrite", # Overwrite plot in Plotly's website
                                             auto_open=FALSE))
            tags$iframe(src=pyout$response$url,
                        frameBorder="0",  # Some aesthetics
                        height=600, width=800)
        })
    }
)