#print("server code being executed...")
#rm(list=ls())
#setwd()

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
        
        output$plot <- renderPlot({
            ui_x_name <- input$plot.x
            ui_x_val <- test_diamonds_df_fn()[ ,ui_x_name]
            if (length(grep("factor", class(median_diamonds_df[, ui_x_name]))) == 0)
                ui_x_median <- median_diamonds_df[, ui_x_name]
            else
                ui_x_median <- unclass(median_diamonds_df[, ui_x_name])[1]
            
            p <- ggplot(diamonds_smp_df_fn(), aes_string(x=ui_x_name, y="price"))
            
            if (input$plot.color != 'None')
                p <- p + geom_point() + aes_string(color=input$plot.color)
            else
                p <- p + geom_point(color="grey")
            
            facets <- paste(input$plot.facet_row, '~', input$plot.facet_col)
            if (facets != '. ~ .')
                p <- p + facet_grid(facets)
            
            if (input$plot.jitter)
                p <- p + geom_jitter()
            if (input$plot.smooth)
                p <- p + geom_smooth()
            
            # Format y-axis
            p <- p + ylab("price ($)")
            p <- p + scale_y_continuous(labels=myformat_number)
            
            # Display median (default for unspecified features) of X-axis feature
            aes_str <- paste0("linetype=\"dotted\", xintercept=as.numeric(", ui_x_name, ")")
            aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))            
            p <- p + geom_vline(mapping=aes_mapping, 
                                data=median_diamonds_df, show_guide=TRUE)
            p <- p + scale_linetype_identity(guide="legend", name="Stats", labels="median")
            
            # Plot the prediction point & conf. interval
            aes_str <- paste0("y=price.predict.fit, x=", ui_x_name)
            aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
            p <- p + geom_point(aes_mapping,
                                data=test_diamonds_df_fn(),
                                color="red", pch=7, size=5)
            
            aes_str <- paste0(
                "ymax=price.predict.upr, ymin=price.predict.lwr, x=", ui_x_name)
            aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))            
            p <- p + geom_errorbar(aes_mapping,
                                   data=test_diamonds_df_fn(),    
                                   color="red", width=0.1)
            
            # Plot the regression line
            p <- p + geom_smooth(method="lm")
            
            # linetype legend messes up the fill legend
            #   but this code doesn't seem to make a difference
#             p <- p + guides(fill=guide_legend(override.aes=list(linetype=0)))
            p <- p + guides(color=guide_legend(override.aes=list(linetype=0)))
            
            print(p)
            
        }, height=700)
     
    }
)