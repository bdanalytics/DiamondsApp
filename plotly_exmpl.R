suppressPackageStartupMessages(require(shiny))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(doBy))
suppressPackageStartupMessages(require(plotly))

## from mydsutils.R
myformat_number <- function(x) {
    if (class(x) != "num") x <- as.numeric(x)
    return(format(x, big.mark=',')) # 000's separator
}

mymedian <- function(vector) {
    if (is.factor(vector)) 
        return(factor(levels(vector)[median(as.numeric(vector))], levels(vector)))
    else return(median(vector))
}

## App specific stuff
diamonds_df <- diamonds
features_lst <- names(diamonds_df)
features_lst <- features_lst[features_lst != "price"]

# Compute medians for all features which will be used to display as the default "test" diamond
median_diamonds_df <- summaryBy(. ~ factor(0), data=diamonds_df, FUN=median, 
                                keep.names=TRUE)
# summaryBy does not compute stats for factor variables
median_diamonds_df$cut <- mymedian(diamonds_df[, "cut"])
median_diamonds_df$color <- mymedian(diamonds_df[, "color"])
median_diamonds_df$clarity <- mymedian(diamonds_df[, "clarity"])

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

## Gather inputs
input <- list("plot.sampleSize"=5000, "predict.carat"=0.7, "predict.cut"="Premium",
              "plot.x"="carat", "plot.color"="color", "plot.facet_row"=".", 
              "plot.facet_col"=".", "plot.jitter"=0, "plot.smooth"=0)

diamonds_smp_df <- diamonds_df[sample(nrow(diamonds_df), input$plot.sampleSize), ]
test_diamonds_df$carat <- input$predict.carat
test_diamonds_df$cut <- factor(input$predict.cut, levels=levels(median_diamonds_df$cut))
test_diamonds_df <- predict_price(test_diamonds_df) 

## Display plot
py <- plotly()

create_ggplot <- function(input, i_pkg="NULL") {
    ui_x_name <- input$plot.x
    ui_x_val <- test_diamonds_df[ ,ui_x_name]
    if (!is.factor(median_diamonds_df[, ui_x_name]))
        ui_x_median <- median_diamonds_df[, ui_x_name]
    else {
        ui_x_median <- unclass(median_diamonds_df[, ui_x_name])[1]
        if (!missing(i_pkg) & (i_pkg == "plotly"))
            stop("plotly rendering of xvar as factor crashes")
    }
        
    p <- ggplot(diamonds_smp_df, aes_string(x=ui_x_name, y="price"))
    
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
    if (missing(i_pkg) | (i_pkg != "plotly"))
        p <- p + geom_point(aes_mapping,
                            data=test_diamonds_df,
                            color="red", pch=7, size=5)
    
    aes_str <- paste0(
        "ymax=price.predict.upr, ymin=price.predict.lwr, x=", ui_x_name)
    aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
    if (missing(i_pkg) | i_pkg != "plotly")
        p <- p + geom_errorbar(aes_mapping,
                               data=test_diamonds_df,    
                               color="red", width=0.1)
    
    # Plot the regression line
    p <- p + geom_smooth(method="lm")
    
    # linetype legend messes up the fill legend
    p <- p + guides(color=guide_legend(override.aes=list(linetype=0)))

    return(p)
}    

input$plot.x <- "carat"
gp_carat <- create_ggplot(input, i_pkg="plotly")
print(gp_carat)
pyout_carat <- py$ggplotly(gp_carat)
pyout_carat$response$url

input$plot.x <- "carat"
gp_carat <- create_ggplot(input)
print(gp_carat)

input$plot.x <- "cut"
gp_cut <- create_ggplot(input, i_pkg="plotly")
gp_cut <- create_ggplot(input)
print(gp_cut)
pyout_cut <- py$ggplotly(gp_cut)
pyout_cut$response$url

gp_carat_tmp <- ggplot(diamonds_smp_df, aes(x=carat, y=price)) + geom_point(color="grey")
print(gp_carat_tmp)
pyout_carat_tmp <- py$ggplotly(gp_carat_tmp)
pyout_carat_tmp$response$url
