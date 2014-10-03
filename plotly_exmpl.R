rm(list=ls())
suppressPackageStartupMessages(require(shiny))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(doBy))
suppressPackageStartupMessages(require(plotly))

source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")

## App specific stuff
diamonds_df <- diamonds
features_lst <- names(diamonds_df)
features_lst <- features_lst[features_lst != "price"]

# Compute medians for all features which will be used to display as the default "test" diamond
median_diamonds_df <- mycompute_medians_df(diamonds_df, keep.names=TRUE)

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

gp_carat <- myplot_scatter(diamonds_smp_df, "carat", "price", ylabel="price ($)",
                           colorcol_name="color",
                           stats_df=median_diamonds_df, 
                           predict_df=test_diamonds_df)
print(gp_carat)
gp_carat_plotly <- myplot_scatter(diamonds_smp_df, "carat", "price", ylabel="price ($)",
                           colorcol_name="color", i_pkg="plotly",
                           stats_df=median_diamonds_df, 
                           predict_df=test_diamonds_df)
print(gp_carat_plotly)
print(myplot_plotly(gp_carat_plotly))

gp_cut <- myplot_scatter(diamonds_smp_df, "cut", "price", ylabel="price ($)",
                         colorcol_name="color",
                         stats_df=median_diamonds_df, 
                         predict_df=test_diamonds_df)
print(gp_cut)
gp_cut_plotly <- myplot_scatter(diamonds_smp_df, "cut", "price", ylabel="price ($)",
                         colorcol_name="color", i_pkg="plotly",
                         stats_df=median_diamonds_df, 
                         predict_df=test_diamonds_df)
print(gp_cut_plotly)