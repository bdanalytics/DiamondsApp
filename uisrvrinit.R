suppressPackageStartupMessages(require(shiny))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(doBy))

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
