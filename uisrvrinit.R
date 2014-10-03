suppressPackageStartupMessages(require(shiny))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(doBy))
suppressPackageStartupMessages(require(plotly))

## from mydsutils.R
myformat_number <- function(x) {
    if (class(x) != "num") x <- as.numeric(x)
    return(format(x, big.mark=',')) # 000's separator
}

mycompute_median <- function(vector) {
    if (is.factor(vector))
        return(factor(levels(vector)[median(as.numeric(vector))], levels(vector)))
    else return(median(vector))
}

mycompute_medians_df <- function(df, keep.names=FALSE) {
    medians_df <- summaryBy(. ~ factor(0), data=df, FUN=median, keep.names=keep.names)
    # summaryBy does not compute stats for factor variables
    fctrs_lst <- sapply(names(df), function(col) if (is.factor(df[, col])) return(col))
    fctrs_lst <- fctrs_lst[!sapply(fctrs_lst, is.null)]

    for (fctr in fctrs_lst) {
        if (keep.names) new_name <- fctr else new_name <- paste0(fctr, ".median")
        medians_df[, new_name] <- mycompute_median(diamonds_df[, fctr])
    }

    rownames(medians_df) <- "median"
    return(medians_df)
}

## App specific stuff
diamonds_df <- diamonds
features_lst <- names(diamonds_df)
features_lst <- features_lst[features_lst != "price"]

# Compute medians for all features which will be used to display as the default "test" diamond
median_diamonds_df <- mycompute_medians_df(diamonds_df, keep.names=TRUE)
