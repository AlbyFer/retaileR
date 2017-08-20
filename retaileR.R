
# retaileR <- R package for Business Analytics. V 0.1

# ------------------------------------------------------------------------------------------------------------------ #

closure_opp_cost <- function(time_closure, weekdays = TRUE, date_var= "Date", time_var= "Time", sales_var= "Final.price..GBP.",
                             format_time= "%H:%M:%S", X){
  
  # Estimate the shop closure opportunity cost and fit a distribution. The default inputs values work for an 
  # IZettle dataset
  # Inputs: X - a data frame of unitary sales
  #         time_closure - a string vector containing the shop closure time for which to estimate the opportunity cost. Its format
  #                        must match format_time.
  #         weekdays - boolean; if TRUE, indicates to consider weekdays only.
  #         date_var - the column name, as string, of the column in X containing the date of each unitary sale.
  #         time_var - the column name, as string, of the column in X containing the time of each unitary sale.
  #         sales_var - the column name, as string, of the column in X containing the monetary value of each unitary sale.
  #         format_time - the time format as string.
  # Output: a scalar
  

  if(weekdays == TRUE) {
    sales <- subset(X, weekdays(c(X[,date_var])) != "Sunday" | weekdays(c(X[,date_var])) != "Saturday")
  } else {
    sales <- X
  }
  
  late.sales <- subset(sales, sales[,time_var] > strptime(time_closure, format = format_time))
  late.sales.by.day <- aggregate(as.formula(paste(sales_var,"~",date_var)), late.sales, sum)
  avg.late.sales <- sum(late.sales.by.day[,sales_var])/length(unique(sales[,date_var]))
  
  return(avg.late.sales)
  
}

# ------------------------------------------------------------------------------------------------------------------ #
# Segmenting sales by product lines

segment_prod_line <- function(X, products_in_line, sales_var= "Final.price..GBP.", name_var="Name", order_frequency = 31){
  
  # Calculate the partial sales for a given product line (i.e. a set of products). The default inputs values work for an 
  # IZettle dataset
  # Inputs: X - a data frame of unitary sales
  #         products_in_line - a string vector containing the names of the products. They must match how they are called in name_var
  #         sales_var - the column name, as string, of the column in X containing the sale price of unitary sale
  #         name_var - the column name, as string, of the column in X containing the product name of each unitary sale
  #         order_frequency - a scalar representing the frequency of procurement (i.e. of products in line) in the given time frame
  #                           for which X refers to. I.e. if X covers a month of 31 days and deliveries of products_in_line are 
  #                           daily, then order_frequency = 31
  # Output: a scalar
  
  sales_line <- X[X[[name_var]] %in% products_in_line,]
  tot <- sum(sales_line[[sales_var]])
  tot_by_day <- tot/order_frequency
  return(tot_by_day)
}



# ------------------------------------------------------------------------------------------------------------------ #
# Create a new class for sales containing the most used grouping of sales by time (e.g. sales by week, day, ...), and 
# every other riassunto che ti viene in mente

as.sales <- function(X, date_var= "Date", time_var= "Time", sales_var= "Final.price..GBP.",
                     format_time= "%H:%M:%S", discount_var = "Discount..GBP.") {
  
  # Create an object of class 'sales'
  # Inputs: X - a data frame of unitary sales
  #         date_var - the column name, as string, of the column in X containing the date of each unitary sale.
  #         time_var - the column name, as string, of the column in X containing the time of each unitary sale.
  #         sales_var - the column name, as string, of the column in X containing the monetary value of each unitary sale.
  #         format_time - the time format as string.
  #         discount_var - the column name, as string, of the column in X containing the monetary value of each discount.
  # Output: a S4 object of class 'sales' with slots:
  #               - Daily: summary of sales as daily timeframe
  #               - Weekly: summary of sales as weekly timeframe
  #               - Monthly: summary of sales as monthly timeframe
  #               - Discount: summary of all discounts in X
  #               - sales_funct_time: an 'lm' object of function sales ~ time
  
  library(lubridate)
  
  sales <- setClass("sales", slots = c(Daily = "data.frame", Weekly = "data.frame", Monthly = "data.frame",
                                       Discount = "numeric", sales_funct_time = 'lm'))
  
  X[[date_var]] <- as.Date(X[[date_var]])
  X[[time_var]] <- strptime(X[[time_var]],  format= format_time)
  X$Week <- week(X[[date_var]])
  X$Month <- month(X[[date_var]])
  day <- aggregate(as.formula(paste(sales_var,"~",date_var)), X, sum)
  week <- aggregate(as.formula(paste(sales_var, "~", "Week")), X, sum)  # Trovare modo di incollare formula a meta'
  month <- aggregate(as.formula(paste(sales_var, "~", "Month")), X, sum)
  disc <- sum(X[[discount_var]])
  regr <- lm(as.formula(paste(sales_var,"~",date_var)), data = day)
  dat <- sales(Daily = day, Weekly = week, Monthly = month, Discount = disc, sales_funct_time = regr)
  return(dat)
}

# ------------------------------------------------------------------------------------------------------------------ #
# Formatting - format iZettle for date/time values

sales.format <- function(X, date_var= "Date", time_var= "Time", format_time = '%H:%M'){
  
  # Formats time and date for a sales dataset. The default inputs values work for an IZettle dataset
  # Inputs: X - a data frame of unitary sales
  #         date_var - the column name, as string, of the column in X containing the date of each unitary sale.
  #         time_var - the column name, as string, of the column in X containing the time of each unitary sale.
  #         format_time - the time format as string.
  # Output: a dataframe.
  
  dat <- X
  dat[[date_var]] <- as.Date(X[[date_var]])
  dat[[time_var]] <- strptime(X[[time_var]],  format= format_time)
  return(dat)
}



# ------------------------------------------------------------------------------------------------------------------ #
# Build histograms on time of sales

mean.items.times <- function(X, date_var= "Date", time_var= "Time", quantity_var = "Quantity", plot = TRUE, 
                             weekdays = TRUE, ...) {
  
  # Estimate and plot the mean number of items sold for each time unit. Optimized for an iZettle dataset
  # Inputs: X - a data frame of unitary sales
  #         date_var - the column name, as string, of the column in X containing the date of each unitary sale.
  #         time_var - the column name, as string, of the column in X containing the time of each unitary sale.
  #         quantity_var - the column name, as string, of the column in X containing the item quantity for each unitary sale.
  #         plot - a boolean. TRUE plots a barplot of the time/quantity distribution.
  #         weekdays - a boolean. TRUE excludes weekends in the estimation.
  #         ... - arguments passed to methods.
  # Output: a dataframe with the time/quantity distribution data.
  
  if(weekdays == TRUE) {
    dat <- subset(X, weekdays(c(X[,date_var])) != "Sunday" | weekdays(c(X[,date_var])) != "Saturday")
  } else {
    dat <- X
  }
  
  dat[[time_var]] <- trunc(dat[[time_var]], units = "hours")
  dat[[time_var]] <- substr(as.character(dat[[time_var]]), 12, 19)
  daily_sum <- aggregate(as.formula(paste(quantity_var,"~", as.character(time_var), "+", date_var)), dat, sum)
  mean_hour <- aggregate(as.formula(paste(quantity_var,"~", time_var)), daily_sum, mean)
  
  if(plot == TRUE){
    barplot(mean_hour[[quantity_var]], xlab = "Time of Day", col = "blue", ylab = "Avg number of items", 
            names.arg =  substr(mean_hour[[time_var]], 1, 2), ... )
  }
  return(mean_hour)
}
