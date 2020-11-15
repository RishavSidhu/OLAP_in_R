# Create Sample Data in csv Files
size_df  <- 
  data.frame(key=1:5,
             short_name=c("xs","s","m","l","xl"),
              name=c("extra_small", "small", "medium", "large", "extra_large"),
              price=c(10,15,20,25,30),
              savings=c(2,4,6,8,10))
write.csv(size_df,"Size.csv", row.names = TRUE)

dough_df <- 
  data.frame(key=1:4,
             short_name=c("wwt","wtk","wre","scr"),
             name=c("whole wheat thin", "whole wheat thick","white regular", "stuffed crust"),
             price=c(50,75,50,100),
             savings=c(5,15,10,20))
write.csv(dough_df,"Dough.csv", row.names = TRUE)

cheese_df <- 
  data.frame(key=1:4,
             short_name=c("sw","cd","mz","fe"),
             name=c("Swiss", "cheddar", "Mozzarella","Feta"),
             price=c(100,100,100,100),
             savings=c(10,10,10,10))
write.csv(cheese_df,"cheese.csv", row.names = TRUE)

toppings_df <-
  data.frame(key=1:6,
             short_name=c("tom","pep","oni","pon","xch"),
             name=c("tomatoes", "pepper", "onions", "pepperoni", "mushrooms", "extra_cheese"),
             price=c(100,50,50,150,150,100),
             savings=c(20,5,5,30,30,10))
write.csv(toppings_df,"Toppings.csv", row.names = TRUE)

province_df <- data.frame(key=1:5,
                          short_name=c("On","Ca","Qu","BC","NY"),
                          name=c("Ontario","California", "Quebec","British Columbia","New York"))
write.csv(province_df,"province.csv", row.names = TRUE)

countries_df <- data.frame(key=1:2,
                           short_name=c("CAD","USA"),
                          name=c("CANADA","AMERICA"))
write.csv(countries_df,"Countries.csv", row.names = TRUE)

provinces <- sample(province_df$short_name, 6, replace=T)
countries <- sample(countries_df$short_name, 6, replace=T)

location_df <- 
  data.frame(key=1:6,
             address = c("Bank Street","Queen Street", "Rideau Street", "Nelson Street", "Bayshore avenue", "Nigra Road"),
             city = c("Toronto","San Fransico","Ottawa","Montreal","Surrey","Hamilton"),
             province=provinces,
             country=countries)
write.csv(location_df,"Locations.csv", row.names = TRUE)

month_df <- 
  data.frame(key=1:12,
             desc=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
             quarter=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))
write.csv(month_df,"Months.csv", row.names = TRUE)

size_df  <- read.csv(file = "Size.csv")
dough_df <- read.csv(file = "Dough.csv")
cheese_df <- read.csv(file = "cheese.csv")
toppings_df <- read.csv(file = "Toppings.csv")
province_df <- read.csv(file = "province.csv")
countries_df <- read.csv(file = "Countries.csv")
location_df <- read.csv(file = "Locations.csv")
month_df <- read.csv(file = "Months.csv")

size_df
dough_df
cheese_df
toppings_df
province_df
countries_df
location_df
month_df

gen_revenue <- function(no_of_recs) {
  
  dates <- sample(month_df$key,no_of_recs, replace = T)
  locs <- sample(location_df$key, no_of_recs, replace=T)
  
  sizes <- sample(size_df$key, no_of_recs, replace=T)
  doughs <- sample(dough_df$key, no_of_recs, replace=T)
  cheeses <- sample(cheese_df$key, no_of_recs, replace=T)
  toppings <- sample(toppings_df$key, no_of_recs, replace=T)
  
  orders <- sample(sample(1:50, 1), no_of_recs, replace=T)
  
  price <- (orders * (size_df[sizes,]$price * dough_df[doughs,]$price + cheese_df[cheeses,]$price * toppings_df[toppings,]$price))/100
  profit <- (orders * (size_df[sizes,]$savings * dough_df[doughs,]$savings + cheese_df[cheeses,]$savings * toppings_df[toppings,]$savings))/100
  
  sizes <- size_df[sizes,]$short_name
  doughs <- dough_df[doughs,]$short_name
  cheeses <- cheese_df[cheeses,]$short_name
  toppings <- toppings_df[toppings,]$short_name
  
  sales <- data.frame(locs=locs,
                      dates = dates,
                      sizes = sizes,
                      doughs = doughs,
                      cheeses = cheeses,
                      toppings = toppings,
                      price = price,
                      profit = profit,
                      orders = orders)
  
  # Sort the records by time order
  sales <- sales[order(sales$date),]
  row.names(sales) <- NULL
  return(sales)
}

# Now create the sales fact table
revenue <- gen_revenue(500)

glimpse(revenue)
# Look at a few records
head(revenue)
tail(revenue)

revenue_cube <- 
  tapply(revenue$profit, 
         revenue[,c("sizes")], 
         function(x){return(sum(x))})
revenue_cube

revenue_cube <- 
  tapply(revenue$profit, 
         revenue[,c("sizes", "doughs")], 
         function(x){return(sum(x))})
revenue_cube

revenue_cube <- 
  tapply(revenue$profit, 
         revenue[,c("sizes", "doughs", "dates")], 
         function(x){return(sum(x))})
revenue_cube

dimnames(revenue_cube)
