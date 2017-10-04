# Library -----------------------------------------------------------------
library(tidyverse)
library(highcharter)
library(scales)
library(treemap)
library(data.table)
library(lubridate)
# Northwind Data Cleaning & Visualizations -------------------------------
# Copy something from another system to this system:
#   scp miguel@fundamentals.rnfc.org:categories.csv /R_SQL/clickhouse/data/categories.csv

# Import the data --------------------------------------------------------
import <- function(x) {
  x <- paste("~/R_SQL/clickhouse/data/", x, ".csv", sep = "")
  fread(x, na.strings = c("\\N","NA","0000-00-00"), 
           stringsAsFactors = FALSE)
}
names <- list("categories", "customers", "employees",
            "order_details", "shippers", "orders",
            "products", "regions", "territories","info")
data <- map(names, import)
names(data) <- names
list2env(data, envir=.GlobalEnv)

# View data structure ---------------------------------------------------
setDT(info)
setDT(products)
str(info)

# Replications ---------------------------------------------------------
# https://www.kaggle.com/fabienvs/grupo-bimbo-data-analysis

# Weeks

weeks <- info[, .N, by = .(year(OrderDate), 
                           week = floor(yday(OrderDate)/7))][order(year,week)]
weeks[, time := paste(year, ", ", week, sep = "")]
weeks
highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Products Sold per Week") %>%
  hc_xAxis(categories = weeks[, time]) %>%
  hc_add_series(data = weeks[, N],
                name = "Products sold")

# Agencias / Company
str(info)
info[, .N, by = .(CustomerID,year(OrderDate), 
                  week = floor(yday(OrderDate)/7))][order(year,week)][
                    , time := paste(year, ", ", week, sep = "")]
companies <- copy(info)
companies[, `:=`(sales = UnitPrice * Quantity)]
agencias <- companies[, .(.N, Sales = sum(sales)), 
                      by = .(CustomerID,year(OrderDate), 
                      week = floor(yday(OrderDate)/7))][
                      , time := paste(year, ", ", week, sep = "")
                      ]

highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Sales Per Week By Company") %>%
  hc_xAxis(categories = agencias[, time]) %>%
  hc_add_series(data = agencias[, Sales],
                name = "Total Sales")

agencias[order(-Sales)]


treemap(agencias[1:100, ],
        index=c("CustomerID"), vSize="Sales", vColor="N",
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="Biggest Orders",
        title="Top Sales")

hchart(agencias[1:100], "treemap", hcaes(x = CustomerID, value = Sales,
                                  color = N))

# Routes and Companies
str(info)
routes <- copy(info)
routes[, `:=`(sales = sum(UnitPrice * Quantity),
              count = .N),
       by = .(CustomerID, Dest)]
routes[, .(sales = sum(UnitPrice * Quantity), count = .N), 
       by = .(CustomerID, Dest)][order(CustomerID)]

popdest <- routes[, .(Sales = sum(UnitPrice*Quantity), Count = .N),
       by = .(Dest)]

hchart(popdest, "scatter", hcaes(x = Sales, y = Count,
                                 group = Dest))

# Canals 
str(info)
ship <- copy(info)
shippy <- ship[, .N, by = .(Shipper, year(OrderDate))][order(year)]

hchart(shippy, "treemap", hcaes(x = Shipper, value = N,
       color = year))

# Routes and Shippers

ships <- copy(info)
rships <- ships[, .(Count = .N, Sales = sum(UnitPrice * Quantity)),
      by = .(Shipper, Dest)]

hchart(rships, "column", hcaes(x = Dest, y = Sales, group = Shipper))


# Products
prods <- copy(info)
pgraph <- prods[, .(Units = sum(Quantity),
             Price = sum(UnitPrice)),
         by = ProductID][, `:=`(avgPrice = Price/Units)]
setkey(pgraph, ProductID)
setkey(products, ProductID)
new_graph <- products[pgraph, .(ProductName, Units, avgPrice)]

highchart() %>%
  hc_title(text = "Most Sold Products") %>%
  hc_xAxis(categories = new_graph[, ProductName]) %>%
  hc_add_series(data = density(new_graph[, avgPrice]),
                type = "area", name = "Avg Price")

highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Sales Per Week By Company") %>%
  hc_xAxis(categories = agencias[, time]) %>%
  hc_add_series(data = agencias[, Sales],
                name = "Total Sales")

names(new_graph)
data(diamonds, package = "ggplot2")
names(diamonds)

new_graph[1:5,]
diamonds

prods[, .N, by = .(ProductID, Dest)]
