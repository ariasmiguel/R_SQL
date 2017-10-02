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
  read.csv(x, na.strings = c("\\N","NA","0000-00-00"), 
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
