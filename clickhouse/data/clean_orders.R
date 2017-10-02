
data <- read.csv("~/Downloads/northwind-mongo-master/orders.csv", 
                 skip = 1, head = FALSE, stringsAsFactors = FALSE)
head(data)
data <- rownames_to_column(data)

cols <- c("OrderID", "CustomerID", "EmployeeID", "OrderDate",
          "RequiredDate", "ShippedDate", "ShipVia", "Freight",
          "ShipName", "ShipAddress", "ShipCity", "ShipRegion",
          "ShipPostalCode", "ShipCountry", "Error")
colnames(data) <- cols
data
head(data)
data %>%
  select(contains("Date")) %>%
  gsub("\\.000", "")
data$RequiredDate <- gsub(" 00:00:00\\.000", "", data$RequiredDate)
data$OrderDate <- gsub(" 00:00:00\\.000", "", data$OrderDate)
data$ShippedDate <- gsub(" 00:00:00\\.000", "", data$ShippedDate)
head(data)

d <- data %>%
  filter(Error != "")

d <- d %>%
  unite(ShipAddress, c("ShipAddress","ShipCity"), sep = "") %>%
  rename(ShipCity = ShipRegion,
         ShipRegion = ShipPostalCode,
         ShipPostalCode = ShipCountry,
         ShipCountry = Error)
d
data[data$Error != "", ] <- d
data <- data[, -15]
data[data == "NULL"] <- NA
colnames(data)[colSums(is.na(data)) > 0]
data$ShippedDate[is.na(data$ShippedDate)] <- "0000-00-00"

write.csv(data, "~/R_SQL/clickhouse/data/orders.csv",
          row.names = FALSE)




CREATE TABLE northwind.orders (OrderID Int32,CustomerID FixedString(5),EmployeeID Int32,OrderDate Date,RequiredDate Date,ShippedDate Nullable(Date),ShipVia Int32,Freight Float32,ShipName String,ShipAddress String,ShipCity String,ShipRegion Nullable(String),ShipPostalCode Nullable(String),ShipCountry String) ENGINE = Memory;