/* Creates a view that contains the ALL order_details, some info from the
orders table and the company that ships the goods */
CREATE MATERIALIZED VIEW info AS SELECT *, OrderDate,
    CompanyName AS Shipper, Freight, ShipCountry AS Dest
    FROM (
      SELECT * FROM northwind.order_details
    ) ALL LEFT JOIN (
      SELECT *, CompanyName
        FROM (
          SELECT * FROM orders
          ) ALL LEFT JOIN (
            SELECT ShipperID AS ShipVia, 
            CompanyName FROM shippers) 
        USING ShipVia) 
    USING OrderID

SELECT *, CustomerID, OrderDate,CompanyName AS Shipper, Freight, ShipCountry AS Dest FROM (SELECT * FROM northwind.order_details) ALL LEFT JOIN (SELECT *, CompanyName FROM (SELECT * FROM orders) ALL LEFT JOIN (SELECT ShipperID AS ShipVia, CompanyName FROM shippers) USING ShipVia) USING OrderID
