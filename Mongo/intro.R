# Library -----------------------------------------------------------------
# https://jeroen.github.io/mongolite/connecting-to-mongodb.html
library(mongolite)

# Chapter 2 Connecting to MongoDB ---------------------------------------
# The mongo() function initiates a connection object to a MongoDB server
m <- mongo("mtcars", 
           url = "mongodb://readwrite:test@ds043942.mongolab.com:43942/jeroen_test")

# To get an overview of the available methods:
print(m)

?mongo

USER = "drivers-team"
PASS = "mongor0x$xgen"
HOST = "ldaptest.10gen.cc"

# Using plain-text
URI = sprintf("mongodb://%s:%s@%s/ldap?authMechanism=PLAIN", USER, PASS, HOST)
m <- mongo(url = URI)
m$find()
