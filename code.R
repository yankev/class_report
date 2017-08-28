library(RPostgreSQL)
library(dplyr)
library(ggplot2)

drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
con <- dbConnect(drv, dbname = "students2",
                 host = "localhost", port = 5432,
                 user = "postgres")

dbListTables(con)
dbExistsTable(con, "users")

# get all useful tables from the db into R dataframes
users <- dbGetQuery(con, "SELECT * from users")
votes <- dbGetQuery(con, "SELECT * from votes")
subscriptions <- dbGetQuery(con, "SELECT * from subscriptions")
schools <- dbGetQuery(con, "select schools.*, short_name as country from schools left join countries ON country_id = countries.id;")
departments <- dbGetQuery(con, "SELECT * from departments")
courses <- dbGetQuery(con, "SELECT * from courses")
documents <- dbGetQuery(con, "SELECT * from documents")
enrollments <- dbGetQuery(con, "SELECT * from enrollments")
logins <- dbGetQuery(con, "SELECT * from logins")

