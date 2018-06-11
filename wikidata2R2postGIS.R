#required packages 
{
  #install.packages("SPARQL")
  require(SPARQL)
  
  #postgres connection parameters
  {
    pg_dbname="[DBNAME]"
    pg_host="[HOST]"
    pg_port=[PORT]
    pg_user="[USERNAME]"
    pg_password="[PASSWORD]"
  }
  
  #install.packages("RPostgreSQL")
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,
                   dbname=pg_dbname,
                   host=pg_host,
                   port=pg_port,
                   user=pg_user,
                   password=pg_password)
  
  #install.packages("sqldf")
  require(sqldf)
  options(sqldf.RPostgreSQL.user = pg_user,
          sqldf.RPostgreSQL.password = pg_password,
          sqldf.RPostgreSQL.dbname = pg_dbname,
          sqldf.RPostgreSQL.host = pg_host,
          sqldf.RPostgreSQL.port = pg_port)
  
  #install.packages("randomcoloR")
  library(randomcoloR)
  
  #install.packages("leaflet")
  require(leaflet)
  
}

    #creating a dataframe from a Wikidata SPARQL query with a large result set (returns dataframe)
      #the built-in parser of the SPARQL-Package cannot handle very large query-results
      #therefore, this function saves the query into a local file in the R working directory
      #and runs the SPARQL query via a curl command (curl must be installed on the commupter)
   
   sparql_to_df <- function (query, endpoint, geoCol=TRUE){
      #query: the SPARQL query (string)
      #endpoint: the url of hte SPARQL endpoint (string)
      #geocol: if true, the result contains geographic coordinates (https://www.wikidata.org/wiki/Property:P625)

    #write the query to a file
    write(query, file = "query.sparql")
    
    # declare a name for the csv file of the query results
    csv_name <- 'outputfile'
    queryResultFileName <- paste(csv_name,'.csv', sep='')
    
    #put together a curl command SPARQL2CSV
    curlString <- 'curl -H \"Accept:text/csv\" --data-urlencode query@query.sparql '
    curlCommand <- paste(curlString, endpoint, ' -o ', queryResultFileName,' -k', sep="")
    
    # execute the curl command (curl must be installed on your PC)  
    system("cmd.exe", input=curlCommand)
        
    #import the csv into an R dataframe
    df = read.csv(queryResultFileName, header = TRUE, stringsAsFactors = FALSE)
    
    if (geoCol==TRUE){
      #write long and lat values from the coordinate string into separate columns
      df$lng <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", df$coord))
      df$lat <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", df$coord))
    }
    
    return(df)
  }

#transfering the dataframe to a postgis table
    df_to_postgis <- function(df, query, endpoint, pgtable, geoCol=TRUE){
        #df: the dataframe you want to bring to postgis
        #query: the SPARQL query (string)
        #endpoint: the url of hte SPARQL endpoint (string)
        #pgtable: a name fo the Postgis table
        #geocol: if true, the result contains geographic coordinates (https://www.wikidata.org/wiki/Property:P625)
    
      #creating the table
        dbWriteTable(con, pgtable, df)
      
      #adding the query details as a comment to the table
        time_now <- Sys.time()
        date_now <- Sys.Date()

        pg_comment <- paste("COMMENT ON TABLE ", pgtable, " IS '",
                            "sparql query: \n", query, 
                            "\n \n endpoint: ", endpoint, 
                            "\n date: ", date_now, " ", time_now,"';", sep="")
        sqldf(pg_comment)
      
      #if input contains coordinates, create point geometries in PG (SRID 4326)
        if (geoCol==TRUE){

          pg_alter <- paste("alter table ",pgtable, " add column geom geometry;",  sep="")
          pg_makepoint <- paste("update ",pgtable, " set geom = st_MakePoint(lng, lat);",  sep="")
          pg_setsrid <- paste("update ",pgtable, " SET geom = ST_SetSRID(geom, 4326);",  sep="")
          sqldf(pg_alter)
          sqldf(pg_makepoint)
          sqldf(pg_setsrid)
          sqldf("select populate_geometry_columns();")
        }
    }
  
