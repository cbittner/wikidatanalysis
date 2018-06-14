#mapping places from Wikdiata that are semantically related
#through the "instance of" statement (https://www.wikidata.org/wiki/Property:P31)

#packages:

  #install.packages("randomcoloR")
  library(randomcoloR)
  
  #install.packages("leaflet")
  require(leaflet)


#Functions:

 #place_id_to_query (takes the Wikidata ID of an item (string) and returns a SPARQL query for all items that are related to the input-item through instance-of statements)
    place_id_to_query <- function(wd_id){
      query_part_a <- 'SELECT ?item ?itemLabel
      WHERE {
      wd:'
      query_part_b <- ' wdt:P31 ?item.
      SERVICE wikibase:label {
      bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
      }'
   
      place_query <- paste(query_part_a, wd_id, query_part_b, sep='')
      
      return(place_query)
  }
  
  # importing a query result into a dataframe requires the function "sparql_to_df" from the file "wikidata2R2postGIS.R"
  source("wikidata2R2postGIS.R")
  
  
  #place_to_semantics (takes object id, runs place_to_query and returns table with semantics (instance_of-statements))
    place_to_semantics <- function(wd_id){
      query <- place_id_to_query(wd_id)
      semantics <- sparql_to_df(query, endpoint, geoCol=FALSE)
      return(semantics)
      
    }
    
  #semantics_to_geo_semantics takes table of semantics (output from place_to_semantics) and returns a list with tables of places with these semantic relations
    semantics_to_geo_semantics <- function(semantics){
      # cretaing a list of all query results 
      geo_semantics <- list()
      for (i in 1:length(semantics$item)){
        #remove all before the id 
        sem_id <- gsub(".*entity/","",semantics$item[i])
        geo_semantics[[i]] <- sparql_to_df(sem_id_to_query(sem_id), endpoint)
      }
      return(geo_semantics)
    }
    
 #geo_semantics_to_map takes table of semantics and list of tables with geo-objects with these semantics and returns a map of these
    geo_semantics_to_map <- function (semantics, geo_semantics) {
      #list of color for markers
      colors <- distinctColorPalette(length(semantics$item))
      
      #creating the map object
      m <- leaflet() %>%
        # Add OpenStreetMap map tiles
        addProviderTiles(providers$OpenStreetMap)
      
      #add layers from query results
      for (i in 1:length(semantics$item)){
        m <- m %>%
          addCircleMarkers(
            lng=geo_semantics[[i]]$lng,
            lat=geo_semantics[[i]]$lat,
            popup=paste('<a href="', geo_semantics[[i]]$item,
                        '">',
                        geo_semantics[[i]]$itemLabel,
                        '</a>"',
                        sep=''),
            color = colors[i],
            fillOpacity = 0.8,
            opacity = 1,
            radius = 5,
            weight = 3,
            group = semantics$itemLabel[i]
          )
      }
      
      #add controls
      m <- m %>%
          addLayersControl(
          baseGroups = c("OSM (default)"),
          overlayGroups = c(semantics$itemLabel),
          options = layersControlOptions(collapsed = FALSE)
        )
      
      #add Legend
      m <- m %>%
        addLegend(
          position="bottomright",
          colors=colors,
          labels=semantics$itemLabel
        )
      
      
      return(m)
    }
    
 #place_to_geosemantics runs all the above functions one by one and returns a leaflet map of results (takes id of wd_object returs a map with all georeferenced objects of all instance of relations)
    place_to_geosemantics <- function(wd_id){
      semantics <- place_to_semantics(wd_id)
      geo_semantics <- semantics_to_geo_semantics(semantics)
      map <- geo_semantics_to_map(semantics, geo_semantics)
      return(map)
    }
