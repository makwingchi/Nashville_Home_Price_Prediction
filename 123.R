dat <- read.csv('train.and.test_student.csv')

## Internal Characteristics
dat <-
  dat %>%
  mutate(effyearbuilt_building = ifelse(effyearbuilt_building==0, NA, effyearbuilt_building),
         sf_finished_less_ifla = ifelse(sf_finished_less_ifla==0, NA, sf_finished_less_ifla),
         Acrage = ifelse(Acrage == 0, NA, Acrage)) %>%
  mutate(effyearbuilt_building = replace_na(effyearbuilt_building, mean(effyearbuilt_building, na.rm = T)),
         sf_finished_less_ifla = replace_na(sf_finished_less_ifla, mean(sf_finished_less_ifla, na.rm = T)),
         Acrage = replace_na(Acrage, mean(Acrage, na.rm = T))) %>%
  mutate(age = 2018 - effyearbuilt_building) %>%
  mutate(age_factor = ifelse(age < 5, 0, 
                             ifelse(age <= 23, 1, 
                                    ifelse(age <= 38, 2, 3))),
         bedroom_factor = ifelse(bedroomsunits_building < 3, 0, 
                                 ifelse(bedroomsunits_building == 3, 1, 2)),
         baths_factor = ifelse(baths == 0, 0,
                               ifelse(baths == 1, 1, 2)),
         building_factor = ifelse(LandUseFullDescription == 'SINGLE FAMILY', 0, 1),
         log_sf_finis_1 = log(sf_finished_less_ifla),
         log_Acrage = log(Acrage),
         offsite = ifelse(as.character(OwnerAddress1) == as.character(LocationAddress), 0, 1)) %>%
  st_as_sf(coords = c("WGS1984X", "WGS1984Y"), crs = 4326, agr = "constant") %>%
  st_transform(crs = 2915) %>%
  select(kenID, SalePrice, age_factor, bedroom_factor, baths_factor, 
         building_factor, log_sf_finis_1, log_Acrage, offsite, test)

## Demographic
census <- 
  read.csv('census.csv', stringsAsFactors = F) %>%
  mutate(GEOID = as.character(FIPS)) %>%
  select(GEOID, Md_Value, Pctwhite, Density)

block_group <- 
  read_sf('shp/block_group.shp') %>%
  select(GEOID) %>%
  st_transform(crs = 2915)

dat <-
  st_join(dat, block_group, join = st_intersects) %>%
  left_join(., census, by = c('GEOID' = 'GEOID'))

## Public Services
public_art <- 
  read_sf('shp/Public_Art_Collection.shp') %>%
  st_transform(crs = 2915)

art.xy <- 
  public_art %>%
  cbind(.,st_coordinates(st_centroid(public_art)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

retail2 <-
  add_osm_feature(opq = opq(bbox = c(-87.054764,35.967785,-86.515587,36.405495)), 
                  key = 'building', 
                  value = 'retail') %>%
  osmdata_sf()

retail2 <- 
  st_geometry(retail2$osm_points) %>%
  st_transform(crs = 2915) %>%
  st_sf()

retail.xy <- 
  retail2 %>%
  cbind(.,st_coordinates(st_centroid(retail2)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

garden <-
  add_osm_feature(opq = opq(bbox = c(-87.054764,35.967785,-86.515587,36.405495)), 
                  key = 'leisure', 
                  value = 'garden') %>%
  osmdata_sf()

garden <- 
  st_geometry(garden$osm_points) %>%
  st_transform(crs = 2915) %>%
  st_sf()

garden.xy <- 
  garden %>%
  cbind(.,st_coordinates(st_centroid(garden)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

BID <- 
  read_sf('shp/BID_point.shp') %>%
  st_transform(crs = 2915)

BID.xy <- 
  BID %>%
  cbind(.,st_coordinates(st_centroid(BID)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

home.xy <-
  dat %>%
  cbind(.,st_coordinates(st_centroid(dat)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

nn_function <- function(measureFrom,measureTo,k) {
  
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint)
  
  return(output)  
}

distArt_20 <- 
  as.data.frame(nn_function(home.xy, art.xy, 20)) %>%
  mutate(log_Public_Art_Collection_20 = log(pointDistance)) %>%
  select(log_Public_Art_Collection_20)

distGarden_10 <- 
  as.data.frame(nn_function(home.xy, garden.xy, 10)) %>%
  mutate(log_garden_10 = log(pointDistance)) %>%
  select(log_garden_10)

distRetail_10 <- 
  as.data.frame(nn_function(home.xy, retail.xy, 10)) %>%
  mutate(log_retail2_10 = log(pointDistance)) %>%
  select(log_retail2_10)

distBID_1 <- 
  as.data.frame(nn_function(home.xy, BID.xy, 1)) %>%
  mutate(BID = log(pointDistance)) %>%
  select(BID)

distHome_20 <-
  as.data.frame(nn_function(home.xy, home.xy, 21)) %>%
  mutate(log_home_20 = log(pointDistance * 21 / 20)) %>%
  select(log_home_20)

vars <-
  dat %>%
  st_set_geometry(NULL) %>%
  cbind(., distArt_20, distGarden_10, distRetail_10, distBID_1, distHome_20) %>%
  select(-GEOID)

## Other Variables
GIS <- read.csv('GIS_output.csv')

vars <-
  left_join(vars, GIS) %>%
  mutate(log_MdValue = log(Md_Value),
         log_Density = log(Density)) %>%
  select(-Md_Value, -Density)










