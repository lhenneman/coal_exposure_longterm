library( data.table)
library( raster)
library( fasterize)
library( sf)
library( fst)
library( USAboundaries)
library( magrittr)

## ============================================================
## Download coal PM2.5 exposure data
## ============================================================
## download all files with the following names to data/inputs/coal_pm25/
# - grids_pm25_total_YYYY.fst (summed impacts from all sources)
# - grids_pm25_byunit_YYYY.fst (impacts from individual sources)


## ============================================================
## list files corresponding total and unit-specific coal pm2.5
## ============================================================
# gridded hyads file location
hyads_file_loc <- './data/inputs/coal_pm25/'
hyads_file_loc <- '~/Dropbox/Harvard/ARP/HyADS/hyads_longterm/exp_pm25_noint/grids_model.lm.cv_single_poly'

#coordinate reference system projection string for spatial data
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

# get the names of the gridded HyADS output files
grid.files.tot.yr <- list.files( hyads_file_loc,
                                 pattern = 'grids_pm25_total_\\d{4}\\.fst',
                                 full.names = TRUE)
grid.files.unit.yr <- list.files( hyads_file_loc,
                                  pattern = 'grids_pm25_byunit_\\d{4}\\.fst',
                                  full.names = TRUE)

## ============================================================
## read in data for total and unit-specific coal pm2.5
## ============================================================
# read files for total impacts
grid.tot.dat <- lapply( grid.files.tot.yr,
                        function( f){
                          print( f)
                          year.f <- gsub( '^.*_|\\.fst', '', f) %>%
                            as( 'integer')
                          
                          in.f <- read.fst( f, as.data.table = T)
                          setnames( in.f, c( 'x', 'y', 'vals.out'))
                          
                          in.f[, year := year.f]
                        }) %>% rbindlist( fill = T)

# create a raster from the total impacts
grid.tot.list <- split( grid.tot.dat, by = 'year', keep.by = F)
grid.tot.dat.r <- 
  lapply( 
    grid.tot.list, 
    rasterFromXYZ
  ) %>%
  brick
crs( grid.tot.dat.r) <- p4s

#  investigate with plot
plot( grid.tot.dat.r)

# create an sf object from raster
grid.unit.dat.sf <- st_as_sf( rasterToPolygons( grid.tot.dat.r))
st_crs( grid.unit.dat.sf) <- p4s

# read coal unit impacts files
grid.unit.dat <- lapply( grid.files.unit.yr,
                         function( f){
                           print( f)
                           year.f <- gsub( '^.*_|\\.fst', '', f) %>%
                             as( 'integer')
                           
                           in.f <- read.fst( f, as.data.table = T)
                           in.f[, year := year.f]
                         }) %>% rbindlist( fill = T)


# melt unit data to long form, limit to pm > 0 to save space (will be replace later)
grid.unit.dat.m <- melt( grid.unit.dat,
                         id.vars = c( 'x', 'y', 'year'),
                         value.name = 'pm',
                         variable.name = 'uID',
                         variable.factor = TRUE,
                         na.rm = T)[pm > 0] #%>%

# remove to clear space
rm( grid.unit.dat)


## =========================================================== ##
# link spatial and population data
## =========================================================== ##
# read in population data (from code/census_data_wrangle.R)
pop_all_yr <- read.fst( 'data/inputs/census_data/population_county.fst', as.data.table = TRUE)

# read in counties data
us_counties.in <- us_counties()
us_counties.pop <- data.table( merge( us_counties.in, pop_all_yr,
                                      by = c( 'countyfp', 'statefp')))

# which counties aren't in the spatial data
county_state_match <- unique( us_counties.pop[, .( countyfp, statefp)])[, ID := 1]
pop_all_yr.miss <- merge( county_state_match, pop_all_yr, all = TRUE,
                          by = c( 'countyfp', 'statefp'))

#just lower 48 states
states.lower <- state.name[ !( state.name %in% c( 'Hawaii', 'Alaska'))]
us_counties.pop <- us_counties.pop[ state_name %in% states.lower]

# transform to consistent crs
us_counties.sf <- st_transform( st_as_sf( us_counties.pop, 
                                          sf_column_name = 'geometry',
                                          crs = crs( us_counties.in)), crs = p4s)

## =========================================================== ##
## # area weight unit population to hyads grid
## creates data table of gridded population data from counties
## =========================================================== ##
grid_popwgt <- 
  lapply( 1999:2020,
          function( yr){
            print( yr)
            interp_col <- c( 'TOT_POP', 'White', 'Black', 'Native',
                             'Asian', 'Pacific', 'Hispanic')
            
            # interpolate county data to grid
            interp <- st_interpolate_aw( us_counties.sf[us_counties.sf$year == yr,
                                                        interp_col], 
                                         grid.unit.dat.sf, extensive = T)
            # fasterize all columns
            interp.r <- 
              lapply( interp_col,
                      function( i){
                        interp.i  <- fasterize( interp[,i], grid.tot.dat.r$X1999,
                                                field = i) %>%
                          rasterToPoints %>%
                          data.table
                        interp.i[, `:=`( year = yr,
                                         pop_name = i)]
                      }         
              ) %>% rbindlist
            
            # melt
            interp.cbind <- dcast( interp.r,
                                   x + y + year ~ pop_name,
                                   value.var = 'layer')
            
            return( interp.r)
          }) %>% 
  rbindlist

## =========================================
# calculate population-weighted exposure for total coal PM2.5
## =========================================
# merge population data with total hyads exposure
grid_popwgt_hy <- merge( grid_popwgt, grid.tot.dat,
                         by = c( 'x', 'y', 'year'))

# calculate race populations and total population
us_population <- grid_popwgt_hy[ pop_name == 'TOT_POP', sum( layer), by = year]
grid_popwgt_hy[, population.race := sum( layer), by = .( pop_name, year)]

# calculate population-weighted exposure by race
# & upper & lower CI
grid_popwgt_hy[, `:=` (pw.hy = vals.out * layer / population.race)]

# sum by race (unit-wise takes a long time)
popwgt_hy_race <- grid_popwgt_hy[, .( pw.hy = sum( pw.hy)),
                                 by = .( pop_name, year)]

write.fst( popwgt_hy_race, './data/outputs/popwgt_hyads_race.fst')


## =========================================
# merge population data with unit coal PM2.5 exposure
## =========================================
# collect the unique unit IDs
unique.locs.unit <- unique( grid.unit.dat.m$uID) %>% as.character

# calculate unit-specific PW exposure for each demographic
# this uses parallel features in R and takes a fair amount of computing power
popwgt_hy_race_unit <- 
  parallel::mclapply( 
    unique.locs.unit,
    function( orig){
      
      # select unit
      grid.unit.o <- grid.unit.dat.m[uID == orig]

      # merge population data and unit-level exposure
      xout <- merge( grid_popwgt, 
                     grid.unit.o,
                     by = c( 'x', 'y', 'year'),
                     all.x = TRUE,
                     allow.cartesian = TRUE)
      
      # calculate race populations and total population
      xout[, population.race := sum( layer), by = .( pop_name, year)]
      
      # calculate population-weighted exposure by race
      # & upper & lower CI
      xout[, `:=` (pw.hy = pm * layer / population.race)]
      
      # sum by race (unit-wise takes a long time)
      xout <- na.omit( xout)
      xout_unit <- xout[, .( pw.hy = sum( pw.hy)),
                        by = .( pop_name, year, uID)]
      
      
      # print( xout)
      print( orig)
      return( xout_unit)
    }) %>% rbindlist

# clear to save on RAM
rm( grid.unit.dat.m)

# save the data
write.fst( popwgt_hy_race_unit, './data/outputs/popwgt_hyads_units_race.fst')


## =========================================
# calculate idwe exposure from all grid locations
## =========================================
# get each grid cell's inv-dist weighted exposure on all other grid cells
grid.tot.dat.sf <- 
  grid.tot.dat.r[[1]] %>%
  rasterToPolygons() %>%
  st_as_sf()
grid.tot.dat.sf$X1999 <- NULL

# this code calculates the inverse distance between all points in the raster
grid.unit.idwe <- 
  parallel::mclapply( 1:nrow( grid.tot.dat.sf),
                      function( sf.row, dat.sf = grid.tot.dat.sf){
                        # print( sf.row)
                        site.sf <- dat.sf[sf.row,]
                        
                        # calculate grid-monitor distances in km
                        site_dist <- as.vector( 
                          st_distance(site.sf, 
                                      dat.sf)) / 1000
                        
                        # if the distance is zero, assign it 36/2
                        site_dist[site_dist < 36] <- 36 / 2
                        
                        # get the coordinates for all grid cells
                        coords <- st_centroid( dat.sf)  %>% st_coordinates %>% as.data.table()
                        coords.site <- st_centroid( site.sf) %>% st_coordinates %>% as.data.table()
                        setnames( coords.site, c( "X", "Y"), c( "X.orig", "Y.orig"))
                        setnames( coords, c( "X", "Y"), c( "X.dest", "Y.dest"))
                        
                        # idwe is the sum of nroads / dist
                        idwe <- data.table( coords.site[, .( X.orig, Y.orig)],
                                                 coords[, .( Y.dest, X.dest)],
                                                 dist1 = site_dist)
                        return( idwe)
                      }) %>% rbindlist

# create list of unique locations
unique.locs <- unique( grid.unit.idwe[, .( X.orig, Y.orig)])

# calculate PWE for each location
# this again takes multiple processors and requires many minutes of run time on 2016 laptop
# this code is written in parallel and takes a while to run
popwgt_hy_race_idwe <- 
  parallel::mclapply( 1:nrow( unique.locs),
                      function( orig){
                        orig.use <- unique.locs[orig]
                        grid.unit.idwe.o <- grid.unit.idwe[X.orig == orig.use$X.orig & 
                                                             Y.orig == orig.use$Y.orig]
                        
                        xout <- merge( grid_popwgt, 
                                       grid.unit.idwe.o,
                                       by.x = c( 'x', 'y'),
                                       by.y = c( 'X.dest', 'Y.dest'),
                                       allow.cartesian = TRUE)
                        
                        # calc expected pop-wgt exposure (exposure = 1/dist)
                        xout[, population.race := sum( layer), by = .( pop_name, year)]
                        xout[, exp := ( 1 / dist1) * layer / population.race]
                        
                        # sum over all destination grid cells
                        xout_pw <- xout[, .( pw.exp = sum( exp)),
                                        by = .( X.orig, Y.orig, year, pop_name)]
                        
                        return( xout_pw)
                      }) %>% rbindlist


# save the idwe data
write.fst( popwgt_hy_race_idwe, './data/outputs/popwgt_hyads_grids_idwe_race.fst')

