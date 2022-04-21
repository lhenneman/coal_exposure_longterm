library( USAboundaries)
library( ggplot2)
library( viridis)
library( scales)
library( raster)
library( sf)
library( fasterize)

# define the coordinate reference system for this work
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

## ============================================
## load data
## ============================================
## data from coal_pm25_to_county.R
# read in the race exposure for all facilities
popwgt_hy_race <- 
  read.fst( './data/outputs/popwgt_hyads_race.fst',
            as.data.table = TRUE)

# read in the race exposure by unit dataset
popwgt_hy_race_unit <- 
  read.fst( './data/outputs/popwgt_hyads_units_race.fst',
            as.data.table = TRUE)
popwgt_hy_race_unit[, uID := gsub( '^X', '', uID)]

# read in the race by grid dataset
grid_popwgt_hy_idwe <- 
  read.fst( './data/outputs/popwgt_hyads_grids_idwe_race.fst',
            as.data.table = TRUE)


## facility-unit crosswalk and facility location data
# merge with facility data 
unit_fac_crosswalk <- fread( './data/inputs/coal_facility_data/unit_fac_crosswalk.csv')
facility_locations <- fread( './data/inputs/coal_facility_data/facilities_by_year.csv')
unit_fac_crosswalk[, `:=` ( uID = as.factor( uID))]
popwgt_hy_race_unit.f <- 
  merge( popwgt_hy_race_unit, unit_fac_crosswalk[, .( FacID, State, uID)],
         by = c( 'uID'))


## ============================================
## this section includes data wrangling to set up 
## state and region-specific PWE enhancements
## ============================================
# sum by facility, merge with facility location info
popwgt_hy_race_fac <- 
  popwgt_hy_race_unit.f[, .( pw.hy = sum( pw.hy)), 
                        by = .( pop_name, FacID, State, year)]
popwgt_hy_race_fac <- 
  merge( popwgt_hy_race_fac, 
         facility_locations[, .( FacID, Latitude, Longitude, year, SOx)],
         by = c( 'FacID', 'year'))

# isolate total population exposure by facility
popwgt_hy_race_fac.tot <- 
  popwgt_hy_race_fac[( pop_name %in% 'TOT_POP'),
                                             .( year, FacID, pw.hy)]
setnames( popwgt_hy_race_fac.tot, 'pw.hy', 'tot.pop')

# merge the facility total population PWE with race-specific info
popwgt_hy_race.wgt <- 
  merge( popwgt_hy_race_fac[!( pop_name %in% 'TOT_POP')],
         popwgt_hy_race_fac.tot,
         by = c( 'year', 'FacID'))

# population-weighted fraction for facilities by race
popwgt_hy_race.wgt[, popwgt.frac := pw.hy / tot.pop]

# merge region-state crosswalk with facility PWE
state.desig <- data.table( reg = state.region,
                           State = state.abb)
popwgt_hy_race.wgt <- merge( popwgt_hy_race.wgt,
                             state.desig,
                             by = 'State')

# sum by region & race / state & race
popwgt_hy_race.reg <- 
  popwgt_hy_race.wgt[, .( pw.hy = sum( pw.hy),
                          tot.pop = sum( tot.pop),
                          SOx = sum( SOx)),
                     by = .( year, pop_name, reg)]
popwgt_hy_race.state <- 
  popwgt_hy_race.wgt[, .( pw.hy = sum( pw.hy),
                          tot.pop = sum( tot.pop),
                          SOx = sum( SOx)),
                     by = .( year, pop_name, reg, State)]

# calculate relative PWE
popwgt_hy_race.reg[,popwgt.frac := pw.hy / tot.pop]
popwgt_hy_race.state[,popwgt.frac := pw.hy / tot.pop]

# create unique object with grid number labels
grid_unique <- unique( grid_popwgt_hy_idwe[, .( X.orig, Y.orig)])
grid_unique[, num.grid := 1: nrow( grid_unique)]

# create sf objecs by state and region
grid_unique.r <- 
  grid_unique %>%
  rasterFromXYZ( crs = p4s)
grid_unique.sf <- 
  grid_unique.r %>%
  rasterToPolygons() %>%
  st_as_sf()

# load state polygons
states <- us_states( resolution = 'low') %>%
  st_transform( crs = p4s)

# assign grid cells to states (and regions)
# use fasterize to go from polygons to grids
grid_states <- 
  st_join( grid_unique.sf, states,
           largest = TRUE)
plot( grid_states[,'stusps'])

# extract the num.grid & state variables
grid_states_info <- grid_states[, c( 'num.grid', 'state_abbr')] %>%
  as.data.table
grid_states_info[, geometry := NULL]

# merge with region
state.desig <- data.table( reg = state.region,
                           state_abbr = state.abb)
grid_states_info_reg <- merge( grid_states_info,
                               state.desig,
                               by = 'state_abbr', all.x = T)

# merge back with population data
grid_popwgt_hy_idwe.grid <- 
  merge( grid_popwgt_hy_idwe,
         grid_unique,
         by = c( 'X.orig', 'Y.orig'))

# merge regional idwe PWE object with grid information object
grid_popwgt_hy_idwe.reg <- 
  merge( grid_popwgt_hy_idwe.grid,
         grid_states_info_reg,
         by = 'num.grid')

## ============================================
## calculate expected population weight exposure (from idwe)
## ============================================
# extract the total population exposure
popwgt_hy_race.TOT <- grid_popwgt_hy_idwe.reg[( pop_name %in% 'TOT_POP'),
                                              .( year, num.grid, pw.exp)]
setnames( popwgt_hy_race.TOT, 'pw.exp', 'tot.pop')

# merge total population exposure with all other exposures
popwgt_idwe_race.wgt <- 
  merge( grid_popwgt_hy_idwe.reg[!( pop_name %in% 'TOT_POP')],
         popwgt_hy_race.TOT,
         by = c( 'year', 'num.grid'))

# sum by region & state
popwgt_idwe_race.reg <- 
  popwgt_idwe_race.wgt[, .( pw.exp = sum( pw.exp),
                            tot.pop = sum( tot.pop)),
                       by = .( year, pop_name, reg)]
popwgt_idwe_race.state <- 
  popwgt_idwe_race.wgt[, .( pw.exp = sum( pw.exp),
                            tot.pop = sum( tot.pop)),
                       by = .( year, pop_name, reg, state_abbr)]

# calculate relative exposure enhancement
popwgt_idwe_race.reg[,popwgt.frac := pw.exp / tot.pop]
popwgt_idwe_race.state[,popwgt.frac := pw.exp / tot.pop]


## ============================================
## merge state and region PWE from coal PM2.5 with IDWE results
## ============================================
setnames( popwgt_hy_race.reg,
          c( 'tot.pop', 'popwgt.frac'), c( 'tot.pop.hy', 'popwgt.frac.hy'))
setnames( popwgt_hy_race.state,
          c( 'tot.pop', 'State', 'popwgt.frac'), c( 'tot.pop.hy', 'state_abbr', 'popwgt.frac.hy'))

# do the merging
popwgt_reg <- 
  merge( popwgt_idwe_race.reg, popwgt_hy_race.reg,
         by = c( 'year', 'pop_name', 'reg'))
popwgt_state <- 
  merge( popwgt_idwe_race.state, popwgt_hy_race.state,
         by = c( 'year', 'pop_name', 'reg', 'state_abbr'))

## ============================================
## nationwide pwe (Fig 3)
## ============================================
# plot it!
gg_popwgt <- 
  ggplot( popwgt_hy_race[!( pop_name %in% 'TOT_POP')],
          aes( x = year, y = pw.hy, group = pop_name, color = pop_name,
               fill = pop_name)) +
  geom_point( data = popwgt_hy_race[ pop_name %in% 'TOT_POP'],
              aes( x = year, y = pw.hy, group = pop_name,
                   shape = pop_name),
              inherit.aes = FALSE,
              color = 'grey50', size = 5) +
  # geom_ribbon( alpha = .25, color = NA) +
  geom_line( size = 2) +
  scale_color_brewer( palette = 'Dark2') +
  scale_fill_brewer( palette = 'Dark2') +
  scale_shape_manual( breaks = 'TOT_POP', values = 19, labels = 'Population average') +
  labs( y = expression(paste( 'Population-weighted coal ', PM["2.5"], ', µg ', m^{"-3"}))) +
  theme_bw() +
  theme( axis.title.x = element_blank(),
         axis.title = element_text( size = 16),
         axis.text = element_text( size = 14),
         legend.position = c( .85, .7),
         legend.text = element_text( size = 12),
         legend.title = element_blank(),
         legend.spacing.y = unit( -0.05, "cm"))


ggsave( './figures/hyads_popwgt_trends.png', 
        gg_popwgt,
        width = 11, height = 5.5, scale = .8)

## =================================================
## regional evolution in absolute/relative PWE (fig 5)
## =================================================
gg_pw_race <- 
  ggplot( popwgt_hy_race.reg,
          aes( x = year,
               y = popwgt.frac.hy,
               color = pop_name,
               group = pop_name)) + 
  geom_hline( yintercept = 1) +
  geom_line( size = 2) +
  scale_y_continuous( name = "Relative PWE difference") +
  scale_color_brewer( palette = 'Dark2') +
  facet_grid( . ~ reg) +
  theme_bw() + 
  theme( axis.text = element_text( size = 14),
         axis.text.x = element_text( angle = 30, vjust = .5),
         axis.title = element_text( size = 18),
         axis.title.x = element_blank(),
         legend.position = c( .1, .75),
         legend.title = element_blank(),
         legend.text = element_text( size = 14),
         plot.margin = unit( c( .1, .2, .1, .1) , 'in'),
         strip.background = element_blank(),
         strip.text = element_text( size = 20))

# absolute difference
gg_pw_race_abs <- 
  ggplot( popwgt_hy_race.reg,
          aes( x = year,
               y = pw.hy - tot.pop.hy,
               color = pop_name,
               group = pop_name)) + 
  geom_hline( yintercept = 0) +
  geom_line( size = 2) +
  labs( y = expression( paste( 'Absolute PWE difference', ', µg', m^{"-3"}))) +
  scale_color_brewer( palette = 'Dark2') +
  facet_grid( . ~ reg) +
  theme_bw() + 
  theme( axis.text = element_text( size = 14),
         axis.text.x = element_text( angle = 30, vjust = .5),
         axis.title = element_text( size = 18),
         axis.title.x = element_blank(),
         legend.position = c( .1, .2),
         legend.title = element_blank(),
         legend.text = element_text( size = 14),
         plot.margin = unit( c( .1, .2, .1, .1) , 'in'),
         strip.background = element_blank(),
         strip.text = element_text( size = 20))

gg_combined_reg <- 
  cowplot::plot_grid( gg_pw_race_abs + 
                        theme( axis.text.x = element_blank(),
                               axis.ticks.x = element_blank(),
                               legend.position = c( .9, .3)),
                      gg_pw_race + 
                        theme( legend.position = 'none',
                               strip.text = element_blank()),
                      ncol = 1,
                      # axis = 'l',
                      align = 'v')


ggsave( './figures/popwgt_region_combined.png',
        gg_combined_reg, width = 6.2, height = 5, units = 'in', scale = 1.6)


## ============================================
# expected relative disparities (fig si-4)
## ============================================
# regional expected disparities
gg_expected_disparities <- 
  ggplot( popwgt_idwe_race.reg[ !( is.na( reg))],
          aes( x = year,
               y = popwgt.frac,
               color = pop_name,
               group = pop_name)) + 
  geom_hline( yintercept = 1) +
  geom_line( size = 2) +
  scale_y_continuous( name = "Expected relative PWE") +
  scale_color_brewer( palette = 'Dark2') +
  facet_grid( . ~ reg) +
  theme_bw() + 
  theme( axis.text = element_text( size = 14),
         axis.text.x = element_text( angle = 30, vjust = .5),
         axis.title = element_text( size = 18),
         axis.title.x = element_blank(),
         legend.position = c( .1, .75),
         legend.title = element_blank(),
         legend.text = element_text( size = 14),
         plot.margin = unit( c( .1, .2, .1, .1) , 'in'),
         strip.background = element_blank(),
         strip.text = element_text( size = 20))
ggsave( './figures/pwe_expected_disparities.png',
        gg_expected_disparities,
        width = 8, height = 5, units = 'in', scale = 1)

## ============================================
# population-weighted exposure enhancement (fig 6)
## ============================================
## regional differences than expected
# regional expected disparities
gg_norm_region <- 
  ggplot( popwgt_reg,
          aes( x = year,
               y = popwgt.frac.hy / popwgt.frac,
               color = pop_name,
               group = pop_name)) + 
  geom_hline( yintercept = 1) +
  geom_line( size = 2) +
  scale_y_continuous( name = "Relative PWE enhancement") +
  scale_color_brewer( palette = 'Dark2') +
  facet_grid( . ~ reg) +
  theme_bw() + 
  theme( axis.text = element_text( size = 14),
         axis.text.x = element_text( ),
         axis.title = element_text( size = 18),
         axis.title.x = element_blank(),
         legend.position = c( .08, .8),
         legend.title = element_blank(),
         legend.text = element_text( size = 14),
         strip.background = element_blank(),
         strip.text = element_text( size = 20))


## spatial plot by state 
# merge state info by population-weighted exposure by state
popwgt_state.sf <- 
  merge( states, popwgt_state, by = 'state_abbr')

gg_norm_maps <- 
  ggplot( popwgt_state.sf[ popwgt_state.sf$year %in% c( 1999, 2020),],
          aes( fill = popwgt.frac.hy / popwgt.frac)) + 
  geom_sf() + 
  scale_fill_gradient2( name = 'Rel.\nPWE\nenhmnt',
                        breaks = c( 0.5, 1, 1.5),
                        midpoint = 1,
                        high = muted( "red"),
                        low = muted( "blue"),
                        limits = c( .5, 1.5), oob = scales::squish) +
  facet_grid( year ~ pop_name, switch = 'y') + 
  theme_bw() +
  theme( axis.text = element_blank(),
         axis.ticks = element_blank(),
         legend.position = 'left',
         legend.title = element_text( size = 12),
         legend.text = element_text( size = 12),
         panel.border = element_blank(),
         panel.grid = element_blank(),
         strip.background = element_blank(),
         strip.text = element_text( size = 16))

gg_combined_norm <- 
  cowplot::plot_grid( gg_norm_region,
                      gg_norm_maps,
                      ncol = 1,
                      rel_heights = c( .73, .4),
                      axis = 'l',
                      align = 'v')
ggsave( './figures/pwe_norm_timeseries_maps.png',
        gg_combined_norm,
        width = 8, height = 5, units = 'in', scale = 1.6)


## ================================================== 
## Create a plot of regions (figure si-9)
## ================================================== 
# collect spatial state info and merge with region definitions
states <- us_states()
states_use <- states[ !(states$state_abbr %in% c( 'AK','HI')),]
states_area <- merge( states_use,
                      state.desig,
                      by = 'state_abbr')

gg_regs <- 
  ggplot( states_area,
          aes( fill = reg)) + 
  geom_sf() + 
  scale_fill_brewer( palette = 3) +
  theme_minimal() + 
  theme( axis.text = element_blank(),
         legend.position = c( .9, .24),
         legend.title = element_blank(),
         panel.grid = element_blank())



ggsave( './figures/regions.png',
        gg_regs, width = 6.2, height = 3, units = 'in', scale = 1.5)




