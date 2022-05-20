library( fst)
library( data.table)
library( ggplot2)
library( tidyr)


## =================================================================
# read in data
## =================================================================
# read in unit pw exposure (created with )
popwgt_hyads_unit_in <- 
  read.fst( './data/outputs/popwgt_hyads_units_race.fst',
            as.data.table = TRUE)[pop_name == 'TOT_POP']
popwgt_hyads_unit_in[, uID := gsub( '^X', '', uID)]

# read in the race by unit dataset
# try just the southeastern units?
popwgt_hy_race_unit <- 
  read.fst( './data/outputs/popwgt_hyads_units_race.fst',
            as.data.table = TRUE)
popwgt_hy_race_unit[, uID := gsub( '^X', '', uID)]

# read in facility emissions
units_all_emiss <- 
  read.fst( 'data/inputs/coal_facility_data/units_coal_1999_2020.fst',
            as.data.table = TRUE)

# read in unit control installations
year_scrub_shut_info <- 
  read.fst( 'data/inputs/coal_facility_data/facility_operating_scrubbers_startyear.fst',
            as.data.table = TRUE)



## ===================================================================
# check 75%ile for heat input to differentiate high operation times
## ===================================================================
units_all_emiss[, Heat.Input75 := quantile( Heat.Input, .5), by = uID]
units_all_emiss[, Heat.Input75.check := Heat.Input > Heat.Input75]
units_all_emiss_HI75 <- 
  units_all_emiss[ (Heat.Input75.check)]


## ===================================================================
# assign 0 to those not open in early years
## ===================================================================
# create expanded grid
pw_year_all <- 
  expand_grid( year = 1999:2020,
               uID = unique( popwgt_hyads_unit_in$uID)) %>% 
  data.table

# merge with expanded grid
popwgt_hyads_unit <- merge( pw_year_all, popwgt_hyads_unit_in,
                            by = c( 'uID', 'year'), all.x = TRUE)

# assign 0 to all closed facilities
popwgt_hyads_unit[ is.na( pw.hy), pw.hy := 0]

## ===================================================================
## take average PW exposure for all years before scrubbers/shutterings
## ===================================================================
## start with merging popwgt & year_scrub_shut
# merge popwgt data with heat input info on whether above 75 %ile
popwgt_hyads_unit_yr <- 
  merge( popwgt_hyads_unit,
         year_scrub_shut_info[, .( uID, year_scrubbed, year_shut)],
         by = 'uID', all = TRUE) %>%
  merge( units_all_emiss[, .( uID, year, Heat.Input75.check)], 
         by = c( 'uID', 'year'), all = TRUE)
# merge popwgt data with

## set year_scrubbed and year_shut that are NA to very high year
popwgt_hyads_unit_yr[ is.na( year_scrubbed), year_scrubbed := 2050]
popwgt_hyads_unit_yr[ is.na( year_shut), year_shut := 2050]

## take the average PW_exp for years before, after scrubs and shuts
# what happens when there are no years less than year_shut?
# how to account for scrub before shut?
powgt_pre_shut_scrub <- 
  popwgt_hyads_unit_yr[ year < year_shut & year < year_scrubbed, 
                        .( pre_shut_pw = mean( pw.hy)),
                        by = uID]
powgt_pre_shut_scrubH <- 
  popwgt_hyads_unit_yr[ year < year_shut & year < year_scrubbed & (Heat.Input75.check), 
                        .( pre_shut_pwH = mean( pw.hy)),
                        by = uID]
powgt_pre_shut_scrubL <- 
  popwgt_hyads_unit_yr[ year < year_shut & year < year_scrubbed & !(Heat.Input75.check), 
                        .( pre_shut_pwL = mean( pw.hy)),
                        by = uID]
powgt_pre_shut_scrubHL <- 
  merge( powgt_pre_shut_scrub, powgt_pre_shut_scrubH, by = 'uID', all = T) %>%
  merge( powgt_pre_shut_scrubL, by = 'uID', all = T)


# for units that do not have averaging data, take 
# years that are equal to scrub/shut
powgt_pre_shut_scrub_ex <- 
  popwgt_hyads_unit_yr[ !( uID %in% unique( powgt_pre_shut_scrub$uID)) &
                          year <= year_shut & year < year_scrubbed, 
                        .( pre_shut_pw = mean( pw.hy)),
                        by = uID]
powgt_pre_shut_scrub_exH <- 
  popwgt_hyads_unit_yr[ !( uID %in% unique( powgt_pre_shut_scrub$uID)) &
                          year <= year_shut & year < year_scrubbed & (Heat.Input75.check), 
                        .( pre_shut_pwH = mean( pw.hy)),
                        by = uID]
powgt_pre_shut_scrub_exL <- 
  popwgt_hyads_unit_yr[ !( uID %in% unique( powgt_pre_shut_scrub$uID)) &
                          year <= year_shut & year < year_scrubbed & !(Heat.Input75.check), 
                        .( pre_shut_pwL = mean( pw.hy)),
                        by = uID]
powgt_pre_shut_scrub_exHL <- 
  merge( powgt_pre_shut_scrub_ex, powgt_pre_shut_scrub_exH, by = 'uID', all = T) %>%
  merge( powgt_pre_shut_scrub_exL, by = 'uID', all = T)


# take average popwgt post scrub
powgt_post_scrub <- popwgt_hyads_unit_yr[ year > year_scrubbed, 
                                          .( post_scrub_pw = mean( pw.hy)),
                                          by = uID]
powgt_post_scrubH <- popwgt_hyads_unit_yr[ year > year_scrubbed & ( Heat.Input75.check), 
                                           .( post_scrub_pwH = mean( pw.hy)),
                                           by = uID]
powgt_post_scrubL <- popwgt_hyads_unit_yr[ year > year_scrubbed & !( Heat.Input75.check), 
                                           .( post_scrub_pwL = mean( pw.hy)),
                                           by = uID]
powgt_post_scrubHL <- 
  merge( powgt_post_scrub, powgt_post_scrubH, by = 'uID', all = T) %>%
  merge( powgt_post_scrubL, by = 'uID', all = T)


# put together dataset of popwgt
# rbind the pre datasets
powgt_pre_shut_scrub_all <- 
  rbind( powgt_pre_shut_scrubHL, powgt_pre_shut_scrub_exHL)

# merge all datasets together
powgt_pre_post <- merge( powgt_pre_shut_scrub_all,  powgt_post_scrubHL, by = 'uID', all = TRUE)

# NA's are actually 0
powgt_pre_post[ is.na( powgt_pre_post)] <- 0

# efficiency savings
powgt_pre_post[ , `:=` ( effi_savings_pre_scrub  = pre_shut_pwH - pre_shut_pwL,
                         effi_savings_post_scrub = post_scrub_pwH - post_scrub_pwL)]
powgt_pre_post[ is.na( effi_savings_pre_scrub), effi_savings_pre_scrub := 0]
powgt_pre_post[ is.na( effi_savings_post_scrub), effi_savings_post_scrub := 0]

# scrubber savings
powgt_pre_post[ pre_shut_pw > 0, scrub_savings := pre_shut_pwH - post_scrub_pwH] # max( pre_shut_pwH - post_scrub_pwH, 0)
powgt_pre_post[ scrub_savings < 0, scrub_savings := 0] # max( pre_shut_pwH - post_scrub_pwH, 0)

# shutdown savings
powgt_pre_post[ , shut_savings := pre_shut_pwL] #min( pre_shut_pwH, pre_shut_pwL, na.rm = T), by = uID]

# NA's are actually 0
powgt_pre_post[ is.na( powgt_pre_post)] <- 0

## ===================================================================
## merge dataset back with years, expand and prepare to plot
## ===================================================================
# merge back with complete year-uID dataset
powgt_pre_post_yr <- merge( pw_year_all, powgt_pre_post,
                            by = c( 'uID'), all.x = TRUE)

# merge wth operating/scrubber dataset and max heat input test
powgt_uid_year_all_op <- 
  merge( powgt_pre_post_yr, popwgt_hyads_unit_yr[, .( uID, year, Heat.Input75.check)],
         by = c( 'uID', 'year')) %>%
  merge( popwgt_hyads_unit_yr[, .( uID, year_scrubbed, year_shut)] %>% unique,
         by = 'uID') 

# sum pre_shut_pw for all units that aren't shut or scrubbed, with consideration by efficiency
powgt_uncontrolled_pre_efic <- 
  powgt_uid_year_all_op[ year < year_shut & year < year_scrubbed & (Heat.Input75.check),
                         .( val = sum( pre_shut_pwH)), by = year][, type := 'uncontrolled_pre_efic']
powgt_uncontrolled_efic_savings <- 
  powgt_uid_year_all_op[ year < year_shut & year < year_scrubbed & !(Heat.Input75.check),
                         .( val = sum( effi_savings_pre_scrub)), by = year][, type := 'uncontrolled_efic_savings']
powgt_shut_down_savings <- 
  powgt_uid_year_all_op[ year >= year_shut & year < year_scrubbed,
                         .( val = sum( shut_savings)), by = year][, type := 'shut_down_savings']
powgt_scrubber_shut_down <- 
  powgt_uid_year_all_op[ year >= year_shut & year >= year_scrubbed,
                         .( val = sum( post_scrub_pw)), by = year][, type := 'scrubber_shut_down']
powgt_scrubber_avoided <- 
  powgt_uid_year_all_op[ year < year_shut & year >= year_scrubbed,
                         .( val = sum( scrub_savings)), by = year][, type := 'scrubber_avoided']
powgt_scrubber_contributed_pre_efic <- 
  powgt_uid_year_all_op[ year < year_shut & year >= year_scrubbed & (Heat.Input75.check),
                         .( val = sum( post_scrub_pwH)), by = year][, type := 'scrubber_contributed']
powgt_scrubber_efic_savings <- 
  powgt_uid_year_all_op[ year < year_shut & year >= year_scrubbed & !(Heat.Input75.check),
                         .( val = sum( effi_savings_post_scrub)), by = year][, type := 'scrubber_efic_savings']


# combine into a single list
popwt_plot <- list( powgt_uncontrolled_pre_efic,
                    powgt_uncontrolled_efic_savings,
                    powgt_shut_down_savings,
                    powgt_scrubber_shut_down,
                    powgt_scrubber_avoided,
                    powgt_scrubber_contributed_pre_efic,
                    powgt_scrubber_efic_savings) %>% rbindlist

## ===================================================================
## create figure for contribute/avoided PWE (Figure 4)
## ===================================================================
# factorize the type to define order
classes <- 
  c( 'scrubber_shut_down' = 'Avoided: retirement after scrubber',
     'shut_down_savings' = 'Avoided: retirement',
     'scrubber_avoided' = 'Avoided: scrubber',
     "scrubber_efic_savings" = "Avoided: reduced operation after scrubber",
     "uncontrolled_efic_savings" = 'Avoided: reduced operation before scrubber',
     'scrubber_contributed' = 'Contributed: scrubber',
     'uncontrolled_pre_efic' = 'Contributed: uncontrolled'
  )

popwt_plot[, classnames := classes[type]]

popwt_plot$classnames <- 
  factor( popwt_plot$classnames, 
          levels = classes)

# define some colors
colors <- 
  c( 'Avoided: retirement after scrubber' =  "#27187E", #'#648DE5',
     'Avoided: retirement' =  '#304C89', #'#304C89',
     'Avoided: scrubber' = '#648DE5',#'#9EB7E5',
     "Avoided: reduced operation after scrubber" = '#9EB7E5', #'#D1F5FF',
     'Avoided: reduced operation before scrubber' = '#EEF8FF',#
     'Contributed: scrubber' = 'grey65',
     'Contributed: uncontrolled' = 'grey50'
  )

# make the plot
gg_pw <- 
  ggplot( popwt_plot,
          aes( y = val,
               x = year,
               fill = classnames)) + 
  geom_area( position = "fill") + 
  scale_y_continuous( labels = scales::percent,
                      name = '% basline PWE contributed or avoided',
                      expand = c( 0, 0)) +
  scale_x_continuous( expand = c( 0, 0)) +
  # scale_fill_brewer( palette = 'Dark2') + 
  scale_fill_manual( values = colors) +
  # facet_wrap( . ~ class) + 
  theme_bw() + 
  theme( axis.text = element_text( size = 14),
         axis.title = element_text( size = 18),
         axis.title.x = element_blank(),
         legend.position = c( .225, .22),
         legend.text = element_text( size = 14),
         legend.title = element_blank(),
         plot.background = element_blank(),
         plot.margin = unit( c( .2, .3, .1, .1), 'in'),
         strip.background = element_blank(),
         strip.text = element_text( size = 20))

ggsave( 'figures/popwgt_evolution.png',
        gg_pw, 
        width = 6.2, height = 3, units = 'in', scale = 1.78)









