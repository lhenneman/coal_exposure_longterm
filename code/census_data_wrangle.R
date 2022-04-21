library( data.table)

## before you begin, download the relevant census data at these locations. If you 
## are running code within this repo, download the files to data/census_data

## ============================================================
## download the data
## ============================================================
## 2010 - 2020
#  https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/asrh/
# README: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/cc-est2019-alldata.pdf

## 2000 - 2009
# https://www2.census.gov/programs-surveys/popest/datasets/2000-2009/counties/asrh/
# README: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/intercensal/county/co-est00int-alldata.pdf

# the following code will download the correct state-level files for 2010-2020
file.list <- paste0( 'cc-est2009-alldata-',
                     formatC( c( 1, 2, 4:6, 8:13,
                                 15:42, 44:51, 53:56), width = 2, flag = '0'),
                     '.csv')
lapply( file.list, function( f) {
  file.d <- paste0( 'https://www2.census.gov/programs-surveys/popest/datasets/2000-2009/counties/asrh/',
                    f)
  file.l <- paste0( './data/inputs/census_data/cc-est_2000-2009/',
                    f)
  # download.file( file.d, file.l)
})


## ============================================================
## read in the data
## ============================================================
# 2010-2020
yr_ref2010_2020 <- data.table( YEAR = 1:13,
                               year = c( 2010.1, 2010.2, 2010, 2011:2020))
popdat2010_2020.in <- fread( './data/inputs/census_data/CC-EST2020-ALLDATA6.csv')
popdat2010_2020 <-   merge( popdat2010_2020.in, yr_ref2010_2020, by = 'YEAR')[ year %in% 2010:2020]

## each state in 2000-2009 has its own demo
# define April pop 
yr_ref2000_2009 <- data.table( YEAR = 1:13,
                               year = c( 1999, 2000:2009, 2010.1, 2010))
files_2000_2009 <- list.files( './data/inputs/census_data/cc-est_2000-2009/',
                               full.names = TRUE)
popdat2000_2009.in <- lapply( files_2000_2009, fread) %>% rbindlist
popdat2000_2009 <-   merge( popdat2000_2009.in, yr_ref2000_2009, by = 'YEAR')[ year %in% 1999:2009]

# merge the lists
popdat <- rbind( popdat2000_2009, popdat2010_2020, fill = TRUE)[AGEGRP == 0]

# convert county to 3 character string to match spatial data
popdat[, `:=`( countyfp = formatC( COUNTY, width = 3, flag = '0'),
               statefp = formatC( STATE, width = 2, flag = '0'))]

# all race variables need to be numbereric
names_num <- grep( '_', names( popdat), value = TRUE)
popdat[, (names_num) := lapply( .SD, as.numeric), .SDcols = names_num]

# calculate totals by race
popdat[, `:=` ( white = WA_MALE + WA_FEMALE,
                black = BA_MALE + BA_FEMALE,
                native = IA_MALE + IA_FEMALE,
                asian = AA_MALE + AA_FEMALE,
                pacific = NA_MALE + NA_FEMALE,
                hispanic = H_MALE + H_FEMALE)]
popdat_age <- popdat[, .( statefp, countyfp, CTYNAME, year, AGEGRP,
                          TOT_POP, white, black, native, asian, pacific, hispanic)]

# sum across age groups
pop_all_yr <- popdat_age[, .( TOT_POP = sum( TOT_POP),
                              White = sum( white),
                              Black = sum( black),
                              Native = sum( native),
                              Asian = sum( asian),
                              Pacific = sum( pacific),
                              Hispanic = sum( hispanic)),
                         by = .( statefp, countyfp, CTYNAME, year)]

write.fst( pop_all_yr,
           'data/inputs/census_data/population_county.fst')
