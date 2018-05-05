# Census data set up data for UCW-CWA

library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
library(tmap)
library(tmaptools)
library(sf)

gdit_cty <- unique(GDIT_WD_History_at_GDIT_sites$County)

florida <- get_acs(geography = 'county',
                   variables = 'B03002_001E',
                   state = c("FL"),
                   year = 2015, geometry = TRUE)

ms <- get_acs(geography = 'county',
              variables = c("B19013_001E",'B25001_001E','B25002_002E',
                            'B25002_003E','B25003_001E','B25003_002E',
                            'B25003_003E',  'B02001_001E',
                            'B02001_002E',
                            'B02001_003E',
                            'B17001_001E',
                            'B17001_002E'),
              state = c('MS'),
              survey = 'acs5',
              output = 'wide',
              geometry = TRUE) %>%
  rename(hhincome16 = "B19013_001E",
         tothu16 = 'B25001_001E',
         totocc16 = 'B25002_002E',
         totvac16 = 'B25002_003E',
         tottenure16 = 'B25003_001E',
         ownocc16 = 'B25003_002E',
         rentocc16 = 'B25003_003E',
         tpop16 = 'B02001_001E',
         white16 = 'B02001_002E',
         black16 = 'B02001_003E',
         tpov16 = 'B17001_001E',
         ipov16 = 'B17001_002E') %>%
  mutate(hopct16 = round(100 * (ownocc16/tothu16),1),
         rntpct16 = round(100 * rentocc16/tothu16),1,
         occpct16 = round(100 * totocc16/tothu16), 1,
         vacpct16 = round(100 * totvac16/tothu16), 1,
         whtpct16 = round(100 * (white16/tpop16), 1),
         blkpct16 = round(100 * (black16/tpop16), 1),
         povrt16 = round(100 * (ipov16/tpov16),1))

#c('Forrest', 'Lamar', 'Jones', 'Wayne', 'Jasper', 'Greene',
#  'Covington', 'Marion', 'Stone', 'George', 'Pearl River',
#  'Hancock', 'Harrison', 'Jackson', 'Walthall', 'Lawrence', 'Pike'),

class(ms)

tm_shape(ms) +
  tm_polygons('hhincome16')

tm_shape(ms) +
  tm_polygons('blkpct16')

gdit_st
