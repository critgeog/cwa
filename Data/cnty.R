
library(tidyverse)
library(tidycensus)
library(sf)

#X
tx_cnty <- c("McLennan","Falls","Bell","Coryell","Limestone","Hill","Bosque","Freestone",
             "Hamilton","Leon","Mills","Cameron","Hidalgo","Willacy","Jim Hogg","Kenedy",
             "Starr","Zapata","Brooks","Duval")

va_cnty <- c("Chesterfield","Amelia","Caroline","Charles City","Dinwiddie","Goochland","Hanover",
             "Henrico","King William","New Kent","Powhatan","Prince George","Sussex","Brunswick",
             "Charlotte County","Greensville","Cumberland","Nottoway","Lunenburg","Prince Edward",
             "Southampton","Surry","James City","York","Norfolk","Portsmouth","Suffolk","Virginia Beach",
             "Williamsburg","Gloucester","Chesapeake","Hampton","Isle of Wight","Spotsylvania",
             "Louisa","Orange","Fluvanna","Albemarle","Buckingham","King and Queen","Lancaster",
             "Middlesex","Northumberland","Westmoreland","Essex","Fairfax County","Alexandria city",
             "Arlington","Falls Church city","Fauquier","Loudoun","Prince William","Stafford","Manassas city",
             "Manassas Park city","Clarke","Rappahannock","Warren","Fredericksburg city",
             "Spotsylvania","King George", "Richmond County","Fredericksburg city", "Mathews",
             "Newport News","Culpeper")

nc_cnty <- c("Gates", "Currituck", "Mecklenburg")

ut_cnty <- c("Salt Lake","Tooele","Davis","Morgan","Weber","Box Elder",'Piute',"Summit",
             "Wasatch","Rich","Juab","Utah")

nm_cnty <- c("Dona Ana","Luna","Sierra","Grant","Hidalgo","Otero","Chaves","Eddy","Lincoln")

ky_cnty <- c("Clark","Fayette","Jessamine","Scott","Woodford","Bourbon","Madison","Menifee",
             "Mercer","Montgomery","Nicholas","Owen","Powell","Robertson","Rockcastle","Rowan",
             "Washington","Anderson","Bath","Boyle","Estill","Fleming","Franklin","Garrard",
             "Harrison","Lincoln","Laurel","Whitley","Marion","McCreary","Pulaski","Taylor",
             "Wayne","Whitley","Marion","Casey","Knox","Bell","Breathitt",
             "Clay","Harlan","Jackson","Knott","Lee","Leslie","Letcher","Morgan","Owsley",
             "Perry","Wolfe", "Green County")

ms_cnty <- c("Forrest","Perry","Lamar","Jones","Clarke","Greene","Jasper","Kemper",
             "Lauderdale","Neshoba","Newton","Wayne","Covington","Jefferson Davis",
             "Leake","Marion","Scott","Smith","George","Pearl River","Stone",
             "Adams","Amite","Claiborne","Franklin","Lawrence","Lamar",
             "Lincoln","Pike","Walthall","Warren","Wilkinson", "Jefferson County")

la_cnty <- c("Washington","Jefferson Parish","Orleans","Plaquemines","St. Bernard","St. Charles",
             "St. John the Baptist","St. Tammany")

ks_cnty <- c("Douglas","Johnson","Leavenworth","Linn","Miami","Wyandotte","Jackson",
             "Jefferson","Osage","Shawnee","Wabaunsee","Anderson","Franklin")

mo_cnty <- c("Bates","Caldwell","Cass","Clay","Clinton","Jackson","Lafayette","Platte","Ray")

az_cnty <- c("Maricopa","Pinal","Yavapai","Pima","Yuma","Apache","Gila","Navajo","La Paz",
             "Graham","Greenlee","Santa Cruz")

md_cnty <- c("Calvert","Charles","Prince George's County","St. Mary's","Montgomery",
             "Frederick","Anne Arundel","Baltimore city","Baltimore County","Carroll",
             "Harford","Howard")

fl_cnty <- c("Duval","Baker","Clay","Nassau","St. Johns","Flagler","Volusia",
             "Columbia","Hamilton","Lafayette","Madison","Putnam","Suwannee",
             "Taylor","Alachua",'Gilchrist',"Bay","Gulf","Walton","Calhoun","Franklin",
             "Holmes","Jackson","Liberty","Washington","Hillsborough","Hernando",
             "Pasco","Pinellas","Polk","Manatee","Sarasota","Sumter","Citrus")

ga_cnty <- c("Camden","Charlton","Pierce","Atkinson","Ben Hill","Berrien","Calhoun","Clinch",
             "Coffee","Colquitt","Cook","Decatur","Grady","Irwin","Mitchell",
             "Randolph","Thomas","Tift","Turner","Ware")

ia_cnty <- c("Johnson","Washington","Cedar","Clinton","Iowa","Jackson","Jefferson",
             "Keokuk","Tama","Van Buren","Des Moines","Henry","Lee","Louisa","Muscatine",
             "Benton","Jones","Linn")

iowa <- get_acs(geography = "county",
                    variables = c('B01003_001E',
                                  'B02001_003E',
                                  'B03001_003E'),
                    county = ia_cnty,
                    state = "IA",
                    survey = "acs5",
                    year = 2015,
                    output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))

ga <- get_acs(geography = "county",
                variables = c('B01003_001E',
                              'B02001_003E',
                              'B03001_003E'),
                county = ga_cnty,
                state = "GA",
                survey = "acs5",
                year = 2015,
                output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))

fl <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = fl_cnty,
              state = "FL",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))

md <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = md_cnty,
              state = "md",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))

az <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = az_cnty,
              state = "AZ",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))

mo <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = mo_cnty,
              state = "MO",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))

ks <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = ks_cnty,
              state = "KS",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))

ut <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = ut_cnty,
              state = "UT",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))

nm <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = nm_cnty,
              state = "NM",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))

tx <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = tx_cnty,
              state = "TX",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))

ky <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = ky_cnty,
              state = "KY",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))

ms <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = ms_cnty,
              state = "MS",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))

md <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = md_cnty,
              state = "MD",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))


la <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = la_cnty,
              state = "LA",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))


va <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = va_cnty,
              state = "VA",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))

nc <- get_acs(geography = "county",
              variables = c('B01003_001E',
                            'B02001_003E',
                            'B03001_003E'),
              county = nc_cnty,
              state = "NC",
              survey = "acs5",
              year = 2015,
              output = 'wide') %>%
  rename(tpop15 = 'B01003_001E',
         black15 = 'B02001_003E',
         hisp15 = 'B03001_003E')  %>%
  mutate(blkpct15 = round(100 * (black15/tpop15), 3),
         hispct15 = round(100 * (hisp15/tpop15),3))


cwa <- rbind(az, fl, ga, iowa, ks, ky, la, md, mo, ms, nc, nm, tx, ut, va)


#write_csv(cwa, "Data/cwa.csv")
#cwa_wd <- read_csv("Data/cwa_data.csv")
#Aaz, fl, ga, iowa, ks, ky, la, md, mo, ms, nc, nm, tx, ut, va

cwa$NAME %>% 
  str_replace(" County", "") %>%
  str_trim()

cwa$NAME <- cwa$NAME %>%
  str_replace(" County", "") %>%
  str_trim()

cwa$NAME %>%
  str_split_fixed(", ", n = 2)

cwa <- cwa %>%
  separate(NAME, c("county", "state"),',') %>%
  select(county, state, GEOID:hispct15)

state <- c("Arizona", "Georgia", "Virginia")
abbr <- c("AZ", "GA", "VA")

cwa$state %>%
  str_trim %>%
  str_replace_all(state, abbr)

#str_replace(cwa$NAME, "Virginia", "VA")
#str_replace(cwa$NAME, "Virginia", "VA")
#cwa$NAME



temp <- strsplit(cwa$NAME, ", ")
mat <- matrix(unlist(temp), ncol =2, byrow = TRUE)
mat
cwa1 <- as.tibble(mat)
cwa1  
colnames(cwa1) <- c("County", "State")
cwa1



cwa1a <- left_join(cwa_wd, cwa1, by.x = c('county','state'), by.y = c('County", "State'))

#321

temp <- strsplit(cwa$NAME, ", ")
temp
#cwa2 <- left_join(cwa_wd, cwa, by.x = 'county')

df <- as.data.frame(mat)
df
colnames(df) <- c("County", "State")
df
head(df)
?str_replace
library(stringr)
#strings = c("TGAS_1121", "MGAS_1432", "ATGAS_1121") 


fruits <- c("one apple", "two pears", "three bananas")
str_replace(fruits, "[aeiou]", "-")

str_replace_all(fruits, "[aeiou]", "-")

str_replace_all(fruits, "[aeiou]", toupper)
str_replace_all(fruits, "b", NA_character_)

str_replace(fruits, "([aeiou])", "")
str_replace(fruits, "([aeiou])", "\\1\\1")
str_replace(fruits, "[aeiou]", c("1", "2", "3"))
str_replace(fruits, c("a", "e", "i"), "-")

# If you want to apply multiple patterns and replacements to the same
# string, pass a named vector to pattern.
fruits %>%
  str_c(collapse = "---") %>%
  str_replace_all(c("one" = "1", "two" = "2", "three" = "3"))

# Use a function for more sophisticated replacement. This example
# replaces colour names with their hex values.
colours <- str_c("\\b", colors(), "\\b", collapse="|")
col2hex <- function(col) {
  rgb <- col2rgb(col)
  rgb(rgb["red", ], rgb["green", ], rgb["blue", ], max = 255)
}

x <- c(
  "Roses are red, violets are blue",
  "My favourite colour is green"
)
str_replace_all(x, colours, col2hex)


ms_cwa <- get_acs(geography = "tract", 
                    variables = c("B19013_001E",'B25001_001E','B25002_002E',
                                  'B25002_003E','B25003_001E','B25003_002E',
                                  'B25003_003E',  'B02001_001E',
                                  'B02001_002E',
                                  'B02001_003E',
                                  'B17001_001E',
                                  'B17001_002E'),
                    county = ms_cnty,
                    state = "MS",
                    year = 2015,
                    survey = "acs5",
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

ms_cnty <- c("Forrest","Perry","Lamar","Jones","Clarke","Greene","Jasper","Kemper",
             "Lauderdale","Neshoba","Newton","Wayne","Covington","Jefferson Davis",
             "Leake","Marion","Scott","Smith","George","Pearl River","Stone",
             "Adams","Amite","Claiborne","Franklin","Lawrence","Lamar",
             "Lincoln","Pike","Walthall","Wilkinson", "Jefferson County")

#ms_cnty2 <- c("Forrest","Perry","Lamar","Jones","Clarke","Greene","Jasper","Kemper",
#             "Lauderdale","Neshoba","Newton","Wayne","Covington","Jefferson Davis",
#             "Leake","Marion","Scott","Smith","George","Pearl River","Stone","Simpson",
#             "Adams","Amite","Claiborne","Franklin","Lawrence","Lamar","Copiah","Hinds","Warren","Rankin",
#             "Lincoln","Pike","Walthall","Wilkinson", "Jefferson County")


tm_shape(ms_cwa) +
  tm_polygons('hhincome16')

tm_shape(ms_cwa_cnty) +
  tm_polygons('hhincome16')

rd <- primary_roads(year = 2016)

ms_cwa_cnty <- ms_cwa_cnty %>%
  st_transform(32616)

st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
             +units=m +no_defs")

## plot yea things
county <-   
  tm_shape(ms_cwa) + 
  tm_polygons('blkpct16', style = 'jenks', palette = 'Greens',
          title = 'Percent Black, 2016') +
  tm_shape(ms_cwa_cnty) + 
  tm_borders(col = "black") + 
    tm_shape(rd) +
  tm_lines(col = "black") +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = 0.8,
            legend.title.size = 1)
county

## plot yea things
cnty_cwa <-   
  tm_shape(ms_cwa_cnty) + 
  tm_polygons('blkpct16', style = 'jenks', palette = "Greens",
              title = 'Percent Black, 2016') +
  tm_shape(ms_cwa_cnty) + 
  tm_borders(col = "black") + 
  tm_shape(rd) +
  tm_lines(col = "black") +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = 0.8,
            legend.title.size = 1) +
  tm_style_grey(bg.color = '#272728')
cnty_cwa
