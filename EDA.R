rm(list=ls())
library(tesseract)
library(magick)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(viridis)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

#census_api_key("8612e820e2b6a2d0c6f10895444971d2479336be", install = TRUE)
#readRenviron("~/.Renviron")

# v14,v15 just have the variables that can be used in get_acs(), click on them to see variable options
v15<- load_variables(2016, "acs1", cache=TRUE)
v14<- load_variables(2016, "acs1/profile", cache=TRUE)


# Customizing for our Question
acs<- get_acs(geography = "tract", state= c("WA", "OR", "CA", "TX", "IL", "WI", "NJ", "MN", "HI", "DE",
                                            "MD", "UT", "VA", "OH", "NY", "DC", "WV", "MA", "NH", "CO"), geometry = TRUE, 
              variables = c("DP03_0002", "DP03_0011", "DP03_0001", "DP03_0010"))

female_pop1<- acs %>% 
  filter(variable=="DP03_0010")
female_lforce1<- acs %>% 
  filter(variable=="DP03_0011")
total_pop1<- acs %>% 
  filter(variable=="DP03_0001")
total_lforce1<- acs %>% 
  filter(variable=="DP03_0002")

#GEO IDs for 3 MSAs out West (once we determine what MSAs to use this can be changed to reflect that)
metros <- core_based_statistical_areas(cb = TRUE) %>%
  dplyr::filter(GEOID %in% c("47900","41860","14460","16980","35620","42660","38300",
                             "37980","38900","33460","31540","19740","26180","31100",
                             "12580","41620","40060","26240","17460","15380")) %>%
  select(metro_name = NAME)

female_lforce <- st_join(female_lforce1, metros, join = st_within, 
                         left = FALSE) 
total_lforce <- st_join(total_lforce1, metros, join = st_within, 
                        left = FALSE) 
female_pop <- st_join(female_pop1, metros, join = st_within, 
                      left = FALSE) 
total_pop <- st_join(total_pop1, metros, join = st_within, 
                     left = FALSE) 
metro_areas<- unique(female_lforce$metro_name)
metro_areas
#Total Labor Force Calculations and Graphs

datalist<- list()
lfprcalculatordata<- function(x, i) {
  localwomenlforce<- female_lforce %>% 
    filter(metro_name== x[i])
  lfwomenlocal<- sum(localwomenlforce$estimate)
  
  localtotallforce<- total_lforce %>% 
    filter(metro_name==x[i])
  lftotallocal<- sum(localtotallforce$estimate)
  
  localpopwomen<- female_pop %>%
    filter(metro_name==x[i])
  popwomenlocal<- sum(localpopwomen$estimate)
  
  localpoptotal<- total_pop %>%
    filter(metro_name==x[i])
  poptotallocal<- sum(localpoptotal$estimate)
  
  #Labor Force Participation Rate for San Francisco
  total_lfpr <- as.data.frame(lftotallocal/poptotallocal)
  women_lfpr <- as.data.frame(lfwomenlocal/popwomenlocal)
  
  LFPR_Women_local<- localwomenlforce$estimate/localpopwomen$estimate
  women_population_local<- localpopwomen$estimate
  LFPR_Total_local<- localtotallforce$estimate/localpoptotal$estimate
  total_population_local<- localpoptotal$estimate
  local_total<- cbind(localtotallforce, LFPR_Total_local, total_population_local, LFPR_Women_local, women_population_local)
  
  datalist<<- cbind(metro_areas[i], total_lfpr, women_lfpr)
}
lfprwomangrapher<- function(x) {
  localwomenlforce<- female_lforce %>% 
    filter(metro_name== x)
  lfwomenlocal<- sum(localwomenlforce$estimate)
  
  localtotallforce<- total_lforce %>% 
    filter(metro_name==x)
  lftotallocal<- sum(localtotallforce$estimate)
  
  localpopwomen<- female_pop %>%
    filter(metro_name==x)
  popwomenlocal<- sum(localpopwomen$estimate)
  
  localpoptotal<- total_pop %>%
    filter(metro_name==x)
  poptotallocal<- sum(localpoptotal$estimate)
  
  #Labor Force Participation Rate for San Francisco
  lftotallocal/poptotallocal
  lfwomenlocal/popwomenlocal
  
  LFPR_Women_local<- localwomenlforce$estimate/localpopwomen$estimate
  women_population_local<- localpopwomen$estimate
  LFPR_Total_local<- localtotallforce$estimate/localpoptotal$estimate
  total_population_local<- localpoptotal$estimate
  local_total<- cbind(localtotallforce, LFPR_Total_local, total_population_local, LFPR_Women_local, women_population_local)
  
  # Women
  ggplot(local_total, aes(fill = LFPR_Women_local, color = LFPR_Women_local)) + 
    geom_sf() + 
    coord_sf(crs = 26910) + 
    theme_minimal() + 
    theme(aspect.ratio = 1) + 
    scale_fill_viridis() + 
    scale_color_viridis()+ ggtitle(x)
}
lfprtotalgrapher<- function(x) {
  localwomenlforce<- female_lforce %>% 
    filter(metro_name== x)
  lfwomenlocal<- sum(localwomenlforce$estimate)
  
  localtotallforce<- total_lforce %>% 
    filter(metro_name==x)
  lftotallocal<- sum(localtotallforce$estimate)
  
  localpopwomen<- female_pop %>%
    filter(metro_name==x)
  popwomenlocal<- sum(localpopwomen$estimate)
  
  localpoptotal<- total_pop %>%
    filter(metro_name==x)
  poptotallocal<- sum(localpoptotal$estimate)
  
  #Labor Force Participation Rate for San Francisco
  lftotallocal/poptotallocal
  lfwomenlocal/popwomenlocal
  
  LFPR_Women_local<- localwomenlforce$estimate/localpopwomen$estimate
  women_population_local<- localpopwomen$estimate
  LFPR_Total_local<- localtotallforce$estimate/localpoptotal$estimate
  total_population_local<- localpoptotal$estimate
  local_total<- cbind(localtotallforce, LFPR_Total_local, total_population_local, LFPR_Women_local, women_population_local)
  
  # Graphing MSA Labor Force Participation Rate
  par(mfrow=c(2,1))
  ggplot(local_total, aes(fill = LFPR_Total_local, color = LFPR_Total_local)) + 
    geom_sf() + 
    coord_sf(crs = 26910) + 
    theme_minimal() + 
    theme(aspect.ratio = 1) + 
    scale_fill_viridis() + 
    scale_color_viridis()+ ggtitle(x)
}
# Enter "MSA" in grapher to graph

lfprcalculatordata(metro_areas, 1)
a<- datalist
lfprcalculatordata(metro_areas, 2)
b<- datalist
lfprcalculatordata(metro_areas, 3)
c<- datalist
lfprcalculatordata(metro_areas, 4)
d<- datalist
lfprcalculatordata(metro_areas, 5)
e<- datalist
lfprcalculatordata(metro_areas, 6)
f<- datalist
lfprcalculatordata(metro_areas, 7)
g<- datalist
lfprcalculatordata(metro_areas, 8)
h<- datalist
lfprcalculatordata(metro_areas, 9)
i<- datalist
lfprcalculatordata(metro_areas, 10)
j<- datalist
lfprcalculatordata(metro_areas, 11)
k<- datalist
lfprcalculatordata(metro_areas, 12)
l<- datalist
lfprcalculatordata(metro_areas, 13)
m<- datalist
lfprcalculatordata(metro_areas, 14)
n<- datalist
lfprcalculatordata(metro_areas, 15)
o<- datalist
lfprcalculatordata(metro_areas, 16)
p<- datalist
datatable1<- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
names(datatable1)<- c("MSA", "Total_LFPR", "Woman_LFPR")

# Bringing in Crime Data
crimedata<- read.csv("crime_data_eda.csv", header = TRUE)
crimedata2<- crimedata %>% 
  mutate(county_name=toupper(county_name))
# Getting MSA Specific Crime Data

# San Francisco
sanfrancrimedata<- crimedata2 %>% 
  filter(county_name=="SAN FRANCISCO COUNTY, CA" |
           county_name=="CONTRA COSTA COUNTY, CA"| 
           county_name== "ALAMEDA COUNTY, CA"|
           county_name=="SAN MATEO COUNTY, CA"|
           county_name=="MARIN COUNTY, CA")
sfcrimerateper100k<- sum(sanfrancrimedata$crime_rate_per_100000*sanfrancrimedata$population)/(sum(sanfrancrimedata$population))
San_Fran_MSA_Crime<- c("San Francisco-Oakland-Hayward, CA", sfcrimerateper100k, sum(sanfrancrimedata$MURDER), sum(sanfrancrimedata$RAPE),sum(sanfrancrimedata$ROBBERY))

# Seattle
seattlecrimedata<- crimedata2 %>% 
  filter(county_name=="KING COUNTY, WA" |
           county_name=="SNOHOMISH COUNTY, WA"| 
           county_name== "PIERCE COUNTY, WA")
seattlecrimerateper100k<- sum(seattlecrimedata$crime_rate_per_100000*seattlecrimedata$population)/(sum(seattlecrimedata$population))
Seattle_MSA_Crime<- c("Seattle-Tacoma-Bellevue, WA", seattlecrimerateper100k, sum(seattlecrimedata$MURDER), sum(seattlecrimedata$RAPE),sum(seattlecrimedata$ROBBERY))

# Portland
portlandcrimedata<- crimedata2 %>% 
  filter(county_name=="CLACKAMAS COUNTY, OR" |
           county_name=="COLUMBIA COUNTY, OR"| 
           county_name== "MULTNOMAH COUNTY, OR"|
           county_name=="WASHINGTON COUNTY, OR"|
           county_name=="YAMHILL COUNTY, OR" |
           county_name=="CLARK COUNTY, WA"|
           county_name=="SKAMANIA COUNTY, WA")
portlandcrimerateper100k<- sum(portlandcrimedata$crime_rate_per_100000*portlandcrimedata$population)/(sum(portlandcrimedata$population))
Portland_MSA_Crime<- c("Portland-Vancouver-Hillsboro, OR-WA", portlandcrimerateper100k, sum(portlandcrimedata$MURDER), sum(portlandcrimedata$RAPE),sum(portlandcrimedata$ROBBERY))

#Madison
madisoncrimedata<- crimedata2 %>% 
  filter(county_name=="COLUMBIA COUNTY, WI"|
           county_name=="DANE COUNTY, WI"| 
           county_name== "GREEN COUNTY, WI"|
           county_name=="IOWA COUNTY, WI")
madisoncrimerateper100k<- sum(madisoncrimedata$crime_rate_per_100000*madisoncrimedata$population)/(sum(madisoncrimedata$population))
Madison_MSA_Crime<- c("Madison, WI", madisoncrimerateper100k, sum(madisoncrimedata$MURDER), sum(madisoncrimedata$RAPE),sum(madisoncrimedata$ROBBERY))

#Chicago
chicagocrimedata<- crimedata2 %>% 
  filter(county_name=="COOK COUNTY, IL" | 
           county_name=="DEKALB COUNTY, IL"| 
           county_name== "DUPAGE COUNTY, IL"|
           county_name== "GRUNDY COUNTY, IL"|
           county_name== "KANE COUNTY, IL"|
           county_name== "KENDALL COUNTY, IL"|
           county_name== "MCHENRY COUNTY, IL"|
           county_name== "WILL COUNTY, IL"|
           county_name== "JASPER COUNTY, IN"|
           county_name== "LAKE COUNTY, IN"|
           county_name== "NEWTON COUNTY, IN"|
           county_name== "PORTER COUNTY, IN"|
           county_name== "LAKE COUNTY, IL"|
           county_name== "KENOSHA COUNTY, WI")
chicagocrimerateper100k<- sum(chicagocrimedata$crime_rate_per_100000*chicagocrimedata$population)/
  (sum(chicagocrimedata$population))
chicago_MSA_Crime<- c("Chicago-Naperville-Elgin, IL-IN-WI", chicagocrimerateper100k, 
                      sum(chicagocrimedata$MURDER), sum(chicagocrimedata$RAPE),sum(chicagocrimedata$ROBBERY))
#Minneapolis
mincrimedata<- crimedata2 %>% 
  filter(county_name=="HENNEPIN COUNTY, MN" | 
           county_name=="RAMSEY COUNTY, MN"| 
           county_name== "DAKOTA COUNTY, MN"|
           county_name== "ANOKA COUNTY, MN"|
           county_name== "WASHINGTON COUNTY, MN"|
           county_name== "SCOTT COUNTY, MN"|
           county_name== "CARVER COUNTY, MN"|
           county_name== "WRIGHT COUNTY, MN"|
           county_name== "SHERBURNE COUNTY, MN"|
           county_name== "ST. CROIX COUNTY, WI"|
           county_name== "CHISAGO COUNTY, MN"|
           county_name== "PIERCE COUNTY, WI"|
           county_name== "ISANTI COUNTY, MN"|
           county_name== "LE SUEUR COUNTY, MN"|
           county_name== "MILLE LACS COUNTY, MN"|
           county_name== "SIBLEY COUNTY, MN"|
           county_name== "BENTON COUNTY, MN")
mincrimerateper100k<- sum(mincrimedata$crime_rate_per_100000*mincrimedata$population)/
  (sum(mincrimedata$population))
min_MSA_Crime<- c("Minneapolis-St. Paul-Bloomington, MN-WI", mincrimerateper100k, 
                  sum(mincrimedata$MURDER), sum(mincrimedata$RAPE),sum(mincrimedata$ROBBERY))
#Philadelphia
philly_crimedata<- crimedata2 %>% 
  filter(county_name== "KENT COUNTY, DE" | 
           county_name== "NEW CASTLE COUNTY, DE"| 
           county_name== "CECIL COUNTY, MD"| 
           county_name== "ATLANTIC COUNTY, NJ"| 
           county_name== "BURLINGTON COUNTY, NJ"|
           county_name== "CAMDEN COUNTY, NJ"|
           county_name== "CAPE MAY COUNTY, NJ"|
           county_name== "CUMBERLAND COUNTY, NJ"|
           county_name== "GLOUCESTER COUNTY, NJ"|
           county_name== "SALEM COUNTY, NJ"|
           county_name== "BERKS COUNTY, PA"|
           county_name== "BUCKS COUNTY, PA"|
           county_name== "CHESTER COUNTY, PA"|
           county_name== "DELAWARE COUNTY, PA"|
           county_name== "MONTGOMERY COUNTY, PA"|
           county_name== "PHILADELPHIA COUNTY, PA")
philly_crimerateper100k<- sum(philly_crimedata$crime_rate_per_100000*philly_crimedata$population)/
  (sum(philly_crimedata$population))
philly_MSA_Crime<- c("Philadelphia-Camden-Wilmington, PA-NJ-DE-MD", philly_crimerateper100k, sum(philly_crimedata$MURDER), 
                     sum(philly_crimedata$RAPE),sum(philly_crimedata$ROBBERY))
#Washington
wash_crimedata<- crimedata2 %>% 
  filter(county_name== "DISTRICT OF COLUMBIA, DC" | 
           county_name== "CALVERT COUNTY, MD"| 
           county_name== "CHARLES COUNTY, MD"| 
           county_name== "FREDERICK COUNTY, MD"| 
           county_name== "MONTGOMERY COUNTY, MD"|
           county_name== "PRINCE GEORGE'S COUNTY, MD"|
           county_name== "ARLINGTON COUNTY, VA"|
           county_name== "CLARKE COUNTY, VA"|
           county_name== "CULPEPER COUNTY, VA"|
           county_name== "FAIRFAX COUNTY, VA"|
           county_name== "FALLS CHURCH CITY, VA"|
           county_name== "FAUQUIER COUNTY, VA"|
           county_name== "LOUDOUN COUNTY, VA"|
           county_name== "PRINCE WILLIAM COUNTY, VA"|
           county_name== "RAPPAHANNOCK COUNTY, VA"|
           county_name== "SPOTSYLVANIA COUNTY, VA"|
           county_name== "STAFFORD COUNTY, VA"|
           county_name== "WARREN COUNTY, VA"|
           county_name== "JEFFERSON COUNTY, WV")
wash_crimerateper100k<- sum(wash_crimedata$crime_rate_per_100000*wash_crimedata$population)/
  (sum(wash_crimedata$population))
wash_MSA_Crime<- c("Washington-Arlington-Alexandria, DC-VA-MD-WV", wash_crimerateper100k, sum(wash_crimedata$MURDER), 
                   sum(wash_crimedata$RAPE),sum(wash_crimedata$ROBBERY))
#Richmond
richmond_crimedata<- crimedata2 %>% 
  filter(county_name== "AMELIA COUNTY, VA" | 
           county_name== "CAROLINE COUNTY, VA"| 
           county_name== "CHARLES CITY COUNTY, VA"| 
           county_name== "CHESTERFIELD COUNTY, VA"| 
           county_name== "DINWIDDIE COUNTY, VA"|
           county_name== "GOOCHLAND COUNTY, VA"|
           county_name== "HANOVER COUNTY, VA"|
           county_name== "HENRICO COUNTY, VA"|
           county_name== "KING WILLIAM COUNTY, VA"|
           county_name== "NEW KENT COUNTY, VA"|
           county_name== "POWHATAN COUNTY, VA"|
           county_name== "PRINCE GEORGE COUNTY, VA"|
           county_name== "SUSSEX COUNTY, VA")
richmond_crimerateper100k<- sum(richmond_crimedata$crime_rate_per_100000*richmond_crimedata$population)/
  (sum(richmond_crimedata$population))
richmond_MSA_Crime<- c("Richmond, VA", richmond_crimerateper100k, sum(richmond_crimedata$MURDER), 
                       sum(richmond_crimedata$RAPE),sum(richmond_crimedata$ROBBERY))
#Buffalo
Buffalo_crimedata<- crimedata2 %>% 
  filter(county_name=="ERIE COUNTY, NY" | 
           county_name=="NIAGARA COUNTY, NY")
Buffalo_crimerateper100k<- sum(Buffalo_crimedata$crime_rate_per_100000*Buffalo_crimedata$population)/
  (sum(Buffalo_crimedata$population))
Buffalo_MSA_Crime<- c("Buffalo-Cheektowaga-Niagara Falls, NY", Buffalo_crimerateper100k, sum(Buffalo_crimedata$MURDER), 
                      sum(Buffalo_crimedata$RAPE),sum(Buffalo_crimedata$ROBBERY))
#Boston
Boston_crimedata<- crimedata2 %>% 
  filter(county_name=="NORFOLK COUNTY, MA" | 
           county_name=="PLYMOUTH COUNTY, MA"| 
           county_name== "SUFFOLK COUNTY, MA"|
           county_name== "MIDDLESEX COUNTY, MA"|
           county_name== "ESSEX COUNTY, MA"|
           county_name== "ROCKINGHAM COUNTY, NH"|
           county_name== "STRAFFORD COUNTY, NH")
Boston_crimerateper100k<- sum(Boston_crimedata$crime_rate_per_100000*Boston_crimedata$population)/
  (sum(Boston_crimedata$population))
Boston_MSA_Crime<- c("Boston-Cambridge-Newton, MA-NH", Boston_crimerateper100k, sum(Boston_crimedata$MURDER), 
                     sum(Boston_crimedata$RAPE),sum(Boston_crimedata$ROBBERY))

#Salt Lake City
salt_crimedata<- crimedata2 %>% 
  filter(county_name=="SALT LAKE COUNTY, UT" | 
           county_name=="TOOELE COUNTY, UT")
salt_crimerateper100k<- sum(salt_crimedata$crime_rate_per_100000*salt_crimedata$population)/
  (sum(Boston_crimedata$population))
salt_MSA_Crime<- c("Salt Lake City, UT", salt_crimerateper100k, sum(salt_crimedata$MURDER), 
                     sum(salt_crimedata$RAPE),sum(salt_crimedata$ROBBERY))
# Cleveland
cleveland_crimedata<- crimedata2 %>% 
  filter(county_name=="LORAIN COUNTY, OH" | 
           county_name=="MEDINA COUNTY, OH"| 
           county_name== "CUYAHOGA COUNTY, OH"|
           county_name== "LAKE COUNTY, OH"|
           county_name== "GEAUGA COUNTY, OH")
cleveland_crimerateper100k<- sum(cleveland_crimedata$crime_rate_per_100000*cleveland_crimedata$population)/
  (sum(cleveland_crimedata$population))
cleveland_MSA_Crime<- c("Cleveland-Elyria, OH", cleveland_crimerateper100k, sum(cleveland_crimedata$MURDER), 
                      sum(cleveland_crimedata$RAPE),sum(cleveland_crimedata$ROBBERY))

# Baltimore
baltimore_crimedata<- crimedata2 %>% 
  filter(county_name=="ANNE ARUNDEL COUNTY, MD" | 
           county_name=="BALTIMORE COUNTY, MD"| 
           county_name== "BALTIMORE CITY, MD"|
           county_name== "CARROLL COUNTY, MD"|
           county_name== "HARFORD COUNTY, MD"|
           county_name== "HOWARD COUNTY, MD"|
           county_name== "QUEEN ANNE'S COUNTY, MD")
baltimore_crimerateper100k<- sum(baltimore_crimedata$crime_rate_per_100000*baltimore_crimedata$population)/
  (sum(baltimore_crimedata$population))
baltimore_MSA_Crime<- c("Baltimore-Columbia-Towson, MD", baltimore_crimerateper100k, sum(baltimore_crimedata$MURDER), 
                        sum(baltimore_crimedata$RAPE),sum(baltimore_crimedata$ROBBERY))

# New York
ny_crimedata<- crimedata2 %>% 
  filter(county_name=="KINGS COUNTY, NY" | 
           county_name=="QUEENS COUNTY, NY"| 
           county_name== "NEW YORK COUNTY, NY"|
           county_name== "BRONX COUNTY, NY"|
           county_name== "RICHMOND COUNTY, NY"|
           county_name== "WESTCHESTER COUNTY, NY"|
           county_name== "SUFFOLK COUNTY, NY"|
           county_name== "NASSAU COUNTY, NY"|
           county_name== "PUTNAM COUNTY, NY"|
           county_name== "DUTCHESS COUNTY, NY"|
           county_name== "BERGEN COUNTY, NJ"|
           county_name== "HUDSON COUNTY, NJ"|
           county_name== "MERCER COUNTY, NJ"|
           county_name== "MIDDLESEX COUNTY, NJ"|
           county_name== "MONMOUTH COUNTY, NJ"|
           county_name== "OCEAN COUNTY, NJ"|
           county_name== "PASSAIC COUNTY, NJ"|
           county_name== "ROCKLAND COUNTY, NY"|
           county_name== "ORANGE COUNTY, NY"|
           county_name== "ESSEX COUNTY, NJ"|
           county_name== "UNION COUNTY, NJ"|
           county_name== "MORRIS COUNTY, NJ"|
           county_name== "SOMERSET COUNTY, NJ"|
           county_name== "SUSSEX COUNTY, NJ"|
           county_name== "HUNTERDON COUNTY, NJ"|
           county_name== "PIKE COUNTY, PA")
ny_crimerateper100k<- sum(ny_crimedata$crime_rate_per_100000*ny_crimedata$population)/
  (sum(ny_crimedata$population))
ny_MSA_Crime<- c("New York-Newark-Jersey City, NY-NJ-PA", ny_crimerateper100k, sum(ny_crimedata$MURDER), 
                        sum(ny_crimedata$RAPE),sum(ny_crimedata$ROBBERY))

#Denver
denver_crimedata<- crimedata2 %>% 
  filter(county_name== "ADAMS COUNTY, CO" | 
           county_name== "ARAPAHOE COUNTY, CO"| 
           county_name== "BROOMFIELD COUNTY, CO"| 
           county_name== "CLEAR CREEK COUNTY, CO"|
           county_name== "DENVER COUNTY, CO"|
           county_name== "DOUGLAS COUNTY, CO"|
           county_name== "ELBERT COUNTY, CO"|
           county_name== "GILPIN COUNTY, CO"|
           county_name== "JEFFERSON COUNTY, CO"|
           county_name== "PARK COUNTY, CO")
denver_crimerateper100k<- sum(denver_crimedata$crime_rate_per_100000*denver_crimedata$population)/
  (sum(denver_crimedata$population))
denver_MSA_Crime<- c("Denver-Aurora-Lakewood, CO", denver_crimerateper100k, sum(denver_crimedata$MURDER), 
                       sum(denver_crimedata$RAPE),sum(denver_crimedata$ROBBERY))
# Making Data Set
msacrimedata<- as.data.frame(rbind(San_Fran_MSA_Crime, Seattle_MSA_Crime, Portland_MSA_Crime, Madison_MSA_Crime, chicago_MSA_Crime,
                                   min_MSA_Crime, philly_MSA_Crime, wash_MSA_Crime, richmond_MSA_Crime,
                                   Buffalo_MSA_Crime, Boston_MSA_Crime, salt_MSA_Crime, cleveland_MSA_Crime,
                                   baltimore_MSA_Crime, ny_MSA_Crime, denver_MSA_Crime))

names(msacrimedata)<- c("MSA", "Crime_Rate_Per_100k", "Murders", "Rapes", "Robberies")
# Would need to rbind all MSAs being used for this

text <- image_read("testimage.png") %>%
  image_resize("2000") %>%
  image_convert(colorspace = 'gray') %>%
  image_trim() %>%
  image_ocr()
text2<- gsub(" K", "%", text)
text2
pngtable<- read.table(text=text2,col.names=c("Car_Commute", "Transit_Commute", "Commute_Difference", "Population_Usage", "Income_Difference", "Index"))
str(pngtable)
pngtable$Car_Commute<- as.numeric(as.character(pngtable$Car_Commute))
pngtable$Commute_Difference<- as.character(pngtable$Commute_Difference)
pngtable$Population_Usage<- as.character(pngtable$Population_Usage)
pngtable$Income_Difference<- as.character(pngtable$Income_Difference)
pngtable$Car_Commute[2]= 30.0
pngtable$Car_Commute[5]=32.6
pngtable$Car_Commute[16]=21.8
pngtable$Car_Commute[24]=21.9
pngtable$Transit_Commute[13]=32.2
pngtable$Commute_Difference[24]="104%"

for(i in 1:25) {
  if(pngtable$Index[i]>100){
    pngtable$Index[i]=pngtable$Index[i]/100   
  }
}

metro_areas
MSA<- c("Washington-Arlington-Alexandria, DC-VA-MD-WV", "San Francisco-Oakland-Hayward, CA",
                      "Boston-Cambridge-Newton, MA-NH", "Chicago-Naperville-Elgin, IL-IN-WI", 
                      "New York-Newark-Jersey City, NY-NJ-PA", "Seattle-Tacoma-Bellevue, WA",
                      "Jersey City, NJ", "Pittsburgh, PA", "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD",
                      "Oakland, CA", "Portland-Vancouver-Hillsboro, OR-WA", "Minneapolis-St. Paul-Bloomington, MN-WI",
                      "Madison, WI", "Newark, NJ", "Denver-Aurora-Lakewood, CO", "St. Paul, MN", "Honolulu, HI", 
                      "Los Angeles-Long Beach-Santa Ana, CA", "Baltimore-Columbia-Towson, MD", "Salt Lake City, UT",
                      "Santa Ana, CA", "Richmond, VA", "Houston-Baytown-Sugar Land, TX", "Cleveland-Elyria, OH",
                      "Buffalo-Cheektowaga-Niagara Falls, NY")
pngtable2<- cbind(MSA, pngtable)

final_data_set<-left_join(datatable1, msacrimedata, by="MSA") %>% 
  left_join(pngtable2, by="MSA")

#write.csv(final_data_set, "projectdata.csv")

