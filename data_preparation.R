library(dplyr)
library(data.table)
library(R.utils)
library(lubridate)

if (!file.exists("./data/storm_data.RData")) {
    storm_data <- load_data()
    save(storm_data, file="./data/storm_data.RData")
} else {
    load(file="./data/storm_data.RData")
}

if (!file.exists("./data/storm_data_clean.RData")) {
    storm_data_clean <- clean_data(storm_data)
    
    storm_data_clean$BGN_DATE_YR <- 
        as.integer(year(as.Date(storm_data_clean$BGN_DATE, 
                                format="%m/%d/%Y %H:%M:%S")))
    
    storm_data_clean <- getDataRange(storm_data_clean, 2001, 2011)
    save(storm_data_clean, file="./data/storm_data_clean.RData")
} else {
    load(file="./data/storm_data_clean.RData")
}

load_data <- function(){
    ## Define the input source file
    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    
    if (!file.exists("./data")) dir.create("./data")
    
    targetURL <- "./data/repdata_data_StormData.csv.bz2"
    
    ## Download the file from the URL provided in the project brief
    if (!file.exists(gsub("[.]bz2$", "", targetURL))) {
        setInternet2(TRUE)
        download.file(fileURL, destfile = targetURL)
        bunzip2(targetURL)
    }
    targetURL <- gsub("[.]bz2$", "", targetURL)
    
    storm_data <- read.csv(targetURL, header = TRUE, 
                           stringsAsFactor = FALSE, na.strings = "?")
    
    storm_data
}

clean_data <- function(storm_data) {
    ## Remove the NA EVTYPE from storm_data
    storm_data <- storm_data[!is.na(storm_data$EVTYPE), ]
    
    ## Remove records that are not relevant to our questions 
    storm_data <- 
        filter(storm_data, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0.0 | CROPDMG > 0.0)
    
    ## Adjust the Multiplier for Property and Crop damages
    ### Property Adjustments
    storm_data$prop_multi[storm_data$PROPDMGEXP ==""] <- 1;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="-"] <- 1;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="?"] <- 1;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="+"] <- 1;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="0"] <- 1;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="1"] <- 1;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="2"] <- 100;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="3"] <- 1000;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="4"] <- 10000;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="5"] <- 100000;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="6"] <- 1000000;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="7"] <- 10000000;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="8"] <- 100000000;
    
    storm_data$prop_multi[storm_data$PROPDMGEXP =="h"] <- 100;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="H"] <- 100;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="k"] <- 1000;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="K"] <- 1000;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="m"] <- 1000000;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="M"] <- 1000000;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="b"] <- 1000000000;
    storm_data$prop_multi[storm_data$PROPDMGEXP =="B"] <- 1000000000;
    
    ### Crop Adjustments
    storm_data$crop_multi[storm_data$CROPDMGEXP ==""] <- 1;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="-"] <- 1;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="?"] <- 1;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="+"] <- 1;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="0"] <- 1;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="1"] <- 1;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="2"] <- 100;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="3"] <- 1000;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="4"] <- 10000;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="5"] <- 100000;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="6"] <- 1000000;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="7"] <- 10000000;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="8"] <- 100000000;
    
    storm_data$crop_multi[storm_data$CROPDMGEXP =="h"] <- 100;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="H"] <- 100;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="k"] <- 1000;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="K"] <- 1000;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="m"] <- 1000000;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="M"] <- 1000000;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="b"] <- 1000000000;
    storm_data$crop_multi[storm_data$CROPDMGEXP =="B"] <- 1000000000;
    
    storm_data$PROPDMG <- storm_data$PROPDMG * storm_data$prop_multi
    storm_data$CROPDMG <- storm_data$CROPDMG * storm_data$crop_multi
    
    ## Clean the Even types.  Visual exploration had shown that it has issues such as
    ## leading and trailing spaces, same event but using different "codes", etc.
    # Change EVTYPE to upper case and triming of leading and trailing spaces
    storm_data$EVTYPE <- tolower(trim(storm_data$EVTYPE))
    
    ## Create a data.table that will be used for detailed cleaning later
    storm_evtypes  <- data.table(evtype = storm_data$EVTYPE,    # The curent EVTYPE for search later  
                                 result = storm_data$EVTYPE)    # As the value to replace EVTYPE with
    
    ## Make copy of storm data into a "clean" data table
    storm_data_clean <- storm_data
    
    ## Codify the 48 NWS's Events
    codebook <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood",
                  "Cold/Wind Chill","Debris Flow", "Dense Fog","Dense Smoke",
                  "Drought","Dust Devil","Dust Storm","Excessive Heat",
                  "Extreme Cold/Wind Chill", "Flash Flood", "Flood", "Frost/Freeze",
                  "Funnel Cloud", "Freezing Fog", "Hail", "Heat", "Heavy Rain", 
                  "Heavy Snow", "High Surf", "High Wind", "Hurricane (Typhoon)",
                  "Ice Storm", "Lake-Effect Snow", "Lakeshore Flood", "Lightning",
                  "Marine Hail","Marine High Wind", "Marine Strong Wind",
                  "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet",
                  "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind",
                  "Tornado",  "Tropical Depression", "Tropical Storm", "Tsunami",
                  "Volcanic Ash", "Waterspout", "Wildfire",  "Winter Storm", 
                  "Winter Weather")
    
    # Define the regular expression to search the original EVTYPES so as to define the actual code 
    # value to be used
    reg_expr = c("^astro.*de$",
                 "^avalanch?e",
                 "^blizz.*?d|ground blizzard", 
                 "coastal *?flood.*|cstl flood|coastal *?erosion|beach erosion|tidal flooding",
                 "^(cold).*|(^extreme ) wind ?chill|^extended cold|^wind|cool and wet|low temperature|unseasonable?y? cold",
                 "^debri?s.*|landslides?|landslump|landspout|[(mud)|(rock)] ?slides?",
                 "(dense)? ?fog$",
                 "^den?s?e? smo?k?e",
                 "^droug?h?t",
                 "^dust devi?l?",
                 "^dust ?stor?m?|blowing dust",
                 "^excessive ?heat|extreme ?heat|record heat|record.excessive heat",
                 "^extreme ?cold|^extreme wind ?chill|hypo?e?r?thermia|record cold",
                 "^flash ?[flood]?.*|flash.?flood|dam break|flood.flash",
                 "^floods?$|^flooding|^breakup flooding|flood \\& heavy rain|flood.rain.winds|major flood|^flood.*flood$|^minor*|rapidly rising water|^river.*flood.*|rural flood|small stream flood|urban.*", 
                 "frost|freez[e|ing]|black ice|glaze.*|^ice$|^ice and.*|^ice flo.*|^ice jam.*|^ice?y?.*road.*|^ice.strong.*",
                 "funnel ?cloud", "(freezing)? ?fog.*?(cold).*?",
                 "^hail|falling snow.ice|small hail", 
                 "^heat|unseasonably warm.*|warm weather", 
                 "^he?a?vy rain|excessive rainfall|excessive wetness|heavy mix|heavy precipitation|heavy shower|mixed precip(itation)?|^rain.*|record rainfall|unseasonal rain|torrential.*",
                 "heavy snow|excessive snow|record snow|snow.*",
                 "^high surf|hazardous surf",
                 "^high wind$|^high .?winds$|^high wind.?.heav|^high winds.$|^high winds.[(cold)|(snow)]|^high winds?..[g8al]|^high$",
                 "hurricane|typhoon",
                 "ice storm",
                 "lake snow|lake effect",
                 "lake flood",
                 "lightn?ing|ligntning",
                 "marine hail",
                 "^high wind and seas|^high winds?.seas|heavy surf.*",
                 "marine strong wind",
                 "^marine thunderstorm wind|coastal ?storm|heavy seas|marine accident|marine mishap|rogue wave|rough seas|rough surf",
                 "^rip.*",
                 "seiche", 
                 "^sleet|snow.sleet$",
                 "^storm surge|tide|swells|high waves|high water|high seas|coastal surge",
                 "^strong wind|downburst|microburst|dry mircoburst( winds)?|gradient wind|gustnado|gusty wind|non-severe wind damage|severe turbulence",
                 "^thun?d?ee?re?s?tr?om?rm ? ?(wind)?|thunderstrom wind|thundersnow|^severe thunder.*|tstm|tunder.*|storm force winds",
                 "tornado|torndao|whirlwind", 
                 "tropical depression", 
                 "tropical storm.*",
                 "tsunami",
                 "volcanic",
                 "waterspout", 
                 "wildfire|forest fire|wild fires|brush fire|grass fires",
                 "blowing snow",
                 "winter weather|^winter|snow.winter|wintry mix|late season snow|light snow.*")
    
    ref_evtype <- data.table(codebook, reg_expr)
    
    storm_evtypes <- arrange(storm_evtypes, evtype)
    
    evtype <- storm_evtypes$evtype 
    
    for (i in 1:nrow(ref_evtype)) {
        if(sum(grepl(ref_evtype[i, ]$reg_expr, storm_evtypes$evtype)) > 0) {
            storm_evtypes$result <- 
                replace(storm_evtypes$result, 
                        grepl(ref_evtype[i, ]$reg_expr, storm_evtypes$evtype),
                        ref_evtype[i, ]$codebook)
        }
    }
    
    for (i in 1:nrow(storm_evtypes)) {
        storm_data_clean$EVTYPE <- 
            replace(storm_data_clean$EVTYPE, 
                    !is.na(storm_data_clean$EVTYPE) &
                        storm_data_clean$EVTYPE == storm_evtypes[i, ]$evtype,
                    storm_evtypes[i, ]$result) 
    }
    
    storm_data_clean
}

getDataRange <- function(dt, startYear, endYear) {
    result <- dt %>% filter(BGN_DATE_YR >= startYear, 
                            BGN_DATE_YR <= endYear)
    
    result
}
