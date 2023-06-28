# Libraries ----

if (!require ("tidyverse")) install.packages("tidyverse")
if (!require ("leaflet")) install.packages("leaflet")
if (!require ("forcats")) install.packages("forcats")
if (!require ("lubridate")) install.packages("lubridate")
if (!require ("hrbrthemes")) install.packages("hrbrthemes")
if (!require ("gcookbook")) install.packages("gcookbook")
if (!require ("tidytext")) install.packages("stringr")
if (!require ("tidytext")) install.packages("reshape")
if (!require ("tidytext")) install.packages("sp")


library('tidyverse')
library('leaflet')
library('lubridate')
library('forcats')
library('hrbrthemes')
library('gcookbook')
library("stringr")
library("tidytext")
library("reshape")
library("sp")


# Load in DATA ----
census <- read.csv("CensusLocalAreaProfiles2016.csv") 


c21_geocode <- read.csv("/census2021BCcomprehensive/98-401-X2021006_Geo_starting_row_BritishColumbia.csv") %>%
  filter(Geolocal %in% c("Metro Vancouver A", "Vancouver", "North Vancouver A", "North Vancouver B", "West Vancouver")) %>%
  mutate(Geo.Name = as.numeric(Geo.Name)) # We need to subset the geocode for local area boundaries from original census 2021 dataset



# Clean Data ----
census <- data.frame(t(census)) # Transpose the data set to get our variables

names(census) <- census[1,] # Make first row the variable names
census <- census[-1,] #Row, Column
rownames(census) <- NULL

# KOL ----
# Total - Knowledge of official languages for the total population excluding institutional residents - 100% data
# KOL <- census[192:206]

# KOL <- gather(key = 'TKOL', value = "N", 1:5) 

KOL_total <- data.frame(census[, 192:196], 
                        LA = census$Variables, 
                        TotalPop = as.numeric(gsub(",", "", census[,2])),
                        check.names = FALSE) %>%
  filter(!LA %in% c("Vancouver CSD ", "Vancouver CMA "))


# Differences between observed and mean

kol_tot <- KOL_total %>%
  group_by(LA) %>%
  mutate(percent = as.numeric(`  Neither English nor French`)/TotalPop) %>%
  ungroup() %>%
  mutate(diff = percent - (mean(percent)))


mean(kol_tot$percent)


kol_diff_plot <- ggplot(kol_tot, aes(fct_rev(LA), diff, fill = LA)) +
  geom_col(position = "dodge", width = 0.8, col = "black", alpha = 0.5) +
  theme_ipsum(grid = "X", axis = "X") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank()) +
  labs(title = "Do Not Speak English or French",
       subtitle = "Mean Differences by Neighborhood | Mean = 7%",
       x = NULL,
       y = "%") +
  scale_y_continuous(limits = c(-0.06, 0.1), 
                     breaks = c(-0.05, -0.1, 0, 0.05, 0.1),
                     labels = scales::percent) +
  coord_flip()


kol_diff_plot



# Immigrant Status and Period of Immigration ----
ims <- data.frame(census[, 2589:2599], 
                  LA = census$Variables, 
                  TotalPop = as.numeric(gsub(",", "", census[,2])), 
                  check.names = FALSE) 


Imm <- ims[-c(23,24),] 

# Remove CSD = Census Subdivision Area
# Includes: North Van, Surrey, etc.
# Remove CMA = Census Metropolitan Area

Imm$`    2011 to 2016` <- as.numeric(Imm$`    2011 to 2016`) # Transform from Character to Numeric


nhi <- ggplot(Imm, aes(x = LA, y = Imm$`    2011 to 2016`, alpha = 0.5, group = 1)) +
  geom_point(col = "#315B94") +
  geom_line() +
  theme_ipsum(grid="X", axis="X") +
  labs(title = "Recent Vancouver Immigrants By Local Area",
       subtitle = "Period of Immigration: 2011-2016 | 2016 Census of Population",
       y = "Total",
       x = NULL) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

nhi

# Age dist by local area ----

g_age <- data.frame(census[, c(9:17, 19:22, 24:27)], LA = census$Variables, TotalPop = ims$TotalPop, check.names = FALSE) %>%
  mutate_at(1:17, as.numeric) # Mutate multiple columns to numeric



imA <- g_age %>%
  filter(!LA %in% c("Vancouver CMA ", "Vancouver CSD ")) %>%
  gather(age, n, 1:17) %>%
  group_by(LA) %>%
  mutate(perc = round(n / sum(n), 2)) %>%
  ungroup() %>%
  mutate(age = factor(age, levels = c("    20 to 24 years", "    25 to 29 years",   "    30 to 34 years",
                                      "    35 to 39 years",   "    40 to 44 years",   "    45 to 49 years",
                                      "    50 to 54 years",   "    55 to 59 years",   "    60 to 64 years",
                                      "    65 to 69 years",   "    70 to 74 years",   "    75 to 79 years",
                                      "    80 to 84 years",   "      85 to 89 years", "      90 to 94 years",
                                      "      95 to 99 years", "      100 years and over")))

  
## Where do our population fall into?
t1 <- imA %>%
  group_by(age) %>%
  mutate(tot = sum(n)) %>%
  ungroup() %>%
  mutate(total_sum = sum(n),
         tot_perc = tot/total_sum)



t2 <- t1 %>%
  filter(age %in% c("    20 to 24 years", "    25 to 29 years",   "    30 to 34 years",
                    "    35 to 39 years",   "    40 to 44 years",   "    45 to 49 years",
                    "    50 to 54 years", "    55 to 59 years", "    60 to 64 years")) %>%
  summarise(n = sum(unique(tot_perc)))



ggplot(t1, aes(x = age, y = tot_perc)) +
  geom_col(position = "dodge", width = 0.5)

  

## What does the age breakdown look per neighborhood?
age_dist <- imA %>%
  group_by(LA) %>%
  mutate(mean = mean(perc)) %>%
  filter(!perc < mean(perc)) %>% # Remove rows where percent < mean
  arrange(desc(perc)) %>%
            slice(1:5)

ggplot(age_dist, aes(x = age, y = perc, fill = age)) +
  geom_col(position = "dodge", width = 0.8, colour = "black") +
  theme_ipsum(grid="X", axis="X") +
  theme(legend.position = "right",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Top 5 Age Groups",
       subtitle = "By Local Area",
       x = NULL,
       y = "%") +
  scale_fill_brewer() +
  facet_wrap(~ LA)



# MT Dataset ----
# Total - MT for the total population excluding institutional residents - 100% data

# Specify which strings to remove
mt_rm <- c("        Other languages, n.i.e.",                           
           "  Multiple responses", "    English and French", "    English and non-official language",                     
           "    French and non-official language", "    English, French and non-official language",
           "      Non-Aboriginal languages", "        Afro-Asiatic languages", "        Sino-Tibetan languages",
           "        Indo-European languages")

# Subset a new data frame so we don't have duplicate variables
census_mt <- data.frame(LA = census$Variables, census[224:492], check.names = FALSE)


mt <- census_mt %>%
  gather(key = "MT", value = "N", 8:270) %>% # Gather new data frame columns into one column
  select(LA, MT, N, total = `Total - Mother tongue for the total population excluding institutional residents - 100% data`,
         nOL = `    Non-official languages`,
         OL = `    Official languages`, 
         single = "  Single responses") %>%
  group_by(LA) %>% # Perform filtering & Calculations for within local area
  mutate(N = as.numeric(N),
         nOL = as.numeric(nOL)) %>% # Transform into numeric
  filter(!MT %in% mt_rm, # Remove all observations from mt_rm var
         !LA %in% c("Vancouver CSD ", "Vancouver CMA ")) %>% # Remove Van Census subdivision & Metropolitan Area
  filter(N == max(N)) %>% # Return results where highest mother tongue in local area
  mutate(percent = N/as.numeric(single))



# Break down Immigrant Population By Local Area ----


Imm$`  Immigrants` <- as.numeric(Imm$`  Immigrants`)

# Local Area Immigrant Population
laip <- Imm %>%
  select(LA, "  Immigrants"  , TotalPop) %>%
  mutate(percent = (`  Immigrants`/as.numeric(TotalPop))) %>%
  arrange(desc(percent))

laip_plot <- laip %>%
  ggplot(aes(x = LA, y = percent, alpha = 0.5)) +
  geom_col(fill = "#315B94", width = 0.7) +
  theme_ipsum(grid="X", axis="X") +
  geom_hline(yintercept=0.6, linetype="dashed", color = "black") +
  scale_y_continuous(limits = c(0, 0.6), 
                     breaks = c(0.2, 0.4, 0.6),
                     labels = scales::percent) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Vancouver's Proportion of Immigrants",
       subtitle = "Total Immigrant Population by Neighborhoods",
       x = NULL,
       y = NULL)


laip_plot

# Household Language ----
census_hh <- data.frame(LA = census$Variables, census[, 1037:1300], check.names = FALSE) 

hh_rm <- c("        Other languages, n.i.e.",                           
           "  Multiple responses", "    English and French", "    English and non-official language",                     
           "    French and non-official language", "    English, French and non-official language",
           "      Non-Aboriginal languages", "        Afro-Asiatic languages", "        Sino-Tibetan languages",
           "        Indo-European languages")


hh <- census_hh %>%
  gather(key = "hh", value = "N", 8:265) %>% # Gather new data frame columns into one column
  select(LA, hh, N, total = "    Non-official languages") %>%
  group_by(LA) %>% # Perform filtering & Calculations for within local area
  mutate(N = as.numeric(N),
         total = as.numeric(total)) %>% # Transform into numeric
  filter(!hh %in% hh_rm, # Remove all observations from hh_rm var
         !LA %in% c("Vancouver CSD ", "Vancouver CMA ")) %>% # Remove Van Census subdivision & Metropolitan Area
  filter(N == max(N)) %>% # Return results where highest hh language in local area
  mutate(percent = N/as.numeric(total))


# Proportion of Asian Immigrants to ALL Immigrants ----

#######  IPB = Immigrant Place of Birth  ##########

ipb <- data.frame(tot_imm = as.numeric(census[, 2591]), census[, 2823:2883], LA = census$Variables, TotalPop = as.numeric(gsub(",", "", census[,2])), check.names = FALSE) 
 ipb <- ipb[-c(23,24),]



#####   Subset data for Asian vs All Immigrants  #########
ava <- ipb %>%
  select(LA, Total = TotalPop,
         Americas = "  Americas" , Europe = "  Europe", Africa = "  Africa" ,  
         Asia = "  Asia" ,  Oceania = "  Oceania and other", tot_imm)

 ####  Subset more  ####
ava2 <- ava %>%
  gather(country, n, 3:7) %>%
  group_by(LA) %>%
  mutate(total_num = sum(as.numeric(n)),
         percent2 = tot_imm/Total) %>%
  ungroup() %>%
  filter(country %in% "Asia") %>%
  mutate(percent = as.numeric(n)/total_num)

 


ava2_plot <- ava2 %>%
  ggplot(aes(x = LA, y = percent, fill = fct_rev(mt$MT))) +
  geom_col(position = "dodge", width = 0.5) +
  geom_line(aes(x = LA, y = mt$percent, group = 1), size = 0.8, alpha = 0.8) +
  geom_point(aes(x = LA, y = mt$percent)) +
  theme_ipsum(grid="X", axis="x") +
  scale_fill_brewer(name = NULL) +
  scale_y_continuous(limits = c(0, 1), 
                     breaks = c(0.25, 0.5, 0.75, 1),
                     labels = scales::percent) +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "",
       y = "",
       title = "Proportion of Recent Immigrants From Asia",
       subtitle = "Asian Immigrants vs Mother Tongue | Period: 2011-2016")



ava2_plot



####   AVA main   ####
ava_m <- ava %>%
  gather(country, n, 3:7) %>%
  group_by(LA) %>%
  mutate(total_num = sum(as.numeric(n))) %>%
  ungroup() %>%
  filter(country %in% "Asia") %>%
  mutate(percent = as.numeric(n)/tot_imm)
  


tot_imm_plot <- ava_m %>%
  ggplot(aes(x = LA, y = percent, fill = fct_rev(mt$MT))) +
  geom_col(position = "dodge", width = 0.5) +
  #geom_line(aes(x = LA, y = mt$percent, group = 1), size = 0.8, alpha = 0.8) +
  #geom_point(aes(x = LA, y = mt$percent)) +
  theme_ipsum(grid="X", axis="x") +
  scale_fill_brewer(name = NULL) +
  scale_y_continuous(limits = c(0, 0.15), 
                     breaks = c(0.05, 0.1, 0.15),
                     labels = scales::percent) +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "",
       y = "",
       title = "Total Immigrants vs Asian Immigrants",
       subtitle = "Period: 2011-2016")

tot_imm_plot

max(ava_m$percent)

# Subset Immigrants from ASIA ----
# Recent Asian Immigrants by Place of Birth
ipb1 <- ipb %>% 
  select(36:58, LA, TotalPop) %>% # Select only Asian Countries
  gather(key = "PoB", value = "N", 1:23) %>%
  filter(!PoB %in% "  Asia") %>%
  group_by(LA) %>%
  mutate(sob = sum(as.numeric(N)), N = as.numeric(N)) %>% # sob = Total Sum of Asian Immigrants in Local Area
  ungroup() %>%
  group_by(LA, PoB) %>%
  mutate(percent = (N/sob)) %>%
  ungroup() %>%
  filter(percent > 0.15)

# Create Table

PoB_table <- ipb1 %>%
  mutate(percent = round(percent*100, digits = 2)) %>%
  group_by(LA) %>%
  arrange(desc(percent))


# Create Plot

prop_pob <- ggplot(ipb1, aes(x = LA, y = percent, fill = fct_rev(PoB))) +
  geom_col(position = "stack", width = 0.7, alpha = 0.8) +
  scale_fill_brewer() +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum(grid="X", axis="x") +
  theme(legend.key.size = unit(0.8,"line"),
        legend.title = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "",
       y = "",
       title = "Recent Asian Immigrants By Place of Birth",
       subtitle = "Greater than 15% of Total Local Area Asian Population")

prop_pob
  


# First Official Languages ----
# Total - First official language spoken for the total population excluding institutional residents - 100% data

FOL <- data.frame(census[, 208:212], LA = census$Variables, TotalPop = as.numeric(gsub(",", "", census[,2])), check.names = FALSE)

FOL <- census %>%
  gather(key = "FOL", value = "N", 208:222) 

FOL <- data.frame(FOL$FOL, FOL$N, FOL$Variables)

FOL <- spread(FOL, FOL.Variables, FOL.N) %>%
  arrange(desc(FOL.FOL))


# Generation Status ----

imm_gen <- data.frame(census[, 3007:3010], LA = census$Variables, TotalPop = as.numeric(gsub(",", "", census[,2])), check.names = FALSE)

# Voter Turnout Federal Elections ----

vtfe <- read.csv("t001d-eng.csv", 
                 fileEncoding = "UTF-8-BOM") %>%
  filter(str_detect(`Age.group`, pattern = "Canadian citizen by ")) %>%
  gather(key = "Year", value = "N", 2:5) %>%
  mutate(Year = str_remove(Year, pattern = "X"))

vtfe$Age.group <- str_remove(vtfe$Age.group, pattern = "Canadian citizen by ")

colnames(vtfe)[1] <- "Canadian Citizen by"


vtfe_plot <- vtfe %>%
  ggplot(aes(Year, N, group = `Canadian Citizen by`, fill = `Canadian Citizen by`)) +
  geom_col(width = 0.5, position = "dodge") +
  theme_ipsum(grid = "X", axis = "X") +
  scale_fill_brewer() +
  labs(title = "Voter Turnout in Federal Elections",
       subtitle = "Compared by Year and Immigration Status",
       x = NULL,
       y = "%")

vtfe_plot




# Limited Sample Election Data Van2022 ----

turnout <- read.csv("VanTurnout2022LA.csv", encoding = "UTF-8", header = TRUE) %>%
  dplyr::rename(Total = X.U.FEFF.Total) %>%
  mutate(Total = as.numeric(Total))

turnout_plot <- turnout %>% ggplot(aes(x = Voting_Location, y = Total, col = Neighborhood)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip()

turnout_plot


# Write csvs ----
#write.csv(laip, "laip.csv", row.names = FALSE)







