#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# EGOLM
# 2021-01-14
# Lorenz curve
# Fabian Braesemann 
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

#%#%#%#%#%#%#%#%#%#%
# Loading packages
#%#%#%#%#%#%#%#%#%#%
library(tidyverse)    # numerous data wrangling packages
library(data.table)   # quick data loadinglibrary(lubridate)
library(lubridate)    # Working with dates
library(RColorBrewer) # Fancy colours
library(ggrepel)      # Fancy labels in plot
library(scales)       # Log-transformed axis labels
library(ggpubr)       # Arranging several ggplots
library(ggrepel)
library(stargazer)    # LaTeX regression tables
'%!in%' <- function(x,y)!('%in%'(x,y)) # opposite of %in% command
options(stringsAsFactors = FALSE)
library(countrycode)

#%#%#%#%#%#%#%#%
# Load data
#%#%#%#%#%#%#%#%

olm_country <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/olm_data/OLM_Country_Data.csv")
wb_country <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/county_wb_data.csv")


wb_country <- wb_country %>% dplyr::select(-long, -country)
wb_country$year <- as.Date(as.character(wb_country$year), format = "%Y")
wb_country$year <- floor_date(wb_country$year, unit = "year")

wb_country <- wb_country %>% filter(short == "WB.POP.TOTL") %>% rename(WB.POP.TOTL = value) %>% select(-short)

#
olm_country$year <- as.Date(as.character(olm_country$year), format = "%d/%m/%Y")

olm_country <- olm_country %>% select(-country)

olm_country <- merge(olm_country, wb_country, by.x = c("ISO_Code", "year"), by.y = c("iso3", "year"))


olm_country$country <- countrycode(olm_country$ISO_Code, origin = "iso3c", destination = "country.name")
olm_country$region <- countrycode(olm_country$ISO_Code, origin = "iso3c", destination = "region")

olm_country <- olm_country %>% mutate(OLM.PRJ.CNT.PC = project_count / WB.POP.TOTL * 1000000)

olm_country_allYears <- olm_country

olm_country <- olm_country %>% filter(year == "2020-01-01") %>% arrange(region, desc(OLM.PRJ.CNT.PC))

olm_country <- olm_country %>% rename(OLM.PRJ.CNT = project_count)

olm_country$label1 <- olm_country$ISO_Code
olm_country$label2 <- olm_country$ISO_Code
olm_country$index <- as.numeric(row.names(olm_country))

for(i in 1:nrow(olm_country)){
  if(olm_country$index[i]%%2 !=0){
    olm_country$label2[i] <- NA
  }
  else{
    olm_country$label1[i] <- NA
  }
}

asian_tigers <- c("Turkmenistan", "Tajikistan", "Uzbekistan", "Kyrgyzstan", "Kazakhstan")

olm_country$region <- ifelse(olm_country$country %in% asian_tigers, 
                             "South & Central Asia", 
                             ifelse(olm_country$region == "South Asia",
                                    "South & Central Asia",
                                    ifelse(olm_country$region == "Europe & Central Asia", "Europe",olm_country$region)))


olm_country$wage_level <- ifelse(olm_country$mean_wage < 10, "< 10",
                                 ifelse(olm_country$mean_wage >= 10 & olm_country$mean_wage < 15, "< 15",
                                        ifelse(olm_country$mean_wage >= 15 & olm_country$mean_wage < 20, "< 20",
                                               ifelse(olm_country$mean_wage >= 20 & olm_country$mean_wage < 30, "< 30", "> 30"))))

olm_country$size_level <- ifelse(olm_country$OLM.PRJ.CNT < 10, "< 10",
                                 ifelse(olm_country$OLM.PRJ.CNT >= 10 & olm_country$OLM.PRJ.CNT < 100, "< 100",
                                        ifelse(olm_country$OLM.PRJ.CNT >= 100 & olm_country$OLM.PRJ.CNT < 1000, "< 1,000",
                                               ifelse(olm_country$OLM.PRJ.CNT >= 1000 & olm_country$OLM.PRJ.CNT < 10000, "< 10,000", "> 10,000"))))

olm_country$size_level <- factor(olm_country$size_level, levels = c("< 10",
                                                                    "< 100",
                                                                    "< 1,000",
                                                                    "< 10,000",
                                                                    "> 10,000"))

olm_country$special_label <- ifelse(olm_country$country %in% c("India", "Pakistan", "Bangladesh", "Philippines", "Ukraine",
                                                               "Kenya", "United States", "Russia",
                                                               "Serbia", "South Africa", "Canada", "United Kingdom",
                                                               "Australia", "Tunisia", "Egypt","Brazil", "Argentina"),
                                    olm_country$country, NA)

olm_country$region <- ifelse(olm_country$region == "Middle East & North Africa", "MENA", olm_country$region)

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# FIGURE 1 B
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

olm_country %>% filter(year == "2020-01-01", OLM.PRJ.CNT > 0, !is.na(mean_wage)) %>%
  ggplot(aes(x = reorder(region,OLM.PRJ.CNT.PC), y = OLM.PRJ.CNT.PC, 
             size = OLM.PRJ.CNT, fill = wage_level)) +
  geom_boxplot(aes(group = region),fill = "grey", alpha = 0, width = 0.3, show.legend = F) +
  geom_point(shape = 21, alpha = 0.8, stroke = 0.2, colour = "black", position= position_jitter(0.15,0.2, seed = 1)) + 
  geom_text_repel(aes(label = special_label), size = 4, position= position_jitter(0.15,0.2, seed = 1)) + 
    scale_fill_manual(values = c(brewer.pal(9,"Blues")[1],brewer.pal(9,"Blues")[3],
                                brewer.pal(9,"Blues")[5],brewer.pal(9,"Blues")[7],
                                brewer.pal(9,"Blues")[9])) +
  scale_size_continuous(breaks = c(1,10,100,1000,10000), range = c(1,12)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "lr", colour = "grey") +
  labs(x = "", y = "OLM project count per 1 mio population",
       size = "OLM project count", fill = "Avg. hourly wage ($)") +
  theme_bw()+
  theme(
        legend.position = "right", legend.direction = "vertical",
        legend.background = element_blank(), text = element_text(size = 17),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, size = 15)) + 
  guides(fill = guide_legend(override.aes = list(size=5)))

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

# Fig 1 C

OECD_stats <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/region_oecd_data.csv")

OECD_stats$iso2 <- sapply(OECD_stats$region_id, function(x) substr(x,1,2)[[1]][1])

OECD_stats$iso2 <- ifelse(OECD_stats$iso2 == "EL", "GR", 
                        ifelse(OECD_stats$iso2 == "UK", "GB", OECD_stats$iso2))

OECD_stats$iso3 <- countrycode(OECD_stats$iso2, origin = "iso2c", destination = "iso3c")

OECD_stats$year <- as.Date(as.character(OECD_stats$year), format = "%Y")
OECD_stats$year <- floor_date(OECD_stats$year, unit = "year")


OECD_capital <- OECD_stats %>% filter(long == "Region holds country capital", year == as.Date("2020-01-01")) %>% dplyr::select(region_id, capital = value)

OECD_stats2 <- merge(OECD_stats, OECD_capital, by = "region_id")

OECD_stats_allYears <- OECD_stats2 %>% filter(long == "Population count") %>% dplyr::select(year, iso3, region_id, region, OLM.PRJ.CNT, OLM.WG.MN, OCD.PPL.CNT = value, capital)

OECD_stats <- OECD_stats %>% filter(long == "Population count", year == as.Date("2020-01-01")) %>% dplyr::select(iso3, region_id, region, OLM.PRJ.CNT, OLM.WG.MN, OCD.PPL.CNT = value)

OECD_stats <- merge(OECD_stats, OECD_capital, by = "region_id")


delete_regions <- c("Chungcheongnam-do", "Federal City of Saint Petersburg")

OECD_stats <- OECD_stats %>% filter(region %!in% delete_regions)

OECD_stats <- OECD_stats %>% distinct()

OECD_stats$GN <- "OECD+ countries"

OECD_stats <- OECD_stats %>% dplyr::select(GN, iso3, region_id, region, project_count = OLM.PRJ.CNT, mean_wage = OLM.WG.MN, population = OCD.PPL.CNT, capital)

OECD_stats <- OECD_stats %>% mutate(OLM.PRJ.CNT.PC = project_count / population * 1000000)

OECD_stats <- OECD_stats %>% filter(iso3 %!in% c("TUN", "PER"))


# 
olm_GDL <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/olm_data/OLM_GDL_Data.csv")
olm_GDL$year <- as.Date(olm_GDL$year, format = "%d/%m/%Y")
olm_GDL <- olm_GDL %>% filter(Country %!in% c("Brazil", "Costa Rica", "China", "India", "Indonesia", "Mexico", "South Africa", "Colombia"))

                              
#####
olm_GDL <- olm_GDL %>% mutate(MEX = str_detect(gdlCode, "MEX")) %>% filter(MEX == 0) %>% dplyr::select(-MEX)

#####

olm_GDL_allYears <- olm_GDL

olm_GDL <- olm_GDL %>% filter(year == as.Date("2020-01-01")) %>% dplyr::select(region_id = gdlCode, mean_wage, region = Region, project_count)

GDL_stats <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/region_gdl_data.csv")

GDL_stats$year <- as.Date(as.character(GDL_stats$year), format = "%Y")
GDL_stats$year <- floor_date(GDL_stats$year, unit = "year")

GDL_stats <- GDL_stats %>% filter(year == as.Date("2020-01-01"), short %in% c("GDL.PPL.CNT", "GDL.CPT.YES")) %>%
  dplyr::select(region_id, iso3, short, value)

GDL_stats <- GDL_stats %>% filter(iso3 %!in% c("BRA", "CRI", "CHN", "IND", "IDN", "MEX", "ZAF", "COL"))

GDL_stats <- GDL_stats %>% spread(short, value)

GDL_stats <- merge(olm_GDL, GDL_stats, by = c("region_id"))

GDL_stats$GN <- "Global South"

GDL_stats <- GDL_stats %>% dplyr::select(GN, iso3, region_id, region, project_count, mean_wage, population = GDL.PPL.CNT, capital = GDL.CPT.YES)

GDL_stats <- GDL_stats %>% mutate(OLM.PRJ.CNT.PC = project_count / population)

regions <- rbind(OECD_stats, GDL_stats)

regions$GN <- factor(regions$GN, levels = c("OECD+ countries", "Global South"))

regions$capital <- ifelse(regions$capital == 1, "Capital", "Other region")

#%#%
# Calculate the ratio between capital and other regions
r2 <- regions %>% group_by(GN, iso3, capital) %>% filter(region_id %!in% c("MNEr101", "MNEr102", "MNEr103")) %>% summarise(OLM.PRJ.CNT.PC = mean(OLM.PRJ.CNT.PC))

r2 <- r2 %>% spread(capital, OLM.PRJ.CNT.PC)

r2 %>% filter(Capital > 0, `Other region` > 0 ) %>% mutate(Ratio = Capital / `Other region`) %>% group_by(GN) %>% summarise(Ratio_mean = mean(Ratio, na.rm =T))

r3 <- regions %>% group_by(GN, iso3, capital) %>% filter(region_id %!in% c("MNEr101", "MNEr102", "MNEr103"))%>% summarise(mean_wage = mean(mean_wage))

r3 <- r3 %>% spread(capital, mean_wage)

r3 %>% filter(Capital > 0, `Other region` > 0 ) %>% mutate(Ratio = Capital / `Other region`) %>% group_by(GN) %>% summarise(Ratio_mean = mean(Ratio, na.rm =T))

#%#%

regions$wage_level <- ifelse(regions$mean_wage < 10, "< 10",
                                 ifelse(regions$mean_wage >= 10 & regions$mean_wage < 15, "< 15",
                                        ifelse(regions$mean_wage >= 15 & regions$mean_wage < 20, "< 20",
                                               ifelse(regions$mean_wage >= 20 & regions$mean_wage < 30, "< 30",
                                                      ifelse(regions$mean_wage >=30, "> 30", "No wage data")))))

regions$wage_level <- ifelse(is.na(regions$wage_level), "No wage data", regions$wage_level)


regions$wage_level <- factor(regions$wage_level, levels = c("No wage data",
                                                                    "< 10",
                                                                    "< 15",
                                                                    "< 20", "< 30",
                                                                    "> 30"))


regions$size_level <- ifelse(regions$project_count < 10, "< 10",
                                 ifelse(regions$project_count >= 10 & regions$project_count < 100, "< 100",
                                        ifelse(regions$project_count >= 100 & regions$project_count < 1000, "< 1,000",
                                               ifelse(regions$project_count >= 1000 & regions$project_count < 10000, "< 10,000", "> 10,000"))))

olm_country$size_level <- factor(olm_country$size_level, levels = c("< 10",
                                                                    "< 100",
                                                                    "< 1,000",
                                                                    "< 10,000",
                                                                    "> 10,000"))


regions$ratio <- ifelse(regions$region_id == "NZ11", "Avg. capital to region ratio: \nProjects per capita = 3.27\nHourly wage = 1.24", 
                        ifelse(regions$region_id == "BLZr101", "Avg. capital to region ratio: \nProjects per capita = 15\nHourly wage = 1.53", NA))

regions$iso3 <- ifelse(regions$iso3 == "MNE", "MEX", regions$iso3)

regions %>% filter(OLM.PRJ.CNT.PC > 0, project_count >0, region_id %!in% c("MNEr101", "MNEr102", "MNEr103")) %>%
  ggplot(aes(x = reorder(iso3, OLM.PRJ.CNT.PC), y = OLM.PRJ.CNT.PC, fill = wage_level, #shape = factor(capital),
             size = project_count)) +
  geom_point(position = position_jitter(0.25, seed = 1), shape = 21, alpha = 0.7) +
  #scale_shape_manual(values = c(24,21)) +
  scale_colour_manual(values = c(brewer.pal(11,"RdBu")[2], brewer.pal(11,"RdBu")[10])) +
  scale_size_continuous(breaks = c(1,10,100,1000,10000), range = c(1.5,10)) +
  scale_fill_manual(values = c("white",brewer.pal(9,"Blues")[1],brewer.pal(9,"Blues")[3],
                               brewer.pal(9,"Blues")[5],brewer.pal(9,"Blues")[7],
                               brewer.pal(9,"Blues")[9])) +
  geom_point(position = position_jitter(0.25, seed = 1), aes(alpha = factor(capital), colour = factor(capital)), shape = 16) +
  scale_alpha_manual(values = c(0.8,0)) +
  facet_wrap(~GN, scales = "free_x", ncol = 1) + 
  geom_label(aes(y = 0.19, label = ratio), size = 4.5, fill = NA) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "lr", colour = "grey") +
  labs(x = "", y = "OLM project count per 1 million population",alpha = "Capital region", colour = "Capital region", fill = "Avg. hourly wage ($)", size = "OLM project count") +
  theme_bw() +
  theme(legend.position = "right", legend.direction ="vertical",
        axis.text.x = element_text(angle = 90, size = 10),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), text = element_text(size = 16)) +
  guides(fill = guide_legend(override.aes = list(size=5)))

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
