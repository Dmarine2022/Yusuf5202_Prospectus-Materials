#Load libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(car)

#2012 DATA
#Load in rawdata from Github ##remember to use that raw link (this appears to create a one time token, that have to be repeaat everytime######
##NLA22_waterchem data
WaterChem2022 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/Yusuf5202_Prospectus-Materials/refs/heads/main/NLA2022_dataset/nla22_waterchem_wide.csv')

##NLA22_Toxin data
toxin2022 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/Yusuf5202_Prospectus-Materials/refs/heads/main/NLA2022_dataset/nla22_algaltoxins.csv')

##NLA22_Secchi data
secchi2022 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/Yusuf5202_Prospectus-Materials/refs/heads/main/NLA2022_dataset/nla22_secchi.csv')

##NLA22_landscape data
landscape2022 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/Yusuf5202_Prospectus-Materials/refs/heads/main/NLA2022_dataset/nla2022_landscape_wide_0.csv')

##NLA22_profile data
profile2022 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/Yusuf5202_Prospectus-Materials/refs/heads/main/NLA2022_dataset/nla2022_profile_wide.csv')


##NLA22_siteinfo data
siteinfo2022 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/Yusuf5202_Prospectus-Materials/refs/heads/main/NLA2022_dataset/nla22_siteinfo.csv')

#NLA22_Phytoplankton data
phytoplanktoncount2022_data <- read.csv("https://raw.githubusercontent.com/Dmarine2022/Yusuf5202_Prospectus-Materials/refs/heads/main/NLA2022_dataset/nla2022_phytoplanktoncount_wide.csv")  

#####
#Toxin2022 Pivot wide
toxin2022_wide <- toxin2022 %>% 
  pivot_wider(
    names_from = ANALYTE,
    values_from = RESULT
  )

#sECCHI CALCULATION (Average)#####
secchi2022_cal <- secchi2022 %>% 
  mutate(Secchi = (DISAPPEARS + REAPPEARS)/2)


############################################################################################################
#Select specific columns i.e. relevant columns for my work####
#WaterChem
#2022
WaterChem2022_subset <- WaterChem2022 %>%
  select(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO, AMMONIA_N_RESULT, ANC_RESULT, CALCIUM_RESULT, CHLA_RESULT,
         CHLORIDE_RESULT, COLOR_RESULT, COND_RESULT, DOC_RESULT, MAGNESIUM_RESULT, NITRATE_N_RESULT, 
         NITRATE_NITRITE_N_RESULT, NITRITE_N_RESULT, NTL_DISS_RESULT, NTL_RESULT, PH_RESULT, POTASSIUM_RESULT,
         PTL_DISS_RESULT, PTL_RESULT, SODIUM_RESULT, SULFATE_RESULT, TURB_RESULT
  )

#Select Toxin
#2022
names(toxin2022_wide)
toxin2022_subset <- toxin2022_wide %>% 
  select(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO, MICX, CYLSPER)


# Select SECCHI
#2022
names(secchi2022_cal)

secchi2022_sebset <- secchi2022_cal %>% 
  select(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO, Secchi)


# Select landscape (elevation)
#2022 select for Elevation

landscape2022_sebset <- landscape2022 %>% 
  select(UNIQUE_ID, SITE_ID, ELEV, ELEV_MAX, ELEV_MIN) #Note : no DATE_COL and VISIT_NO in landscape Data

# select profile
#1-depth-averaged values across all depths
mean_profiles_alldepths <- profile2022 %>%
  group_by(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_mean = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_mean = mean(OXYGEN, na.rm = TRUE),
    pH_mean = mean(PH, na.rm = TRUE),
    Conductivity_mean = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#2-calculate mean of temp, DO, pH, conductivity at the top 1m
mean_profiles_top1m <- profile2022 %>%
  filter(DEPTH <= 1) %>%
  group_by(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_top1m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_top1m = mean(OXYGEN, na.rm = TRUE),
    pH_top1m = mean(PH, na.rm = TRUE),
    Conductivity_top1m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#3-compute the mean between 1 and 2 meters
mean_profiles_1to2m <- profile2022 %>%
  filter(DEPTH >= 1, DEPTH <= 2) %>%
  group_by(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_1to2m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_1to2m = mean(OXYGEN, na.rm = TRUE),
    pH_1to2m = mean(PH, na.rm = TRUE),
    Conductivity_1to2m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )
#4- compute the mean between 2 and 4 meters
mean_profiles_2to4m <- profile2022 %>%
  filter(DEPTH >= 2, DEPTH <= 4) %>%
  group_by(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_1to2m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_1to2m = mean(OXYGEN, na.rm = TRUE),
    pH_1to2m = mean(PH, na.rm = TRUE),
    Conductivity_1to2m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )
#5- compute the mean below 4 meters
mean_profiles_below4m <- profile2022 %>%
  filter(DEPTH >= 4) %>%
  group_by(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_below4m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_below4m = mean(OXYGEN, na.rm = TRUE),
    pH_below4m = mean(PH, na.rm = TRUE),
    Conductivity_below4m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )
#6- compute the mean below 5 meters
mean_profiles_below5m <- profile2022 %>%
  filter(DEPTH >= 5) %>%
  group_by(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_below5m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_below5m = mean(OXYGEN, na.rm = TRUE),
    pH_below5m = mean(PH, na.rm = TRUE),
    Conductivity_below5m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )


# select siteinfo
#2022

siteinfo2022_subset <- siteinfo2022 %>% 
  select(UNIQUE_ID, SITE_ID, AREA_HA, ELEVATION,LAKE_ORGN, LAT_DD83, LON_DD83, INDEX_SITE_DEPTH) #note: we have elevation in landscape data too



# Select and calculation on phytoplankton data

# We define PTOX taxa based on Chapman & Foss (2020); Chorus & Welker (2021).
ptox_taxa <- c("ANABAENOPSIS", "ANABAENA", "APHANIZOMENON", "APHANOCAPSA", "ARTHROSPIRA", "CHRYSOSPORUM", "CUSPIDOTHRIX",
               "RAPHIDIOPSIS", "CYLINDROSPERMOPSIS", "DESMONOSTOC",  "DOLICHOSPERMUM", "FISCHERELLA", "GEITLERINEMA", 
               "GLOEOTRICHIA", "HAPALOSIPHON", "LEPTOLYNGBYA", "PLECTONEMA", "LIMNOTHRIX", "MERISMOPEDIA", "MICROCOLEUS",
               "PHORMIDIUM", "MICROCYSTIS", "MICROSEIRA", "LYNGBYA", "NOSTOC", "OSCILLATORIA", "PLANKTOTHRIX", "PSEUDANABAENA",
               "RADIOCYSTIS", "RIVULARIA", "ROMERIA", "SCYTONEMA", "SNOWELLA", "SPHAEROSPERMOPSIS", "STENOMITOS", "SYNECHOCOCCUS",
               "SYNECHOCYSTIS", "TOLYPOTHRIX", "TRICHODESMIUM", "TRICHORMUS", "UMEZAKIA", "WORONICHINIA")


phyto2022_summary <- phytoplanktoncount2022_data %>%
  group_by(SITE_ID, DATE_COL) %>%
  summarise(
    # Total biovolume calculations
    total_phytoplankton_biovolume = sum(BIOVOLUME, na.rm = TRUE),
    total_cyanobacteria_biovolume = sum(BIOVOLUME[ALGAL_GROUP == "CYANOBACTERIA"], na.rm = TRUE),
    
    # Total density calculations
    total_phytoplankton_density = sum(DENSITY, na.rm = TRUE),
    total_cyanobacteria_density = sum(DENSITY[ALGAL_GROUP == "CYANOBACTERIA"], na.rm = TRUE),
    
    # Total abundance calculations
    total_phytoplankton_abundance = sum(ABUNDANCE, na.rm = TRUE),
    total_cyanobacteria_abundance = sum(ABUNDANCE[ALGAL_GROUP == "CYANOBACTERIA"], na.rm = TRUE),
    
    # PTOX Biovolume Calculation: Which is the Sum of biovolume where "TARGET_TAXON" matches "PTOX taxa"
    ##To ensure that any species containing the name "Anabaena" for example (including "Anabaena oscillarioides" etc) is captured, we modify the code to use pattern matching with grepl()
    ##grepl(pattern, TARGET_TAXON, ignore.case = TRUE)
    ##Checks if each TARGET_TAXON contains any word from ptox_taxa.
    ##Example: "Anabaena oscillarioides" matches "Anabaena".
    ##BIOVOLUME[grepl(...)]
    ##Filters BIOVOLUME only where the taxon contains a PTOX keyword.
    PTOX_biovolume = sum(BIOVOLUME[grepl(paste(ptox_taxa, collapse = "|"), TARGET_TAXON, ignore.case = TRUE)], na.rm = TRUE)   #selects only the BIOVOLUME values where TARGET_TAXON matches a taxon in ptox_taxa.
  ) %>%
  mutate(
    percent_cyanobacteria_biovolume = (total_cyanobacteria_biovolume / total_phytoplankton_biovolume) * 100,
    percent_cyanobacteria_density = (total_cyanobacteria_density / total_phytoplankton_density) * 100,
    percent_cyanobacteria_abundance = (total_cyanobacteria_abundance / total_phytoplankton_abundance) * 100,
    percent_PTOX_biovolume = (PTOX_biovolume / total_cyanobacteria_biovolume) * 100  # % PTOX biovolume relative to total_cyanobacteria_biovolume
  )


# View
print(phyto2022_summary)





#View the first few rows
#2022
head(WaterChem2022_subset)
head(toxin2022_subset)
head(secchi2022_sebset)
head(landscape2022_sebset)
#head profiles
head(mean_profiles_alldepths)
head(mean_profiles_top1m) #Note: I will likely be using this top1m in the combined dataset
head(mean_profiles_1to2m)
head(mean_profiles_2to4m)
head(mean_profiles_below4m)
head(mean_profiles_below5m)
##siteinfo2022
head(siteinfo2022_subset)
#Phyto
head(phyto2022_summary)





#count missing values (NAs)
colSums(is.na(WaterChem2022_subset))
colSums(is.na(toxin2022_subset))
colSums(is.na(secchi2022_sebset))
colSums(is.na(landscape2022_sebset))
#profiles
colSums(is.na(mean_profiles_alldepths))
colSums(is.na(mean_profiles_top1m))
colSums(is.na(mean_profiles_1to2m))
colSums(is.na(mean_profiles_2to4m))
colSums(is.na(mean_profiles_below4m))
colSums(is.na(mean_profiles_below5m))
#siteinfo
colSums(is.na(siteinfo2022_subset)) #note NAs in LAKE_ORGN and INDEX_SITE_DEPTH 
#phyto
colSums(is.na(phyto2022_summary)) #note: NAs retured for percent ptox is due to 0s





###TO DEAL WITH BDL (non detect-ND),we use half detection limit#######
##Toxin MDL MICX = 0.1 ug/L; CYLSPER = 0.05 ug/L
#2022
toxin2022_DL <- toxin2022_subset %>% 
  mutate(MICX = ifelse(is.na(MICX), 0.1 / 2, MICX),
         CYLSPER = ifelse(is.na(CYLSPER), 0.05 / 2, CYLSPER)
  )
head(toxin2022_DL) #CHECK HEAD
colSums(is.na(toxin2022_DL)) #cHECK NA count again


###JOIN SEBSET DATA#####
##To combine the datasets####
#2022
combined_data <- left_join(WaterChem2022_subset, toxin2022_DL, 
                           by = c("UNIQUE_ID", "SITE_ID", "DATE_COL", "VISIT_NO"))
#join secchi
combined_data2 <- left_join(combined_data, secchi2022_sebset,
                            by = c("UNIQUE_ID", "SITE_ID", "DATE_COL", "VISIT_NO"))
#join landscape
combined_data3 <- left_join(combined_data2, landscape2022_sebset,
                            by = c("UNIQUE_ID", "SITE_ID"))
#Join mean_profiles_top1m
combined_data4 <- left_join(combined_data3, mean_profiles_top1m,       #Note: Consider joining the "mean_profiles_alldepths" later
                            by = c("UNIQUE_ID", "SITE_ID", "DATE_COL", "VISIT_NO"))

#join site information
combined_data5 <- left_join(combined_data4, siteinfo2022_subset,
                            by = c("UNIQUE_ID", "SITE_ID"))

combined_data5b <- left_join(combined_data4, siteinfo2022_subset, 
                             by = c("UNIQUE_ID", "SITE_ID"),
                             relationship = "many-to-many") #note

View(combined_data5b)

#Join phyto data

combined_data6 <- left_join(combined_data5b, phyto2022_summary, 
                            by = c("SITE_ID", "DATE_COL"),
                            relationship = "many-to-many") #note
names(combined_data6)


################################################################################################
install.packages("randomForest")
library(randomForest)


vars_needed <- c("MICX", "CHLA_RESULT", "total_phytoplankton_biovolume", 
                 "total_cyanobacteria_biovolume", "PTOX_biovolume")

RF_data <- combined_data6[complete.cases(combined_data6[, vars_needed]), ]


# Fit the random forest regression model
set.seed(123)  # for reproducibility
rf_model <- randomForest(
  MICX ~ CHLA_RESULT + total_phytoplankton_biovolume + total_cyanobacteria_biovolume + PTOX_biovolume,
  data = RF_data ,
  importance = TRUE,
  ntree = 500
)

# Print the model summary
print(rf_model)

# Get variable importance
importance(rf_model)

#########
plot(rf_model$predicted, RF_data$MICX,
     xlab = "Predicted", ylab = "Observed MICX",
     main = "Random Forest Predicted vs Observed")
abline(0, 1, col = "red")

####################################################################################################
RF_data$MICX_class <- ifelse(RF_data$MICX <= 0.05, "NonDetect", "Detect")
RF_data$MICX_class <- as.factor(RF_data$MICX_class)

rf_class <- randomForest(
  MICX_class ~ CHLA_RESULT + total_phytoplankton_biovolume + total_cyanobacteria_biovolume + PTOX_biovolume,
  data = RF_data,
  importance = TRUE
)
print(rf_class)
varImpPlot(rf_class)
importance(rf_class)
#################################################################################################
p1A <- combined_data6 %>%
  ggplot(aes(x = CHLA_RESULT, y = MICX)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 2, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Chlorophyll-a",
    y = "Microcystins (MIC; µg/L)",
    title = "Relationship Between Chlorophyll-a and Microcystins"
  )


p2B <- combined_data6 %>%
  ggplot(aes(x = total_phytoplankton_density, y = MICX)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_log10() +  
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 2, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Total_phytoplankton_density",
    y = "Microcystins (MIC; µg/L)",
    title = "Relationship Between Total_phytoplankton_density and Microcystins"
  )



p3C <- combined_data6 %>%
  ggplot(aes(x = total_cyanobacteria_density, y = MICX)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  scale_x_log10() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 0.5, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Total_cyanobacteria_density",
    y = "Microcystins (MIC; µg/L)",
    title = "Relationship Between Total_cyanobacteria_density and Microcystins"
  )



p4D <- combined_data6 %>%
  ggplot(aes(x = PTOX_biovolume, y = MICX)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  scale_x_log10() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "PTOX_biovolume",
    y = "Microcystins (MIC; µg/L)",
    title = "Relationship Between PTOX_biovolume and Microcystins"
  )

# Arrange plots together
ggarrange(p1A, p2B, p3C, p4D, ncol = 2, nrow = 2)
#####################################################################################################
S1A <- combined_data6 %>%
  ggplot(aes(x = CHLA_RESULT, y = CYLSPER)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 2, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Chlorophyll-a",
    y = "Cylindrospermopsins (CYL; µg/L)",
    title = "Relationship Between Chlorophyll-a and Cylindrospermopsins"
  )


S2B <- combined_data6 %>%
  ggplot(aes(x = total_phytoplankton_density, y = CYLSPER)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_log10() +  
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 2, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Total_phytoplankton_biovolume",
    y = "Cylindrospermopsins (CYL; µg/L)",
    title = "Relationship Between Total_phytoplankton_density and Cylindrospermopsins"
  )



S3C <- combined_data6 %>%
  ggplot(aes(x = total_cyanobacteria_density, y = CYLSPER)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  scale_x_log10() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 0.5, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Total_cyanobacteria_density",
    y = "Cylindrospermopsins (CYL; µg/L)",
    title = "Relationship Between Total_cyanobacteria_density and Cylindrospermopsins"
  )


S4D <- combined_data6 %>%
  ggplot(aes(x = PTOX_biovolume, y = CYLSPER)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  scale_x_log10() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "PTOX_biovolume",
    y = "Cylindrospermopsins (CYL; µg/L)",
    title = "Relationship Between PTOX_biovolume and Cylindrospermopsins"
  )

# Arrange plots together
ggarrange(S1A, S2B, S3C, S4D, ncol = 2, nrow = 2)


##############################################
#MIC
combined_data6 %>%
  ggplot(aes(x = percent_cyanobacteria_biovolume, y = MICX)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_continuous() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "percent_cyanobacteria_biovolume",
    y = "Microcystins (MIC; µg/L)",
    title = "Relationship Between percent_cyanobacteria_biovolume and Microcystins"
  )

#####CYL
combined_data6 %>%
  ggplot(aes(x = percent_cyanobacteria_biovolume, y = CYLSPER)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  scale_x_continuous() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "percent_cyanobacteria_biovolume",
    y = "Cylindrospermopsins (CYL; µg/L)",
    title = "Relationship Between percent_cyanobacteria_biovolume and Cylindrospermopsins"
  )
######################################################
#%ptox plot
#MIC
combined_data6 %>%
  ggplot(aes(x = percent_PTOX_biovolume, y = MICX)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "percent_PTOX_biovolume",
    y = "Microcystins (MIC; µg/L)",
    title = "Relationship Between percent_PTOX_biovolume and Microcystins"
  )

#####CYL
combined_data6 %>%
  ggplot(aes(x = percent_PTOX_biovolume, y = CYLSPER)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  scale_x_log10() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "percent_PTOX_biovolume",
    y = "Cylindrospermopsins (CYL; µg/L)",
    title = "Relationship Between percent_PTOX_biovolume and Cylindrospermopsins"
  )
########################################
#Mulitiple linear regression
#check names
names(combined_data6)

# Fit the multiple linear regression model
model <- lm(MICX ~ CHLA_RESULT + total_phytoplankton_density + total_cyanobacteria_density + 
              PTOX_biovolume + MIC_PTOX_biovolume + percent_cyanobacteria_biovolume +
              percent_PTOX_biovolume, data = combined_data6)

# Summary of the model
summary(model)

# Check assumptions
# Plot diagnostic plots
par(mfrow = c(2, 2))
plot(model)

# Check for multicollinearity
library(car)
vif(model)

# check correlations
cor(combined_data6[, c("MICX", "CHLA_RESULT", "total_phytoplankton_density", "total_cyanobacteria_density", 
             "PTOX_biovolume", "MIC_PTOX_biovolume", "percent_cyanobacteria_biovolume",
             "percent_PTOX_biovolume")], use = "complete.obs")
#####################################################################
Model2 <- lm(MICX ~ CHLA_RESULT + PTOX_biovolume + MIC_PTOX_biovolume +
     percent_cyanobacteria_biovolume + percent_PTOX_biovolume, data = combined_data6)

summary(Model2)
#####
Model3 <- lm(MICX ~ CHLA_RESULT + PTOX_biovolume + total_cyanobacteria_density + MIC_PTOX_biovolume +
               percent_cyanobacteria_biovolume + percent_PTOX_biovolume, data = combined_data6)

summary(Model3)
#not much difference
##############Log transferm
# Log-transform MICX (adding a small constant to avoid log(0) if needed)
combined_data6$log_MICX <- log(combined_data6$MICX + 0.01)  # Use 0.01 or other small value if zeros exist

# Fit the linear model without the highly collinear variable
log_model <- lm(log_MICX ~ CHLA_RESULT + total_cyanobacteria_density +
                  PTOX_biovolume + MIC_PTOX_biovolume +
                  percent_cyanobacteria_biovolume + percent_PTOX_biovolume,
                data = combined_data6)

# Summarize the model
summary(log_model)

# Optional: Check VIFs again
library(car)
vif(log_model)
#############################################################################
#cyl
combined_data6$log_CYLSPER <- log(combined_data6$CYLSPER + 0.01)  # Use 0.01 or other small value if zeros exist

# Fit the linear model without the highly collinear variable
log_model2 <- lm(log_CYLSPER ~ CHLA_RESULT + total_cyanobacteria_density +
                  PTOX_biovolume + CYL_PTOX_biovolume +
                  percent_cyanobacteria_biovolume + percent_PTOX_biovolume,
                data = combined_data6)

# Summarize the model
summary(log_model2)

# Optional: Check VIFs again
library(car)
vif(log_model2)
######################################
#siGNIFICANT ONLY
log_model3 <- lm(log_CYLSPER ~ PTOX_biovolume + CYL_PTOX_biovolume +
                   percent_cyanobacteria_biovolume,
                 data = combined_data6)

# Summarize the model
summary(log_model3)

log_model4 <- lm(log_MICX ~ CHLA_RESULT + total_cyanobacteria_density +
                  percent_cyanobacteria_biovolume,
                data = combined_data6)
# Summarize the model
summary(log_model4)
###########################################
