#This script is to QC the port sampling data from the 2022 NSEI fishery
#Last updated 12/20/22


library(pacman)
p_load("ggplot2","tidyr","dplyr", "tibble", "janitor", "stringr",
       "readr","lubridate")

raw_dat <- read_csv("Chatham Fishery/Data/NSEI_port_sampling_data_030223.csv", na = c("", " ", "NA")) %>% 
  rename_all(tolower) %>% 
  rename(trip_number="trip number",
         sample_date="sample date",
         groundfish_management_area_code="groundfish management area code",
         sample_type="sample type",
         specimen_number="specimen number",
         length_type_code="length type code",
         weight_kg="weight kilograms",
         maturtiy_code="maturity code",
         age_readability_code="age readability code",
         project_code="project code",
         adfg_num="adfg number",
         groundfish_stat_area="groundfish stat area",
         delivery_code="delivery code",
         species_code="species code",
         length_type="length type",
         sex_code="sex code",
         age_readability="age readability",
         groundifsh_stat_area_group = "groundfish stat area group",
         sample_type_code="sample type code",
         length_mm="length millimeters",
         sampler_name="sampler name") 

project_code <- raw_dat %>% 
  select(project,project_code) %>% 
  distinct()

# #Excel does this weird thing with dates - change it into an actual date!
# raw_dat$sample_date <- as.Date(raw_dat$sample_date, origin="1900-01-01")

#clean up the data to include only NSEI landings
dat <- raw_dat %>% 
  filter(project_code %in% c("602","617"),
         year==2022,
         species_code==710,
         groundfish_management_area_code=="NSEI")

write.csv(dat,"Outputs/port_sampling_data.csv")

#1610 observations

dat_summary <-dat %>%
  select(length_mm, weight_kg, age) %>%
  summarise(across(c(length_mm, weight_kg, age),list(mean=mean,max=max), na.rm=TRUE))

#Any missing ADFG numbers?
dat <- dat %>% 
  mutate(missing_len=is.na(dat$length_mm))

unique(dat$missing_len)
#one missing length - EXCL this sample?

#Any missing ADFG numbers?
dat <- dat %>% 
  mutate(missing=is.na(dat$adfg_num))

unique(dat$missing)
#all false

#Any missing stat areas?
dat <- dat %>% 
  mutate(missing_area=is.na(dat$groundfish_stat_area))

unique(dat$missing_area)

missing_stat_area <- dat %>% 
  filter(missing_area=="TRUE")

unique(missing_stat_area$trip_number)
#113  139  143  204  208
#113 - 345701 and 345631
#139 - 345731 and 345701
#143 - 345731 and 345701
#204 - 345702 and 345701
#208 - 345702 and 345701

#check the sample type - should be mostly random
unique(dat$sample_type)

#check the delivery - should be whole fish/food fish
unique(dat$delivery)

#check length code - should be tip of nout to form of tail
unique(dat$length_type)

#lets check the sex code - sex should be 01–Male, 02–Female, or 99–Indiscernible
unique(dat$sex_code)

#length
boxplot(dat$length_mm,
        main="Length of 2022 NSEI Sablefish",
        ylab = "Length (mm)")

length_summary <- dat
summary(dat$length_mm)

#weight
boxplot(dat$weight_kg,
        main="Weight of 2022 NSEI Sablefish",
        ylab = "Weight (kg)")

#length vs. weight
ggplot(data=dat,aes(x=length_mm,y=weight_kg))+
  geom_point(size=3,alpha=.6)+
  labs(x="Length (mm)", y="Weight (kg)")+
  theme_classic()

ggsave("Chatham Fishery/Figures/NSEI_length_Weight_plot.jpeg")

dat_new <- dat %>% 
  filter(maturity=="Mature"|maturity=="Immature") %>% 
  mutate(length_cm = length_mm/10)

ggplot(data = dat_new, aes(length_cm, weight_kg, color = sex)) +
  geom_jitter(size = 2, alpha = .4) +
  geom_smooth(color = "black")+
  # geom_hline(yintercept=63, linetype="dashed", color = "black")+
  facet_grid(~maturity)+
  labs(x="Length (cm)", y="Weight (kg)", color = "")+
  theme_classic()+
  theme(
    legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    strip.text = element_text(size=14))+
  theme(legend.position="bottom")

ggsave("Chatham Fishery/Figures/NSEI_length_Weight_sex_plot.jpeg")


dat_female <- dat_new %>% 
  filter(sex=="Female") 
# %>%
#   group_by(maturity) %>% 
#   summarize(n())#1141 Females 

dat_male <- dat_new %>% 
  filter(sex=="Male")
# %>%
#   group_by(maturity) %>% 
#   summarize(n())#460 Males


#female only
a <- ggplot(data = dat_female, aes(weight_kg, length_cm)) +
  geom_jitter(size = 2, alpha = .4) +
  geom_hline(yintercept=63, linetype="dashed", color = "red")+
  facet_grid(~maturity)+
  labs(x="Weight (kg)", y="Length (cm)", color = "", title = "2022 NSEI Female Sablefish")+
  theme_classic()+
  theme(
    legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    strip.text = element_text(size=14))

# ggsave("Chatham Fishery/Figures/NSEI_length_Weight_female_only_plot.jpeg")

#male only
b <- ggplot(data = dat_male, aes(weight_kg, length_cm)) +
  geom_jitter(size = 2, alpha = .4) +
  facet_grid(~maturity)+
  labs(x="Weight (kg)", y="Length (cm)", color = "", title = "2022 NSEI Male Sablefish")+
  theme_classic()+
  theme(
    legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    strip.text = element_text(size=14))

# ggsave("Chatham Fishery/Figures/NSEI_length_Weight_male_only_plot.jpeg")

