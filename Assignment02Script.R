library(tidyverse)
library(Stat2Data)
data("Hawks")

names_variable <- colnames(Hawks)
print(names_variable)

data_species <- Hawks  %>%
  distinct(Species)
print(data_species)

weight_data <- Hawks  %>%
  distinct(Weight) %>%
  arrange(desc(Weight))
head(weight_data,5)

hSF <- Hawks %>%
  filter(Species == "RT", Weight >= 1000) %>%
  select(Wing, Weight, Tail)

head(hSF,5)

hawkSpeciesNameCodes <- data.frame(
  species_code=c("CH","RT","SS"),
  species_name_full=c("Cooper's","Red-tailed","Sharp-shinned")
)
print(hawkSpeciesNameCodes)

hawkFullName <- Hawks %>%
  left_join(hawkSpeciesNameCodes, by=c("Species"="species_code")) %>%
  select(-Species) %>%
  rename(Species=species_name_full) 
head(hawkFullName,7)

num_variables <- length(names_variable)
print(num_variables)

hSF_wing <- hSF %>%
  arrange(Wing)
head(hSF_wing,5)


hawksWithBMI <- Hawks %>%
  select(Species,Wing,Weight) %>%
  mutate(bird_BMI = 1000*Weight/(Wing*Wing)) %>%
  select(Species,bird_BMI) %>%
  arrange(desc(bird_BMI))
head(hawksWithBMI, 8)

summary_data <- hawkFullName %>%
  filter(Wing != 0 | Tail != 0) %>% 
  group_by(Species) %>%
  summarize(
    num_rows = n(),
    mn_wing = mean(Wing, na.rm = TRUE),
    nd_wing = median(Wing, na.rm = TRUE),
    t_mn_wing = mean(Wing, trim=0.1, na.rm = TRUE),
    b_wt_ratio = max(Wing/Tail, na.rm = TRUE)
    ) %>%
  select(Species,num_rows,mn_wing,nd_wing,t_mn_wing,b_wt_ratio)
print(summary_data)

summary_number <- hawkFullName %>%
  select(Species,everything()) %>%
  group_by(Species) %>%
  summarize(
    across(
      Wing:Tarsus,
      ~ sum(is.na(.))
    ),
    Crop = sum(is.na(Crop))
  )
print(summary_number)
  