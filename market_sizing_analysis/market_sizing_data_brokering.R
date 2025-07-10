


# Setup -------------------------------------------------------------------

library(tidyverse)
#library(tidycensus)
library(jsonlite)
library(cluster)
library(factoextra)
library(tigris)

#key <- rstudioapi::askForPassword()

#A key can be obtained from http://api.census.gov/data/key_signup.html
#census_api_key(key)


# Get data ----------------------------------------------------------------

# Use API to retrieve: all of DP03 from US Census for Ohio
data <- fromJSON(url("https://api.census.gov/data/2023/acs/acs5/profile?get=group(DP03)&ucgid=pseudo(0400000US39$1400000)&descriptive=true"))
data <- as.data.frame(data)
colnames(data) <- data[1,]
data <- data[-1,]
data <- data %>%
  select(GEO_ID,NAME,ends_with("PE"))

metadata <- data[1,]
metadata <- metadata %>%
  pivot_longer(everything(),names_to="metric",values_to="description")
data <- data[-1,]

data2 <- fromJSON(url("https://api.census.gov/data/2023/acs/acs5/profile?get=group(DP03)&ucgid=pseudo(0400000US53$1400000)&descriptive=true"))
data2 <- as.data.frame(data2)
colnames(data2) <- data2[1,]
data2 <- data2[-1,]
data2 <- data2 %>%
  select(GEO_ID,NAME,ends_with("PE"))
data2 <- data2[-1,]

# Manipulate & clean data ---------------------------------------------------------

neg_to_na <- function(x) {
  ifelse(x<0,NA,x)
}

# convert to numeric & change negatives to NAs
data_clean <- data %>%
  union(data2) %>%
  mutate(across(c(DP03_0001PE:DP03_0137PE), as.numeric)) %>% 
  mutate(across(c(DP03_0001PE:DP03_0137PE), neg_to_na)) 
nrow(data_clean)

# look at NAs
na_sums <- data_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count")

na_sums %>%
  arrange(desc(na_count)) %>%
  print(n=139)

# remove variables with >40 NAs
vars_keep <- na_sums %>%
  filter(na_count<=40)
data_clean <- data_clean %>%
  select(vars_keep$column) %>%
  na.omit()
nrow(data_clean)

# prep data for clustering
data_clust <- data_clean %>%
  mutate(across(where(is.numeric), 
                ~ as.numeric(scale(.x))))

# Clustering --------------------------------------------------------------

# create plot of number of clusters vs total within sum of squares
fviz_nbclust(data_clust[3:103], kmeans, method = "wss") # 4 clusters for now

# perform k-means clustering with k = 5 clusters
set.seed(17)
km <- kmeans(data_clust[3:103], centers = 5, nstart = 25)

# add cluster assignment to data
final_data <- cbind(data_clean, cluster = km$cluster)

# Cluster demographics ----------------------------------------------------

table(final_data$cluster)

# 1:15 = basic demographics
# 16:47 = jobs
# 48:64 = income indiv
# 65:101 = families

cluster_sums1 <- final_data %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean)) %>%
  select(cluster,DP03_0001PE:DP03_0022PE) %>%
  pivot_longer(!cluster, names_to="metric",values_to="value") %>%
  left_join(metadata,by="metric")

cluster_sums2 <- final_data %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean)) %>%
  select(cluster,DP03_0023PE:DP03_0043PE) %>%
  pivot_longer(!cluster, names_to="metric",values_to="value") %>%
  left_join(metadata,by="metric")

cluster_sums3 <- final_data %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean)) %>%
  select(cluster,DP03_0044PE:DP03_0066PE) %>%
  pivot_longer(!cluster, names_to="metric",values_to="value") %>%
  left_join(metadata,by="metric")

cluster_sums4 <- final_data %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean)) %>%
  select(cluster,DP03_0068PE:DP03_0099PE) %>%
  pivot_longer(!cluster, names_to="metric",values_to="value") %>%
  left_join(metadata,by="metric")

ggplot(cluster_sums1,aes(x=cluster,y=value)) +
  geom_col() +
  facet_wrap(~metric,scales="free") +
  labs(x="",y="")
ggplot(cluster_sums2,aes(x=cluster,y=value)) +
  geom_col() +
  facet_wrap(~metric,scales="free") +
  labs(x="",y="")
ggplot(cluster_sums3,aes(x=cluster,y=value)) +
  geom_col() +
  facet_wrap(~metric,scales="free") +
  labs(x="",y="")
ggplot(cluster_sums4,aes(x=cluster,y=value)) +
  geom_col() +
  facet_wrap(~metric,scales="free") +
  labs(x="",y="")


# Plot maps ---------------------------------------------------------------

# Ohio
# oh_tracts <- tracts("OH")
# oh_tracts <- final_data %>%
#   select(GEO_ID,cluster) %>%
#   right_join(oh_tracts,by=c("GEO_ID"="GEOIDFQ"))
# 
# table(oh_tracts$cluster,useNA="ifany")
# 
# plot(oh_tracts$geometry)
# 
# ggplot() + 
#   geom_sf(data = oh_tracts, aes(fill = as.factor(cluster),geometry = geometry)) +
#   labs(fill="Cluster") +
#   theme(
#     panel.background = element_blank(),
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     axis.ticks = element_blank()
#   )
# 
# # Washington
# wa_tracts <- tracts("WA")
# wa_tracts <- final_data %>%
#   select(GEO_ID,cluster) %>%
#   right_join(wa_tracts,by=c("GEO_ID"="GEOIDFQ"))
# 
# table(wa_tracts$cluster,useNA="ifany")
# 
# plot(wa_tracts$geometry)
# 
# ggplot() + 
#   geom_sf(data = wa_tracts, aes(fill = as.factor(cluster),geometry = geometry)) +
#   labs(fill="Cluster") +
#   theme(
#     panel.background = element_blank(),
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     axis.ticks = element_blank()
#   )

# Examine external variables ----------------------------------------------

ev <- read.csv("/cloud/project/market_sizing_analysis/Electric_Vehicle_Population_Data.csv")
ev <- ev %>%
  mutate(census_tract = paste0("1400000US",X2020.Census.Tract))

table(ev$Make) # could always group these using the lump factor

# ev %>%
#   group_by(Electric.Vehicle.Type,Model.Year) %>%
#   count() %>%
#   ggplot(aes(x=Model.Year,y=n,color=Electric.Vehicle.Type)) +
#   geom_line() +
#   labs(x="Model Year",
#        y="Count per Model Year",
#        color="EV Type") +
#   theme_minimal()


# Join in EV data ---------------------------------------------------------

ev_sums <- ev %>%
  group_by(census_tract) %>%
  count()

tract_data <- final_data %>%
  mutate(pop_over_16=DP03_0001PE) %>%
  select(GEO_ID,NAME,cluster,pop_over_16) %>%
  left_join(ev_sums,by=c("GEO_ID"="census_tract")) %>%
  separate(NAME,into=c("tract", "county","state"), sep="; ")

tract_data %>%
  filter(state=="Washington") %>%
  group_by(cluster) %>%
  summarize(avg_evs=mean(n,na.rm=T),
            avg_evs_per_capita=mean(n/pop_over_16,na.rm=T),
            avg_evs_per_1000=mean(n/pop_over_16*1000,na.rm=T))



