rm(list = ls())
library(readr)
library(ggplot2) 
library(dslabs)
library(dplyr)

# ====================================================
# load data
meteo_data <- read_csv("Meteo_Vergiano skilled.csv", skip=10)
View(meteo_data)
xy_degree <- read.csv("Meteo_Vergiano skilled.csv",header=FALSE, skip = 6, nrows = 2)
xy_degree<- xy_degree[c(-3,-4,-5)]# dropping useless columns
View(xy_degree)
# ====================================================
# explore data
str(meteo_data) # to see dataframe structure
T_max_historic<- plot (meteo_data$Tmax, type="l", col=1,xlab = "DoY", ylab = "Tmax" ,ylim = c(-5,39))
T_min_historic<- plot (meteo_data$Tmin, type="l", col=2,xlab = "DoY", ylab = "Tmin" )
Rain_historic<- plot (meteo_data$`Rain [mm]`, type = "l", col= 3,xlab = "DoY", ylab = "Rain [mm]" ,ylim= c (0, 50))
cumulative_rain<- cumsum(meteo_data$`Rain [mm]`)
Cum_rain_historic<- plot(cumulative_rain,type= "l", col= 1, xlab ="DoY", ylab = "Cumulative rain [mm]")
# ====================================================
# calculate statistics
max(meteo_data$Tmin)
min(meteo_data$Tmin)
mean(meteo_data$Tmax)

stat_T_max<- summary(meteo_data$Tmax)

hist_T_max<- hist(meteo_data$Tmax)

# ====================================================
# define variables and parameters
lat_degree = xy_degree[1,2]
J = meteo_data$DoY
Tmax = meteo_data$Tmax
Tmin = meteo_data$Tmin
Tmin = as.numeric(Tmin)
Prec = meteo_data$`Rain [mm]`

# ====================================================
# Calculate ETp based on Hargreaves and Samani equation

# ------------------
# equation 1
lat_rad = lat_degree * pi / 180

# ------------------
# equation 2
inv_dist = 1 + 0.033 * cos(2 * pi * J / 365)

# ------------------
# equation 3
sol_decl = 0.409 * sin(2 * pi * J/365 - 1.39)

# ------------------
# equation 4
sun_angle = acos(-tan(lat_rad)*tan(sol_decl))

# ------------------
# equation 5
Ra = (24 * 60 / pi) * 0.082 * inv_dist * 
  (sun_angle * sin(lat_rad) * sin(sol_decl) 
   + cos(lat_rad) * cos(sol_decl) * sin(sun_angle))

# ------------------
# equation 6
average_T = (Tmax+Tmin)/2 

# ------------------
# equation 7
ETP = 0.0023 * Ra * (average_T + 17.8) * (Tmax - Tmin)^0.5 / 2.45

# plot
ETP_historic<- plot(ETP,type="l",ylab="ETP [mm/d]",xlab="J", ylim = c(-1,8))
grid()
WaterBalance = Prec-ETP
historic_WaterBalance<- barplot(WaterBalance, xlab = "DoY", ylab = "mm")

# ====================================================
#calculating kc based on ndvi and checking correlation
NDVI <- read_csv("NDVI_Vergiano.csv", col_names = FALSE)
colnames(NDVI) <- NDVI[2, ]
NDVI <- NDVI[-c(1, 2), ]
NDVI<- NDVI[-1]
NDVI$ndvi <- gsub(",", ".", NDVI$ndvi) # so that R recognizes number in NDVI $ ndvi
NDVI$ndvi <- as.numeric(NDVI$ndvi)
NDVI$Data <- as.Date(NDVI$Data,format = "%d/%m/%Y")
View(NDVI)

#calculating NDVI
NDVI['Kc']<- 1.33*(1.22*NDVI['ndvi']-0.21 )+0.14
mu_ndvi<- mean(NDVI$ndvi)
mu_kc<- mean(NDVI$Kc)
s_ndvi<- sd(NDVI$ndvi)
s_kc<- sd(NDVI$Kc)
r <- cor(NDVI$ndvi, NDVI$Kc)

NDVI %>%
  ggplot(aes(x = ndvi, y = Kc)) +  
  geom_point(alpha = 0.5) +  # adding points to the graph
  geom_abline(slope = r * s_kc / s_ndvi, 
              intercept = mu_kc - r * s_kc / s_ndvi * mu_ndvi) +  # adding the intercept
  labs(x = "kc", y = "ndvi", title = "Scatter Plot of NDVI vs kc") +  # labels and titol of the graph
  theme_minimal() 
