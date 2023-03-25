# Author: Parra Caporal José Andrés 
# Average years of schooling México


# Libraries
library("tidyverse") # To the data manipulation (dplyr) and visualization (ggplot)
library("sf") # To manage special data types
library("magick") # To make the gif

# Data file directory
setwd("C:/Users/parra/OneDrive/Documents/R/Data")

  # soh format from INEGI to the Map 
entidades <- st_read("Marco_geoestadistico_INEGI/Entidades/00ent.shp")

# The following function aims to read a list of file addresses
# and set of selected variables from the ENOE csv files. The final output
# of the function is to obtain a data frame containing all the 
# selected variables, joining each data frame by rows.

ENOE <- function(variables, datos, año_inicio, año_fin) {
  
  # A list of the years used
  años <- seq(from = año_inicio, to = año_fin)
  
  # This function use a lapply function to apply an anonmous 
  # function to each item in the data address list.
  # This code uses seq_along(datos) to loop over the indices of 
  # the datos vector and get the corresponding year from the años vector.
  df_list <- lapply(seq_along(datos), function(i) {
    # File directory.
    x <- datos[i]
    # Year from años vector.
    year <- años[i]
    # Read csv file.
    marco_datos <- read_csv(x, show_col_types = FALSE) %>% 
      # Rename the columns to minus.
      rename_all(tolower) %>% 
      # Select the variables.
      select(all_of(variables)) %>% 
      # Revodify the na values.
      mutate(across(everything(), ~ ifelse(. == "..", NA, .)))
    # Asign a year variable.
    marco_datos$year <- year
    return(marco_datos)
  })
  # the sapply() function applies a function to each element of a list or vector, 
  # and returns a vector with the results. In this case, the is.numeric() function 
  # is applied to each column of the first element of df_list. 
  # When we use the double bracket, R returns the lowest possible structure, 
  # so if we have a data frame the output when using the double bracket is a vector. 
  num_cols <- sapply(df_list[[1]], is.numeric)
  df_list <- lapply(df_list, function(df) {
    # Double formar to numeric variables.
    df[num_cols] <- lapply(df[num_cols], as.numeric)
    # Character to numeric.
    df[!num_cols] <- lapply(df[!num_cols], as.numeric)
    return(df)
  })
  
  # Bind data frames in the list by rows.
  datos <- bind_rows(df_list)
  
  return(datos)
}

# List of files directory, in this case only use the sdem table.
datos_list <- c("ENOE/sdemt405.csv",
                "ENOE/sdemt406.csv",
                "ENOE/sdemt407.csv",
                "ENOE/sdemt408.csv",
                "ENOE/sdemt409.csv",
                "ENOE/sdemt410.csv",
                "ENOE/sdemt410.csv",
                "ENOE/sdemt412.csv",
                "ENOE/sdemt413.csv",
                "ENOE/SDEMT414.csv",
                "ENOE/SDEMT415.csv",
                "ENOE/SDEMT416.csv",
                "ENOE/SDEMT417.csv",
                "ENOE/SDEMT418.csv",
                "ENOE/sdemt419.csv",
                "ENOE/enoen_sdemt420.csv",
                "ENOE/ENOEN_SDEMT421.csv",
                "ENOE/ENOEN_SDEMT422.csv")

# Variables selected.
variables_enoe <- c("ent",       # Federal Entity
                    "anios_esc", # Years of schooling
                    "cs_p13_1")  # Levels of schooling

# Data, from 2005 to 2022.                                     
data_ENOE <- ENOE(variables_enoe,datos_list,2005,2022)
str(data_ENOE)

# Convert ent variable to a factor variable.
data_ENOE$ent <- factor(data_ENOE$ent, 
                        levels = c(1,2,3,4,5,6,7,8,
                                   9,10,11,12,13,14,15,16,
                                   17,18,19,20,21,22,23,24,
                                   25,26,27,28,29,30,31,32),
                        labels = c("Aguascalientes",
                                   "Baja California",
                                   "Baja California Sur",
                                   "Campeche",
                                   "Coahuila de Zaragoza",
                                   "Colima",
                                   "Chiapas",
                                   "Chihuahua",
                                   "Ciudad de México",
                                   "Durango",
                                   "Guanajuato",
                                   "Guerrero",
                                   "Hidalgo",
                                   "Jalisco",
                                   "México",
                                   "Michoacán de Ocampo",
                                   "Morelos",
                                   "Nayarit",
                                   "Nuevo León",
                                   "Oaxaca",
                                   "Puebla",
                                   "Querétaro",
                                   "Quintana Roo",
                                   "San Luis Potosí",
                                   "Sinaloa",
                                   "Sonora",
                                   "Tabasco",
                                   "Tamaulipas",
                                   "Tlaxcala",
                                   "Veracruz de Ignacio de la Llave",
                                   "Yucatán",
                                   "Zacatecas"))

# I only consider the people with defined levels of education,
# In ths case we omit the "No sabe" observations.
data_ENOE <- data_ENOE %>% 
  filter(cs_p13_1 != 99)

# Convert the levels of educatio to factor.
data_ENOE$cs_p13_1 <- factor(data_ENOE$cs_p13_1,
                             levels = c(0,1,2,3,4,5,6,7,8,9),
                             labels = c("Ninguna",
                                        "Preescolar",
                                        "Primaria",
                                        "Secundaria",
                                        "Preparatoria o bachillerato",
                                        "Normal",
                                        "Carrera técnica",
                                        "Profesional",
                                        "Maestría",
                                        "Doctorado"))

# We calculate the average years of schooling by state in heach year.
# First we agroup the variables by year, and then by entity, calculing the
# average years of schooling by year from each state.
data_ENOE_yr_school <- data_ENOE %>%
  select(c("ent","anios_esc","year")) %>% 
  group_by(year, ent) %>% 
  summarise(mean_yr_school = mean(anios_esc)) %>%
  mutate(decil = factor(ntile(mean_yr_school, 10))) %>% 
  ungroup()

estados_variables <- left_join(entidades, 
                               data_ENOE_yr_school, 
                               by = c("NOMGEO" = "ent"),
                               multiple = "all")

str(estados_variables)

colores <- c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", 
             "#CB181D", "#A50F15", "#67000D", "#7F0000")

years_schooling_plot <- estados_variables %>% 
  filter(year == 2022) %>% 
  ggplot() +
  geom_sf(aes(fill = decil)) +
  scale_fill_manual(values = colores) +
  labs(title = paste("Deciles, Average Years Schooling, 2022"),
       subtitle = "Fuente: ENOE") +
  guides(fill=guide_legend(title = "Decil")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "italic"),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.text = element_text(size = 10),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.5, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

years_schooling_plot

# Loop through years and create plot for each year

setwd("C:/Users/parra/OneDrive/Documents/R/Programs/Figures/years_edu")

for (i in 2005:2022) {
  p <- estados_variables %>% 
    filter(year == i) %>% 
    ggplot() +
    geom_sf(aes(fill = decil)) +
    scale_fill_manual(values = colores) +
    labs(title = paste("Deciles, Average Years Schooling,", i),
         subtitle = "Fuente: ENOE") +
    guides(fill=guide_legend(title = "Decil")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10, face = "italic"),
          legend.position = "bottom",
          legend.title.align = 0.5,
          legend.text = element_text(size = 10),
          legend.key.width = unit(1, "cm"),
          legend.key.height = unit(0.5, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  
  # Save plot as jpg
  ggsave(paste0("years_schooling_", i, ".jpg"), p, width = 6, height = 4)
}


# List of images
images <- image_read(paste0("years_schooling_", 2005:2022, ".jpg"))

# Create a GIF
years_schooling_gif <- image_animate(images, fps = 1)

# Save the gif
image_write(years_schooling_gif, "years_schooling.gif")