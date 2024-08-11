## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Cristina Alvarez
##
## Author(s):         Cristina Alvarez
##
## Dependencies:      World Justice Project
##
## Creation date:     Agosto 08, 2024
##
## This version:      Agosto 08, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, and data set
source("Settings/Settings.R")

# Upload data

schools <-  read_csv(paste0(path2SP,"Data/IEA_busqueda_08-08-2024.csv")) 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Proporción de escuelas públicas y privadas por nivel ----
##                                                      
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

education_path <- c(" MEDIA SUPERIOR",
                    " SECUNDARIA",                  
                    " PREESCOLAR",
                    " PRIMARIA",                
                    " SUPERIOR")


matrix_profile <- as.data.frame(schools) %>%
                  mutate(NIVEL = str_trim(NIVEL)) %>% 
                  group_by(NIVEL, SOSTENIMIENTO) %>%
                  summarise(value = n(), .groups = 'drop') %>%
                  rename(source = SOSTENIMIENTO, target = NIVEL) %>% 
                  filter(!target %in% c("CAPACITACION PARA EL TRABAJO", "ESPECIAL", "CONAFE", "INICIAL")) %>% 
                  mutate(order_var = case_when(target == "PREESCOLAR" ~ 1, 
                                               target == "PRIMARIA"   ~ 2,
                                               target == "SECUNDARIA" ~ 3,
                                               target == "MEDIA SUPERIOR" ~ 4,
                                               target == "SUPERIOR"  ~ 5)) %>% 
                arrange(order_var) %>% 
                group_by(target)  %>%
                mutate(
                 value2plot = value / sum(value) * 100,
                 value2plot = paste0(round(value2plot, 0), "%")) %>% 
                ungroup() 


nodes <- data.frame(
                    name=c(as.character(matrix_profile$source), 
                    as.character(matrix_profile$target)) %>% 
                    unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
matrix_profile$IDsource <- match(matrix_profile$source, nodes$name)-1 
matrix_profile$IDtarget <- match(matrix_profile$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = matrix_profile, 
                   Nodes = nodes,
                   Source = "IDsource", 
                   Target = "IDtarget",
                   Value = "value", 
                   NodeID = "name", 
                   sinksRight=FALSE,
                   fontSize = 12)

p


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.1. Proporción de escuelas públicas y privadas por nivel - DATOS ----
##                                                      
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Assuming you already have the matrix_profile_display data frame
matrix_profile_display <-  matrix_profile %>% 
  select(-order_var, -IDsource, -IDtarget) %>% 
  rename(Nivel = target,
         TIPO = source, 
         Total = value,
         Proporción = value2plot)

# Convert to a flextable
ft <- flextable(matrix_profile_display)

# Apply conditional background color for rows where TIPO is "PÚBLICO"
ft <- ft %>%
  bg(i = ~ TIPO == "PÚBLICO", bg = "grey", part = "body") %>%
  theme_vanilla() %>%
  set_header_labels(
    Nivel = "Nivel Educativo",
    TIPO = "Tipo de Sostenimiento",
    Total = "Total de Escuelas",
    Proporción = "Proporción (%)"
  ) %>%
  fontsize(size = 10) %>%
  bold(part = "header")

# Display the flextable
ft

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Mapa                                                  ----
##                                                      
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data2plot <- as.data.frame(schools) %>%
  mutate(NIVEL = str_trim(NIVEL),
         MUNICIPIO = str_trim(MUNICIPIO)) %>% 
  group_by(MUNICIPIO) %>%
  summarise(value = n(), .groups = 'drop') %>%
  mutate(
    value2plot = value / sum(value) * 100,
    value2plot = paste0(round(value2plot, 0), "%")) 


mapa <- st_read(paste0(path2SP,"Data/5_1_4_3_Municipios_shape")) %>% 
        filter(CVE_ENT == "01") %>% 
        mutate(NOMGEO = case_when(NOMGEO == "Aguascalientes"         ~ "AGUASCALIENTES",
                                  NOMGEO == "Asientos"               ~ "ASIENTOS",
                                  NOMGEO == "Calvillo"               ~ "CALVILLO",
                                  NOMGEO == "Cos\xedo"               ~ "COSÍO",
                                  NOMGEO == "Jes\xfas Mar\xeda"      ~ "JESÚS MARÍA",
                                  NOMGEO == "Pabell\xf3n de Arteaga" ~ "ENCARNACIÓN DE DÍAZ",
                                  NOMGEO == "Rinc\xf3n de Romos"     ~ "RINCÓN DE ROMOS",
                                  NOMGEO == "San Jos\xe9 de Gracia"  ~ "SAN JOSÉ DE GRACIA",
                                  NOMGEO == "Tepezal\xe1"            ~ "TEPEZALÁ",
                                  NOMGEO == "El Llano"               ~ "EL LLANO",
                                  NOMGEO == "San Francisco de los Romo" ~ "SAN FRANCISCO DE LOS ROMO")) %>% 
      rename(MUNICIPIO = NOMGEO)



ags_map <- mapa %>%
  left_join(data2plot, by = "MUNICIPIO") %>%
  mutate(
    color_group = case_when(
      value  <= 60   ~ "bajo",
      value  <= 250  ~ "medio",
      value  <= 1342   ~ "alto"
    ),
    color_group = as.factor(color_group)
  )

cat_palette <- c( "bajo"  = "#99D7DD",
                  "medio"  = "#0087A3",
                  "alto" = "#004E70")
# Drawing plot
p <- ggplot(ags_map, aes(label = MUNICIPIO)) +
  geom_sf(data  = ags_map,
          aes(fill = color_group),
          color = "grey65",
          size  = 0.5) +
  geom_sf(data  = ags_map,
          fill  = NA,
          color = "grey25") +
  scale_fill_manual("",
                    values   = cat_palette,
                    na.value = "grey95",
                    drop = F) +
  # scale_y_continuous(limits = c(1445631, 5273487)) +
  # scale_x_continuous(limits = c(2581570, 5967160)) +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    axis.text       = element_blank(),
    legend.position = "none",
    panel.grid      = element_blank(),
    panel.border    = element_blank(),
    plot.margin     = margin(0,0,0,0)
  ); p



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.1. Proporción de escuelas públicas y privadas por nivel - DATOS ----
##                                                      
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Convert to a flextable
ft <- flextable(data2plot)

# Apply conditional background color for rows where TIPO is "PÚBLICO"
ft <- ft %>%
  theme_vanilla() %>%
  set_header_labels(
    MUNICIPIO = "Nivel Educativo",
    value = "Total de Escuelas",
    value2plot = "Proporción (%)"
  ) %>%
  fontsize(size = 10) %>%
  bold(part = "header")

# Display the flextable
ft


  
  