## Por Josué Jiménez Vázquez
## Se utilizaron datos abiertos de las cirugías efectuadas en el hospital general dr. Manuel Gea Gonzales.
## Disponibles en: https://datos.gob.mx/busca/dataset/cirugias-efectuadas-en-el-hospital

## Se cargan los datos de todos los años

year_start <- 2013
year_end <- 2020

cirugias <- lapply(year_start:year_end, cirugias_data_func)

cirugias <- Reduce(rbind, cirugias)

## Unificando los C.I.E.10 que tienen dos descripciones

cirugias <- cirugias %>% mutate(DESCRIPCION...5 = case_when(
  C.I.E.10 == "A979" ~ "Dengue, no especificado", 
  C.I.E.10 == "E101" ~ "Diabetes mellitus tipo 1, con cetoacidosis",
  C.I.E.10 == "E111" ~ "Diabetes mellitus tipo 2, con cetoacidosis",
  C.I.E.10 == "E115" ~ "Diabetes mellitus tipo 2, con complicaciones circulatorias periféricas",
  C.I.E.10 == "E117" ~ "Diabetes mellitus tipo 2, con complicaciones múltiples",
  C.I.E.10 == "E118" ~ "Diabetes mellitus tipo 2, con complicaciones no especificadas",
  C.I.E.10 == "E119" ~ "Diabetes mellitus tipo 2, sin mención de complicación",
  C.I.E.10 == "P361" ~ "Sepsis del recién nacido debida a otros estreptococos y a los no especificados",
  TRUE ~ DESCRIPCION...5))

## Para determinar el numero de procedimientos realizados a cada paciente

cirugias <- cirugias %>% mutate(n_na = rowSums(is.na(cirugias)))

cirugias <- cirugias %>% mutate(N_CIE9 = (27-7-n_na)/2)

## Para obtener la tabla del codigo de los diagnosticos (C.I.E.10) y su descripción

CIE10 <- cirugias %>% group_by(C.I.E.10 , DESCRIPCION...5) %>% 
  summarise(n = n(),.groups = 'drop') %>% 
  rename("CIE10" = C.I.E.10, "DESCRIPCION" = DESCRIPCION...5) %>%
  mutate(factor = as.numeric(as.factor(CIE10)))

# Este codigo se uso para determinar los codigos C.I.E.10 que tenian dos descripciones
# CIE10_mas1 <- CIE10 %>% group_by(CIE10) %>% summarise(n = n(),.groups = 'drop') %>% 
#               filter(n > 1)
# CIE10_mas1 <- CIE10 %>% filter(CIE10 %in% CIE10_mas1$CIE10)
# 
# CIE10_mas1

## Para obtener la tabla del codigo de los procedimientos (C.I.E.9)

CIE9_names <- lapply(1:10, CIE9_names_func)

CIE9_names <- Reduce(rbind, CIE9_names)

CIE9_names <- CIE9_names %>% group_by(CIE9, DESCRIPCION) %>% summarise(n = n(),.groups = "drop")

## Para cambiar los nombres de las columnas, quitar las descripciones y solo tomar los codigos

cirugias_set <- cirugias %>% 
  select(-names(cirugias[(str_detect(names(cirugias),"DESCRIPCION"))]), -year, -n_na)

old_names <- names(cirugias_set[(str_detect(names(cirugias_set),"\\."))])
old_names

new_names <- data.frame(nombre = old_names)
new_names

new_names <- new_names %>% mutate(nombre = str_replace_all(nombre, "\\.", "")) %>%
  separate(nombre, c("codigo", "numero"), "MC", remove = FALSE)

new_names <- new_names %>% mutate(numero = 0:(length(numero)-1),
                                  nombre = ifelse(
                                    nombre == "CIE10", nombre, paste0(codigo, "_", as.character(numero)))) %>%
  pull(nombre)
new_names

## Arreglo de los datos para la aplicación

cirugias_set <- cirugias_set %>% rename_at(vars(all_of(old_names)), ~new_names)

cirugias_dat <- cirugias_set %>% filter(TiempoEdad == "AÑOS")

save(cirugias_dat, file = "rdas/cirugias_dat.rda")

cirugias_5mas <- cirugias %>% filter(C.I.E.10 %in% CIE10_5mas$CIE10)

CIE10_5a <- cirugias_5mas %>% filter(TiempoEdad == "AÑOS") %>%
  select(C.I.E.10, DESCRIPCION...5) %>%
  rename("CIE10" = C.I.E.10, "DESCRIPCION" = DESCRIPCION...5) 

CIE10_5a <- CIE10_5a %>% 
  group_by(CIE10, DESCRIPCION) %>% 
  summarise(n = n(),.groups = "drop") 

save(CIE10_5a, file = "rdas/CIE10_5a.rda")


