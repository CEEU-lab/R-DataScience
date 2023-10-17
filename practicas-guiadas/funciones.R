estadia_promedio <- function(data, region, year, trim){
  
  estadia <- data %>% 
    group_by(anio, trimestre, region_destino) %>% 
    summarise(turistas = sum(pondera, na.rm = T),
              pernoctes = sum(pondera * px07, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(estadia_promedio = pernoctes/turistas) %>% 
    filter(region_destino == region,
           anio == year,
           trimestre == trim) %>% 
    select(anio, trimestre, region_destino, estadia_promedio)
  
  estadia
}
