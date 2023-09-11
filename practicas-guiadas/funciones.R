estadia_promedio <- function(data, region, fecha){
  
  estadia <- data %>% 
    group_by(trim_date, region_destino) %>% 
    summarise(turistas = sum(pondera, na.rm = T),
              pernoctes = sum(pondera * px07, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(estadia_promedio = pernoctes/turistas) %>% 
    filter(region_destino == region,
           trim_date == fecha) %>% 
    select(trim_date, region_destino, estadia_promedio)
  
  estadia
}
