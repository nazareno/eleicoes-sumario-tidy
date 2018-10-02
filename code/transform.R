dados_presidente_partido <- function(data_path){
    # data_path - caminho para os dados votos_tidy_long
    library(tidyverse)
    library(readr)
    
    votos <- read_csv(data_path)
    
    votos_presidente <- votos %>%
        mutate(coligacao = sapply(str_extract_all(candidato, "\\b[A-Z]+\\b|PCdoB|PDCdoB|PTdoB"), paste, collapse= ' ')) %>% 
        mutate(partido = gsub("([A-Za-z]+).*", "\\1", coligacao)) %>%
        rowwise() %>% 
        mutate(nome = gsub(paste0("\\", partido, ".*", "|\\("), "", candidato)) %>% 
        na.omit() %>% 
        filter(estado != "TOTAL") %>%
        select(estado, nome, partido, coligacao, votos, cargo, ano, turno)
    
    return(votos_presidente)
}

data_path <- here("data/votos_tidy_long.csv")
votos <- dados_presidente_partido(data_path)
write.csv(votos, here("data/votos_presidente_partido.csv"), row.names = FALSE)
