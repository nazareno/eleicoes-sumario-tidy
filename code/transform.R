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
        
        group_by(ano, turno) %>% 
        mutate(total_votos = sum(votos)) %>% 
        mutate(porcentagem = round((votos/total_votos) * 100, digits = 2)) %>% 
        ungroup() %>% 
        
        select(estado, nome, partido, coligacao, votos, porcentagem, cargo, ano, turno)
    
    return(votos_presidente)
}

dados_presidente_agrupado <- function(data_path) {
    votos <- dados_presidente_partido(data_path)
    
    votos_agrupado <- votos %>% 
        group_by(partido, ano, turno) %>% 
        summarise(nome = first(nome),
                  votos = sum(votos),
                  porcentagem = sum(porcentagem))
    
    return(votos_agrupado)
}

dados_presidente_disputa_turno <- function(data_path) {
    votos <- dados_presidente_partido(data_path) %>% 
        mutate(nome = ifelse(grepl("Collor", nome), "Fernando Collor", nome)) %>% 
        mutate(nome = ifelse(grepl("Lula", nome), "Luiz Inácio Lula da Silva", nome)) %>% 
        mutate(nome = ifelse(grepl("Geraldo", nome), "Geraldo Alckmin", nome)) %>% 
        mutate(nome = ifelse(grepl("Serra", nome), "José Serra", nome))

    votos_turno <- votos %>% 
        group_by(nome, ano, turno) %>% 
        summarise(votos = sum(votos),
                  porcentagem = sum(porcentagem)) %>% 
        ungroup() %>% 
        group_by(ano, nome) %>% 
        mutate(n = n()) %>% 
        filter(n > 1) %>% 
        select(-n) %>% 
        ungroup() %>% 
        mutate(candidato = nome) %>% 
        mutate(nome = paste0(ano, " - ", nome)) %>% 
        arrange(nome)
    
    return(votos_turno)
}

dados_presidente_estado_historico <- function(data_path, ano_eleicao) {
    
    votos <- read_csv(data_path)
    
    votos_presidente <- votos %>%
        mutate(coligacao = sapply(str_extract_all(candidato, "\\b[A-Z]+\\b|PCdoB|PDCdoB|PTdoB"), paste, collapse= ' ')) %>% 
        mutate(partido = gsub("([A-Za-z]+).*", "\\1", coligacao)) %>%
        rowwise() %>% 
        mutate(nome = gsub(paste0("\\", partido, ".*", "|\\("), "", candidato)) %>% 
        na.omit() %>% 
        filter(estado != "TOTAL") %>%
        
        mutate(estado = ifelse(grepl("Federal", estado), "Distrito Federal", estado)) %>% 
        select(estado, ano, turno, partido, votos)
        
    votos_estado_historico <- votos_presidente %>% 
        filter(partido %in% c("PT", "PSDB")) %>% 
        filter(ano == ano_eleicao, turno == "Turno 2") %>% 
        
        group_by(estado) %>% 
        mutate(total = sum(votos)) %>% 
        mutate(porcentagem = (votos / total) * 100) %>% 
        ungroup()
    
    return(votos_estado_historico)
}

library(here)
data_path <- here("data/votos_tidy_long.csv")
votos <- dados_presidente_partido(data_path)
write.csv(votos, here("data/votos_presidente_partido.csv"), row.names = FALSE)

votos_agrupado <- dados_presidente_agrupado(data_path)
write.csv(votos_agrupado, here("data/votos_presidente_agrupado.csv"), row.names = FALSE)

votos_turno <- dados_presidente_disputa_turno(data_path)
write.csv(votos_turno, here("data/votos_presidente_segundo_turno.csv"), row.names = FALSE)

votos_estado_ano <- dados_presidente_estado_historico(data_path, 2014)
write.csv(votos_estado_ano, here("data/votos_presidente_estado_ano.csv"), row.names = FALSE)
