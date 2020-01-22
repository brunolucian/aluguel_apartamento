library(magrittr) # não vivo sem esse pacote
library(rvest) # principal pacote para web-scraping
library(readr) # usado para extrair numeros de texto
library(stringr) # usado para o data cleaning
library(curl) # usado como suporte para o rvest
library(tidyr) # data cleaning
library(dplyr) # data cleaning


url_apt <- "https://sp.olx.com.br/sao-paulo-e-regiao/imoveis/aluguel/apartamentos"
number_pages <- 100 #hard coded
# Criar vetor com todos os urls para as páginas do olx
lista_urls <- paste0(url_apt, "?o=", 1:number_pages)

source("functions_data_cleaning.R")

lista_dados <- NULL
for (i in seq_along(lista_urls)) {
  print(paste0("Estamos na pagina ", i, ", faltam ", max(seq_along(lista_urls)) - i))
  lista_dados[[i]] <- extrairAnuncios(lista_urls[i])
  
}

dados_full = do.call("rbind", lista_dados)
