library(magrittr) # não vivo sem esse pacote
library(rvest) # principal pacote para web-scraping
library(readr) # usado para extrair numeros de texto
library(stringr) # usado para o data cleaning
library(curl) # usado como suporte para o rvest
library(tidyr) # data cleaning
library(dplyr) # data cleaning


url_apt <- "https://sp.olx.com.br/sao-paulo-e-regiao/imoveis/aluguel/apartamentos"
number_pages <- 245 #hard coded
# Criar vetor com todos os urls para as páginas do olx
lista_urls <- paste0(url_apt, "?o=", 1:number_pages)

url_teste <- lista_urls[1]
# system.time(df <- extrairAnuncios(url_teste, info_adicional = TRUE))

mycurl <- curl(url_teste, handle = curl::new_handle("useragent" = "Mozilla/5.0"))
mycurl <- read_html(mycurl)

x <- mycurl %>% html_nodes(".OLXad-list-link")

x <- mycurl %>% 
  html_nodes(".section_OLXad-list") %>% 
  html_nodes(".item:not(.list_native)") 

# extrair link do anuncio

col_links <- mycurl %>% 
  html_nodes(".section_OLXad-list") %>% 
  html_nodes(".item:not(.list_native)") %>% 
  html_nodes("a") %>% 
  html_attr("href")

# col_links <- mycurl %>% html_nodes(".OLXad-list-link") %>% html_attr("href")
# extrair titulo do anuncio
col_titles <- mycurl %>% 
  html_nodes(".section_OLXad-list") %>% 
  html_nodes(".item:not(.list_native)") %>% 
  html_nodes("a") %>% 
  html_attr("title")
  
# extrair preço
precos <- lapply(x, . %>% html_nodes(".col-3"))
precos <- lapply(x, . %>% html_nodes(".OLXad-list-price"))

precos %<>% lapply(html_text)
precos %<>% unlist()
precos %<>% str_squish() 
# precos %<>% limparString()
# precos %<>% as.numeric()
col_precos <- precos
# extrair bairros
bairros <- mycurl %>% html_nodes(".OLXad-list-line-2") %>% html_text()
bairros <- mycurl %>% html_nodes(".detail-region") %>% html_text()
bairros %<>% str_replace_all("[\t]", "")
bairros %<>% str_replace_all("[\n]", "")
bairros %<>% str_replace_all("Apartamentos", "")
bairros %<>% str_replace_all("Aluguel de quartos", "")
bairros %<>% str_replace_all("Anúncio Profissional", "")
bairros %<>% str_replace("-", "")
bairros %<>% str_trim()
col_bairros <- bairros
# extrair informações adicionais de apartamento

adicional <- mycurl %>% html_nodes(".mb5px") %>% html_text()
adicional %<>% str_replace_all("[\t]", "")
adicional %<>% str_replace_all("[\n]", "")
col_adicionais <- adicional

dados <- data.frame(link = col_links,
           titulo = col_titles,
           preco = col_precos,
           bairro = col_bairros,
           adicional = col_adicionais,
          stringsAsFactors = FALSE)
