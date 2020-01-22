
# definir função para limpar strings coletadas
limparString <- function(x) {
  # x = string coletado do olx
  x %<>% str_replace_all("[\t]", "")
  x %<>% str_replace_all("[\n]", "")
  x %<>% str_replace_all("Apartamentos", "")
  x %<>% str_replace_all("Anúncio Profissional", "")
  x %<>% str_replace("-", "")
  x %<>% str_replace_all("[R$]", "")
  x %<>% str_replace_all("[.]", "")
  x %<>% str_trim()
  return(x)
}


extrairAnuncios <- function(url_pagina, info_adicional = T) {
  ### INPUTS:
  # url_pagina: url de uma pagina do olx com uma lista de links de anúncios.
  # info_adicional: variavel booleana. se verdadeiro, faz o scraping de dados adicionais do anuncio
  # ... deve ser usado apenas para apartamentos, pois a sintaxe do html para quartos é diferente
  mycurl <- curl(url_pagina, handle = curl::new_handle("useragent" = "Mozilla/5.0"))
  mycurl <- read_html(mycurl)
  
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
  precos[precos == "character(0)"] = 0
  precos %<>% unlist()
  precos %<>% str_squish() 
  precos %<>% limparString()
  precos %<>% as.numeric()
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
  
  dados <- data.frame(link = col_links,
             titulo = col_titles,
             preco = col_precos,
             bairro = col_bairros,
             # adicional = col_adicionais,
             stringsAsFactors = FALSE)
  
  
  if (info_adicional) {
    adicional <- mycurl %>% html_nodes(".detail-specific") %>% html_text()
    adicional %<>% str_replace_all("[\t]", "")
    adicional %<>% str_replace_all("[\n]", "")
    col_adicionais <- adicional
    dados$adicional <- col_adicionais
    
    # COLUNA DE QUANTIDADE DE QUARTOS
    # Quarto: pegar posicao inicial e final do string quarto
    # Localizar trecho dentro do string referente a quartos
    matriz_posicao <- str_locate(dados$adicional, "quarto")
    # Voltar 2 posições no string para pegar o número (ex: 2 quarto)
    matriz_posicao[,1] <- matriz_posicao[,1] - 2
    # extrair string com posições iniciais e finais
    vetor_quartos <- str_sub(dados$adicional, matriz_posicao[,1], matriz_posicao[,2])
    # extrair apenas número (primeiro caractere do string) e converter para numeric
    vetor_quartos <- str_sub(vetor_quartos, 1, 1)
    vetor_quartos %<>% as.numeric()
    # adicionar ao data frame
    dados$qtd_quarto <- vetor_quartos
    
    
    # Condominio
    # retirar cifrao pra ficar mais facil
    dados$adicional %<>% str_replace_all("\\$", "S")
    matriz_posicao <- str_locate(dados$adicional, "Condomínio: RS ")
    # mover cinco posicoes para pegar algarismos após o RS
    vetor_taxa <- str_sub(dados$adicional, matriz_posicao[, 2], matriz_posicao[, 2] + 4)
    # extrair apenas numeros
    vetor_taxa %<>% parse_number()
    # vendo se funcionou
    # data.frame(dados$adicional, vetor_taxa) %>% head(20)
    
    # Funcionou! Incorporar vetor ao data frame
    dados$taxa_condominio <- vetor_taxa
    
    
    # Área
    matriz_posicao <- str_locate(dados$adicional, " m²")
    # voltar quatro posições
    vetor_area <- str_sub(dados$adicional, matriz_posicao[,1] - 4, matriz_posicao[, 1])
    # converter para numerico
    vetor_area %<>% parse_number()
    # vendo se funcionou
    # data.frame(dados$adicional, vetor_area) %>% head(20)
    
    
    # Funcionou! Incorporar ao data frame
    dados$area_condominio <- vetor_area
    
    
    # Garagem
    matriz_posicao <- str_locate(dados$adicional, " vaga")
    # voltar quatro posições
    vetor_garagem <- str_sub(dados$adicional, matriz_posicao[,1] - 2, matriz_posicao[, 1])
    # converter para numerico
    vetor_garagem %<>% readr::parse_number()
    # vendo se funcionou
    # data.frame(dados$adicional, vetor_garagem) %>% head(20)
    
    
    # Funcionou! Incorporar ao data frame
    dados$garagem <- vetor_garagem
    
    # Remover objetos desnecessários
    # rm(matriz_posicao, vetor_adicional, vetor_area, vetor_garagem, vetor_quartos, vetor_taxa)
      
  }
  
  return(dados)
}


# definir função para limpar strings coletadas
limparString <- function(x) {
  # x = string coletado do olx
  x %<>% str_replace_all("[\t]", "")
  x %<>% str_replace_all("[\n]", "")
  x %<>% str_replace_all("Apartamentos", "")
  x %<>% str_replace_all("Anúncio Profissional", "")
  x %<>% str_replace("-", "")
  x %<>% str_replace_all("[R$]", "")
  x %<>% str_replace_all("[.]", "")
  x %<>% str_trim()
  return(x)
}


extrairCEP <- function(url) {
  # url = url de um quarto
  mycurl <- curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0"))
  url <- read_html(mycurl, encoding = "ISO8859-1")
  #url <- read_html(url, encoding = "ISO8859-1")
  #url <- html_nodes(url, ".OLXad-location-map") deprecated
  
  # if clause para pegar casos em que o node id é diferente
  if (length(html_nodes(url, ".OLXad-location-map")) > 0) {
    url %<>% html_nodes(".OLXad-location-map")
  } else {
    url %<>% html_nodes(".OLXad-location")
  }
  
  url <- html_nodes(url, "p")
  url <- url[2]
  url <- html_text(url)
  cep <- limparString(url)
  cep <- readr::parse_number(cep)
  return(cep)
}

postal<-function(cep){
  # converter cep em endereço
  library(httr)
  l<-list()
  for(i in seq_along(cep)){
    cep <- stringr::str_replace(cep,"\\D","")
    cep <- stringr::str_pad(cep,8,side="left",pad="0")
    cep <- as.character(cep)
    url <- paste0("http://correiosapi.apphb.com/cep/",cep)
    a <- GET(url[i])
    b <- content(a,as="parsed")
    l[[i]] <- b
  }
  x <- as.data.frame(do.call("rbind",l))
  for (col in 1:ncol(x)) {x[, col] <- as.character(x[, col])}
  return(x)
}

