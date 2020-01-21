extrairAnuncios <- function(url_pagina, info_adicional) {
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
  
  if (info_adicional) {
    adicional <- mycurl %>% html_nodes(".detail-specific") %>% html_text()
    adicional %<>% str_replace_all("[\t]", "")
    adicional %<>% str_replace_all("[\n]", "")
    col_adicionais <- adicional
    
  }
  return(data.frame(link = col_links,
                    titulo = col_titles,
                    preco = col_precos,
                    bairro = col_bairros,
                    adicional = col_adicionais,
                    stringsAsFactors = FALSE))
}
