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

url_teste <- lista_urls[1]
# system.time(df <- extrairAnuncios(url_teste, info_adicional = TRUE))

mycurl <- curl(url_teste, handle = curl::new_handle("useragent" = "Mozilla/5.0"))
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

adicional <- mycurl %>% html_nodes(".detail-specific") %>% html_text()
adicional %<>% str_replace_all("[\t]", "")
adicional %<>% str_replace_all("[\n]", "")
col_adicionais <- adicional

dados <- data.frame(link = col_links,
           titulo = col_titles,
           preco = col_precos,
           bairro = col_bairros,
           adicional = col_adicionais,
          stringsAsFactors = FALSE)

# dados %<>% filter(str_detect(bairro, "Niterói") | str_detect(bairro, "Rio de Janeiro"))
dados %<>% separate(bairro, c("cidade", "bairro"), sep = ",")

dados$adicional %<>% str_replace_all("quartos", "quarto")
dados %<>% mutate(
  tem_quarto = str_detect(adicional, "quarto"),
  tem_area = str_detect(adicional, "m²"),
  tem_taxa = str_detect(adicional, "Condomínio"),
  tem_garagem = str_detect(adicional, "vaga")
)




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
data.frame(dados$adicional, vetor_taxa) %>% head(20)


# Funcionou! Incorporar vetor ao data frame
dados$taxa_condominio <- vetor_taxa


# Área
matriz_posicao <- str_locate(dados$adicional, " m²")
# voltar quatro posições
vetor_area <- str_sub(dados$adicional, matriz_posicao[,1] - 4, matriz_posicao[, 1])
# converter para numerico
vetor_area %<>% parse_number()
# vendo se funcionou
data.frame(dados$adicional, vetor_area) %>% head(20)


# Funcionou! Incorporar ao data frame
dados$area_condominio <- vetor_area


# Garagem
matriz_posicao <- str_locate(dados$adicional, " vaga")
# voltar quatro posições
vetor_garagem <- str_sub(dados$adicional, matriz_posicao[,1] - 2, matriz_posicao[, 1])
# converter para numerico
vetor_garagem %<>% readr::parse_number()
# vendo se funcionou
data.frame(dados$adicional, vetor_garagem) %>% head(20)


# Funcionou! Incorporar ao data frame
dados$garagem <- vetor_garagem

# Remover objetos desnecessários
rm(matriz_posicao, vetor_adicional, vetor_area, vetor_garagem, vetor_quartos, vetor_taxa)

# df %<>% filter(str_detect(bairro, "Niterói") | str_detect(bairro, "Rio de Janeiro"))
dados_full %<>% separate(bairro, c("cidade", "bairro"), sep = ",")

lista_dados <- NULL
for (i in seq_along(lista_urls)) {
  print(paste0("Estamos na pagina ", i, ", faltam ", max(seq_along(lista_urls)) - i))
  lista_dados[[i]] <- extrairAnuncios(lista_urls[i])
  
}

dados_full = do.call("rbind", lista_dados)







