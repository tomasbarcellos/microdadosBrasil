context('Todos os links dos metadados ainda existem')

gera_arquivo <- function(dataset) {
  system.file('extdata', dataset, paste0(dataset, '_files_metadata_harmonization.csv'), package = 'microdadosBrasil')
}

metadados <- list(
  gera_arquivo('CAGED'),
  gera_arquivo('CENSO'),
  gera_arquivo('CensoEducacaoSuperior'),
  gera_arquivo('CensoEscolar'),
  gera_arquivo('ENEM'),
  gera_arquivo('PME'),
  gera_arquivo('PNAD'),
  gera_arquivo('PNADcontinua'),
  gera_arquivo('PNS'),
  gera_arquivo('POF'),
  gera_arquivo('RAIS')
  )

tabelas <- lapply(metadados, function(x) {
  data.table::fread(x)[['download_path']]
  }) %>% unlist()

# dois casos de teste: http e ftp
ftp <- tabelas[grep('ftp://', tabelas)]
http <- tabelas[grep('https?://', tabelas)]

teste_ftp <- function(caminho) {
  if (length(caminho) > 1) return(sapply(caminho, teste_ftp))

  barras <- stringr::str_locate_all(caminho, '/')[[1]][, 1]

  if (nchar(caminho) %in% barras) { # ultimo char = '/'?
    arquivos <- RCurl::getURL(caminho, dirlistonly = TRUE) %>%
      strsplit('\\n') %>% `[[`(1) %>% gsub(pattern = '\\r', replacement = '')
    # sem erro na leitura da pasta significa OK
    TRUE
  } else {
    url <- substr(caminho, 1, barras[length(barras)])
    arquivos <- RCurl::getURL(url, dirlistonly = TRUE) %>%
      strsplit('\\n') %>% `[[`(1) %>% gsub(pattern = '\\r', replacement = '')

    caminho %in% paste0(url, arquivos)
  }
}

expect_tudo_true <- function(x, FUN) {
  expect_identical(FUN(x), rep(TRUE, length(x)))
}

test_that('Arquivos hospedados em ftp existem', {
  expect_tudo_true(tabelas[[1]], teste_ftp) # CAGED
  expect_tudo_true(tabelas[[2]], teste_ftp) # CENSO
  expect_tudo_true(tabelas[[6]], teste_ftp) # PME
  expect_tudo_true(tabelas[[7]], teste_ftp) # PNAD
  expect_tudo_true(tabelas[[8]], teste_ftp) # PNAD-C
  expect_tudo_true(tabelas[[9]], teste_ftp) # PNS
  expect_tudo_true(tabelas[[10]], teste_ftp) # POF
  expect_tudo_true(tabelas[[11]], teste_ftp) # RAIS
})

teste_http <- function(caminho) {
  if (length(caminho) > 1) return(sapply(teste_http, caminho))
  !httr::http_error(caminho)
}

test_that('Arquivos hospedados em http existem', {
  expect_tudo_true(tabelas[[3]], teste_ftp) # Edu Sup
  expect_tudo_true(tabelas[[4]], teste_ftp) # Censo Esc
  expect_tudo_true(tabelas[[5]], teste_ftp) # ENEM
})

