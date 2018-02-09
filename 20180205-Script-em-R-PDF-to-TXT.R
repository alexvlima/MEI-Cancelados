
# Biblioteca para transformar pdf para txt
# install.packages("pdftools")
# install.packages("stringr")
library(pdftools)
library(stringr)

# Para considerar numero ate 14 digitos
options("scipen" = 14)

# Download do arquivo
download.file(url = "http://idg.receita.fazenda.gov.br/noticias/ascom/2018/fevereiro/receita-divulga-relacao-de-baixa-de-cnpjs-do-mei/relacaoinscricoesbaixadascnpj-ade01-2018.pdf",
              "2018-02-05-MEI-Cancelado.pdf", mode="wb")

# Funcao para retirar espaco
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

# Funcao para verificar se o CNPJ e valido
cnpjValido <- function(cnpj){
  temp <- cnpj %/% 100
  prefix <- integer(12)
  for(i in 10^(11:0)){
    prefix[12 - log10(i)] <- temp %/% i
    temp <- temp %% i
  }
  prefix <- c(prefix, ifelse(sum(prefix * c(5:2,9:2)) %% 11 < 2, 0, 11 - sum(prefix * c(5:2,9:2)) %% 11))
  prefix <- c(prefix, ifelse(sum(prefix * c(6:2,9:2)) %% 11 < 2, 0, 11 - sum(prefix * c(6:2,9:2)) %% 11))
  prefix <- sum(prefix * 10^(13:0))
  return(ifelse(cnpj == prefix, 1, 0))
}


# Tratamento
txt <- pdf_text("2018-02-05-MEI-Cancelado.pdf")

temp <- strsplit(txt, "\r")

temp1 <- unlist(temp)
head(temp1)

temp1 <- temp1[-1]
head(temp1)

temp2<-gsub("\n","",temp1)
head(temp2)

temp3 <- strsplit(temp2," ")
head(temp3)

temp4 <- unlist(temp3)
head(temp4)

base <- as.data.frame(temp4)

names(base)[names(base)=='temp4'] <- 'CNPJ'
base$CNPJ <- trim(base$CNPJ)
cnpjLimpo <- base$CNPJ
cnpjLimpo <-gsub("[.]", "", cnpjLimpo)
cnpjLimpo <-gsub("[-]", "", cnpjLimpo)
cnpjLimpo <-gsub("[/]", "", cnpjLimpo)
cnpjLimpo <- as.numeric(str_extract(cnpjLimpo,"[[:digit:]]+"))
base <- transform(base, NR_CNPJ = cnpjLimpo)

# Verificar CNPJ
i=1
total = nrow(base)
pb <- txtProgressBar(min = 0, max = total, style = 3); cat('\n')
valido<-matrix(ncol=1,nrow=total)
for(i in 1:total){
  valido[i]=cnpjValido(base$NR_CNPJ[i])
  setTxtProgressBar(pb, i)
}; cat('\n')
table(valido)
# base <- transform(base, valido=valido)

write.table(base, file = "20180205-MEI-cancelados.txt", col.names = TRUE, sep = '\t', dec = '.', row.names = FALSE, quote = FALSE, na = '')

