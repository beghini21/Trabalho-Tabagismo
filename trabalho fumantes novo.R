#Tabagismo e Pobreza no Brasil: uma analise do perfil da populacao tabagista a partir da pof 2017-2018

#---- 1 - Pacotes Utilizados ----------
library('rio')
library('tidyverse')
library('dplyr')
library('DT')

#---- 2 - Estabelecer área de Trabalho ----
setwd("C:\\Users\\vhbeg\\Desktop\\Trabalho Fumantes 2.0")

#--------- 3 - importar dados - "Leitura dos microdados R.R" -----
MORADOR <- 
  read.fwf("C:\\Users\\vhbeg\\Desktop\\Trabalho Fumantes 2.0\\MORADOR.txt" 
           , widths = c(2,4,1,9,2,1,2,2,1,2,2,4,3,1,1,
                        1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,2,1,1,2,1,1,2,1,1,1,
                        2,1,2,14,14,10)
           , na.strings=c(" ")
           , col.names = c("uf", "estrato_pof", "tipo_situacao_reg",
                           "cod_upa", "num_dom", "num_uc",
                           "cod_informante", "V0306", "V0401",
                           "V04021", "V04022", "V04023", "V0403",
                           "V0404", "V0405", "V0406", "V0407",
                           "V0408", "V0409", "V0410", "V0411",
                           "V0412", "V0413", "V0414", "V0415",
                           "V0416", "V041711", "V041712", "V041721",
                           "V041722", "V041731", "V041732","V041741",
                           "V041742", "V0418", "V0419", "V0420",
                           "V0421", "V0422", "V0423", "V0424",
                           "V0425", "V0426", "V0427", "V0428",
                           "V0429", "V0430", "ANOS_ESTUDO","PESO", 
                           "PESO_FINAL", "RENDA_TOTAL")
           , dec="."
  )
despesa_individual <- 
  read.fwf("DESPESA_INDIVIDUAL.txt" 
           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2
                        ,2,1,1,1,12,10,1,2,14,14,10)
           , na.strings=c(" ")
           , col.names = c("uf", "estrato_pof", "tipo_situacao_reg",
                           "cod_upa", "num_dom", "num_uc",
                           "cod_informante", "quadro", "seq", "v9001",
                           "v9002", "v8000", "v9010", "v9011", "v9012",
                           "v4104", "v4105", "deflator", "v8000_defla",
                           "cod_imput_valor", "fator_anualizacao",
                           "peso", "peso_final", "renda_total"
           )
           , dec="."
  )  
#Salvar dados .rds
export(MORADOR,'morador.rds')
export(despesa_individual, 'despesa_individual.rds')

#---- 4 Separando dados -----
# Importar dados
MORADOR <- import('morador.rds')
despesa_individual <- import('despesa_individual.rds')

# Colunas e produtos para o trabalho
colunas_filter <- c("V0403", "V0404", "V0405", "ANOS_ESTUDO", "renda_total", "v9001", "v8000", "cod_upa")
produtos_tabagismo <- c('2100101','2100201','2100301','2100401','2100402','2100501','2100601','2100701','2100801','2100901','2101001','2101101','2101201','2101301','2101401','2101501')

#Unir dados
mdi <- inner_join(MORADOR, despesa_individual)
mdi_distinct <-tibble(mdi) %>%
  dplyr::select(colunas_filter) %>%
  distinct(cod_upa, .keep_all = TRUE)

#Populacao tabagista
mdi_fumantes <- mdi_distinct %>%
  dplyr::filter(mdi_distinct$v9001 %in% produtos_tabagismo)

#Resto da Populacao
resto_populacao <- mdi_distinct %>%
  dplyr::filter(!(mdi_distinct$v9001 %in% produtos_tabagismo))

#---- tabela 1.1 - Fumantes ----------------------------------------------------
#Criar coluna condicional para agrupar a idades a populacao fumante
mdi_fumantes_idade <- mutate(mdi_fumantes, Idade= case_when(
  mdi_fumantes$V0403 < 18 ~ "De 14 a 17",
  mdi_fumantes$V0403 > 17 & mdi_fumantes$V0403 < 31 ~ "18 a 30",
  mdi_fumantes$V0403 > 30 & mdi_fumantes$V0403< 46 ~ "31 a 45",
  mdi_fumantes$V0403 > 45 & mdi_fumantes$V0403 <61 ~ "46 a 60",
  mdi_fumantes$V0403 > 60 & mdi_fumantes$V0403 < 76 ~ "61 a 75",
  mdi_fumantes$V0403 > 75 ~ "75 e mais")
)

#idade e sexo group_by
mdi_idade_sexo <- mdi_fumantes_idade %>%
  group_by(Idade,V0404) %>%
  count()

#Organizando base de dados
fumantes_t1 <-pivot_wider(mdi_idade_sexo, names_from = V0404, values_from = n)
colnames(fumantes_t1) <- c('Idade','Masculino_T','Feminino_T')
fumantes_t1$Total_T <- fumantes_t1$Masculino_T + fumantes_t1$Feminino_T
fumantes_t1$Masculino_T <- round(fumantes_t1$Masculino_T *100/nrow(mdi_fumantes),2)
fumantes_t1$Feminino_T <- round(fumantes_t1$Feminino_T *100/nrow(mdi_fumantes),2)
fumantes_t1$Total_T <- round(fumantes_t1$Total_T *100/nrow(mdi_fumantes),2)

#---- tabela 1.2 - Não Fumantes ----------------------------------------------------
#Criar coluna condicional para agrupar a idades a populacao fumante
mdi_nf_idade <- mutate(resto_populacao, Idade= case_when(
  resto_populacao$V0403 < 18 ~ "De 14 a 17",
  resto_populacao$V0403 > 17 & resto_populacao$V0403 < 31 ~ "18 a 30",
  resto_populacao$V0403 > 30 & resto_populacao$V0403< 46 ~ "31 a 45",
  resto_populacao$V0403 > 45 & resto_populacao$V0403 <61 ~ "46 a 60",
  resto_populacao$V0403 > 60 & resto_populacao$V0403 < 76 ~ "61 a 75",
  resto_populacao$V0403 > 75 ~ "75 e mais")
)

#idade e sexo group_by
mdi_idade_sexo_nf <- mdi_nf_idade %>%
  group_by(Idade,V0404) %>%
  count()

#Organizando base de dados
nfumantes_t1 <-pivot_wider(mdi_idade_sexo_nf, names_from = V0404, values_from = n)
colnames(nfumantes_t1) <- c('Idade','Masculino_NT','Feminino_NT')
nfumantes_t1$Total_NT <- nfumantes_t1$Masculino_NT + nfumantes_t1$Feminino_NT
nfumantes_t1$Masculino_NT <- round(nfumantes_t1$Masculino_NT *100/nrow(resto_populacao),2)
nfumantes_t1$Feminino_NT <- round(nfumantes_t1$Feminino_NT *100/nrow(resto_populacao),2)
nfumantes_t1$Total_NT <- round(nfumantes_t1$Total_NT *100/nrow(resto_populacao),2)

#---- tabela 1.3 - FINAL ------
#Unir tabelas
tabela_1 <- full_join(fumantes_t1,nfumantes_t1)
tabela_1[is.na(tabela_1)] = 0
tabela_1_final <- datatable(tabela_1)


#----- tabela 2.1 - fumantes -------
#coluna condicional para distribuir a populacao fumante por anos de estudo, e idade que ja estava contida no data frame
mdi_fumantes_ie <- mutate(mdi_fumantes_idade, Estudo = case_when(
  mdi_fumantes_idade$ANOS_ESTUDO == 0 ~ "Zero",
  mdi_fumantes_idade$ANOS_ESTUDO > 0 & mdi_fumantes_idade$ANOS_ESTUDO < 5 ~ "1 a 4",
  mdi_fumantes_idade$ANOS_ESTUDO > 4 & mdi_fumantes_idade$ANOS_ESTUDO< 8 ~ "5 a 7",
  mdi_fumantes_idade$ANOS_ESTUDO == 8 ~ "8",
  mdi_fumantes_idade$ANOS_ESTUDO > 8 & mdi_fumantes_idade$ANOS_ESTUDO < 12 ~ "9 a 11",
  mdi_fumantes_idade$ANOS_ESTUDO > 11 ~ "12 ou mais"
))

#idade e sexo group_by
mdi_idade_estudo <- mdi_fumantes_ie %>%
  group_by(Estudo, Idade) %>%
  count()

#colocar em percentual 
mdi_idade_estudo$n <- round(mdi_idade_estudo$n *100/nrow(mdi_fumantes),2)

#"alongando o tibble"
fumantes_t2 <- pivot_wider(mdi_idade_estudo,names_from = Idade, values_from = n)
fumantes_t2$condição <- "Tabagista"

#----- tabela 2.2 -  Não fumantes -------

#coluna condicional para distribuir a populacao fumante por anos de estudo, e idade que ja estava contida no data frame
mdi_nfumantes_ie <- mutate(mdi_nf_idade, Estudo = case_when(
  mdi_nf_idade$ANOS_ESTUDO == 0 ~ "Zero",
  mdi_nf_idade$ANOS_ESTUDO > 0 & mdi_nf_idade$ANOS_ESTUDO < 5 ~ "1 a 4",
  mdi_nf_idade$ANOS_ESTUDO > 4 & mdi_nf_idade$ANOS_ESTUDO< 8 ~ "5 a 7",
  mdi_nf_idade$ANOS_ESTUDO == 8 ~ "8",
  mdi_nf_idade$ANOS_ESTUDO > 8 & mdi_nf_idade$ANOS_ESTUDO < 12 ~ "9 a 11",
  mdi_nf_idade$ANOS_ESTUDO > 11 ~ "12 ou mais"
))

#idade e sexo group_by
mdi_idade_estudo_nf <- mdi_nfumantes_ie %>%
  group_by(Estudo, idade) %>%
  count()

#colocar em percentual 
mdi_idade_estudo_nf$n <- round(mdi_idade_estudo_nf$n *100/nrow(resto_populacao),2)

#"alongando o tibble"
nfumantes_t2 <- pivot_wider(mdi_idade_estudo_nf,names_from = Idade, values_from = n)
nfumantes_t2$condição <- "Não Tabagista"

#----- tabela 2.3 - Final --------
tabela_2 <- full_join(fumantes_t2,nfumantes_t2)
tabela_2[is.na(tabela_2)] = 0
tabela_2$Total <- tabela_2$Total <- tabela_2$`De 14 a 17` + tabela_2$`18 a 30` + tabela_2$`31 a 45` + tabela_2$`46 a 60` + tabela_2$`61 a 75` + tabela_2$`75 e mais`
tabela_2_ord <- tabela_2[,c(1,7,8,2,3,4,5,6,9)]
tabela_2_final <- datatable(tabela_2_ord)

#----- tabela 3 ------
#Coluna condicional para distribuir tibble em anos de estudo
p_estudo <- mutate(mdi_distinct, Estudo = case_when(
  mdi_distinct$ANOS_ESTUDO == 0 ~ "Zero",
  mdi_distinct$ANOS_ESTUDO > 0 & mdi_distinct$ANOS_ESTUDO < 5 ~ "1 a 4",
  mdi_distinct$ANOS_ESTUDO > 4 & mdi_distinct$ANOS_ESTUDO< 9 ~ "5 a 8",
  mdi_distinct$ANOS_ESTUDO > 8 & mdi_distinct$ANOS_ESTUDO < 12 ~ "9 a 11",
  mdi_distinct$ANOS_ESTUDO > 11 & mdi_distinct$ANOS_ESTUDO < 15 ~ "12 a 14",
  mdi_distinct$ANOS_ESTUDO > 14 ~ "15 ou +",
))
#coluna condicional para separa a populacao tabagista da nao tabagista
pt_estudo <- mutate(p_estudo, Condicao = case_when(
  p_estudo$v9001 %in% produtos_tabagismo ~ "Tabagista",
  !(p_estudo$v9001 %in% produtos_tabagismo) ~ "Não tabagista"
))

#agrupando os dados pela faixa de estudo e pela condicao de tabagista e nao tabagista
pt_estudo_ag <- pt_estudo %>%
  group_by(Estudo,Condicao) %>%
  count()

#"Alongando tibble"
tabela_3 <- pivot_wider(pt_estudo_ag, names_from = Estudo, values_from = n)
tabela_3_total <- colSums(tabela_3[,c(2:7)])
tabela_3_total$Condicao <- "Total"
tabela_3_rbind <- rbind(tabela_3,tabela_3_total)
tabela_3_final <- datatable(tabela_3_rbind[,c(1,7,2,5,6,3,4)])

#Resultado
tabela_3_final

#----- tabela 4 --------
pt_etnia <- mutate(pt_estudo, etnia = case_when(
  pt_estudo$V0405 == 1 ~ "Branca",
  pt_estudo$V0405 == 2 ~ "Preta" ,
  pt_estudo$V0405 == 3 ~ "Amarela",
  pt_estudo$V0405 == 4 ~ "Parda",
  pt_estudo$V0405 == 5 ~ "Indigena",
  pt_estudo$V0405 == 9 ~ "Sem declaracao",
))

#agrupando por condicao e etnia
pt_etnia_ag <- pt_etnia %>%
  group_by(Condicao, etnia) %>%
  count()

#alongar e adicionando total a base de dados
tabela_4 <- pivot_wider(pt_etnia_ag, names_from = etnia, values_from = n)
tabela_4[is.na(tabela_4)] = 0
tabela_4_total <- colSums(tabela_4[,c(2:7)])
tabela_4_total$Condicao <- "Total"
tabela_4_final <- rbind(tabela_4,tabela_4_total)

#finalizando tabela
tabela_4_final$Total <- tabela_4_final$Amarela + tabela_4_final$Branca + tabela_4_final$Indigena + tabela_4_final$Parda + tabela_4_final$Preta + tabela_4_final$`Sem declaracao`
tabela_4_total1 <- as.numeric(tabela_4_final[3,])
tabela_4_final$Branca <- round(tabela_4_final$Branca * 100/ tabela_4_total1[3],2)
tabela_4_final$Preta <- round(tabela_4_final$Preta * 100/ tabela_4_total1[6],2)
tabela_4_final$Amarela <- round(tabela_4_final$Amarela * 100/ tabela_4_total1[2],2)
tabela_4_final$Parda <- round(tabela_4_final$Parda * 100/ tabela_4_total1[5],2)
tabela_4_final$Indigena <- round(tabela_4_final$Indigena * 100/ tabela_4_total1[4],2)
tabela_4_final$`Sem declaracao` <- round(tabela_4_final$`Sem declaracao` * 100/ tabela_4_total1[7],2)
tabela_4_final$Total <- round(tabela_4_final$Total * 100/ tabela_4_total1[8],2)

#Resultado
tabela_4_fim <- datatable(tabela_4_final)
tabela_4_fim
