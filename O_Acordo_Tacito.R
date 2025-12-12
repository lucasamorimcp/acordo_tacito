
#Baixando survey

#Para acesso ao dataframe: https://www.cesop.unicamp.br/eng/banco_de_dados/v/4680
library(haven)
ESEB2022 <- read_sav("ESEB2022.sav")

#Parte 1 Resultados - Analise descritiva sobre o aborto

#Avaliando a variavel de aborto

attributes(ESEB2022$Q31_7)
table(ESEB2022$Q31_7)
prop.table(table(ESEB2022$Q31_7))

#Grafico 5 - Descritivo aborto

library(dplyr)
library(ggplot2)

df_freq_aborto <- as.data.frame(prop.table(table(ESEB2022$Q31_7))) %>%
  rename(
    valor = Var1,
    freq  = Freq
  )

df_freq_aborto$valor <- factor(
  df_freq_aborto$valor,
  levels = c("1", "2", "3", "97", "98"),
  labels = c("A favor", "Contra", "Depende", "Não sabe", "Não respondeu")
)

library(scales)

ggplot(df_freq_aborto, aes(x = valor, y = freq)) +
  geom_col(fill = "white", colour = "black") +
  geom_text(
    aes(label = percent(freq, accuracy = 0.1)),
    vjust = -0.4,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.1))  
  ) +
  labs(
    x = NULL,
    y = "Proporção",
    title = ""
  ) +
  theme_bw()

ggsave(
  filename = "aborto_ESEB2022_distribuicao.png",
  plot     = last_plot(),
  width    = 8,    
  height   = 5,    
  dpi      = 300,
  bg       = "white"
)

#Possui 3 categorias: A favor (1), Contra (2), Depende (3)

ESEB2022$report_aborto <- ifelse(ESEB2022$Q31_7 > 3,NA,ESEB2022$Q31_7) #Só para descritiva

ESEB2022$aborto <- ifelse(ESEB2022$Q31_7 > 3,NA,ESEB2022$Q31_7) #Excluindo NS e NR
ESEB2022$aborto <- ifelse(ESEB2022$aborto == 2,0,1) #Binarizando, quem e contra (0) vs quem e A favor ou Depende (1)

table(ESEB2022$aborto)
prop.table(table(ESEB2022$aborto))

#Grafico 6 - variavel recodificada

df_freq_aborto_rec <- as.data.frame(prop.table(table(ESEB2022$aborto))) %>%
  rename(
    valor = Var1,
    freq  = Freq
  )

df_freq_aborto_rec$valor <- factor(
  df_freq_aborto_rec$valor,
  levels = c("0", "1"),
  labels = c("Oposição ao aborto", "Não oposição ao aborto")
)

ggplot(df_freq_aborto_rec, aes(x = valor, y = freq)) +
  geom_col(fill = "white", colour = "black") +
  geom_text(
    aes(label = percent(freq, accuracy = 0.1)),
    vjust = -0.4,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = NULL,
    y = "Proporção",
    title = ""
  ) +
  theme_bw()

ggsave(
  filename = "aborto_ESEB2022_dicotomica.png",
  plot     = last_plot(),
  width    = 8,
  height   = 5,
  dpi      = 300,
  bg       = "white"
)

#Primeiro, olhando para os determinantes do aborto

#Recodificando variaveis para rodar modelo de regressao

#Sexo

attributes(ESEB2022$D02)
table(ESEB2022$D02)
prop.table(table(ESEB2022$D02))

#Criando variavel binaria ser homem

ESEB2022$homem <- ifelse(ESEB2022$D02 == 1,1,0)

table(ESEB2022$homem)
prop.table(table(ESEB2022$homem))

#Escolarizacao

attributes(ESEB2022$D03)
table(ESEB2022$D03)
prop.table(table(ESEB2022$D03))

ESEB2022$escolarizacao <- ifelse(ESEB2022$D03 > 10,NA,ESEB2022$D03) #retirando NR
prop.table(table(ESEB2022$escolarizacao))

#Idade

attributes(ESEB2022$T_D01A_3)
table(ESEB2022$T_D01A_3)

ESEB2022$idade <- 2025 - ESEB2022$T_D01A_3

#Classe

attributes(ESEB2022$D07a)
table(ESEB2022$D07a)
prop.table(table(ESEB2022$D07a))
class(ESEB2022$D07a)

ESEB2022$classe <- ifelse(ESEB2022$D07a > 3,NA,ESEB2022$D07a) #retirando NS e NR

ESEB2022$classe <- as.factor(ESEB2022$classe) #transformando em um fator
prop.table(table(ESEB2022$classe))

#Renda

attributes(ESEB2022$D09_RENDAF)
table(ESEB2022$D09_RENDAF)
prop.table(table(ESEB2022$D09_RENDAF))

ESEB2022$renda <- ifelse(ESEB2022$D09_RENDAF > 171200, NA,ESEB2022$D09_RENDAF) #retirando NS e NR

class(ESEB2022$renda)

#Religiao

attributes(ESEB2022$D10)
table(ESEB2022$D10)
prop.table(table(ESEB2022$D10))

library(dplyr)

#Recodificando varivel religiao

ESEB2022$religiao <- dplyr::case_when(
  ESEB2022$D10 == 3 ~ 1, #Catolicos
  ESEB2022$D10 == 5 ~ 2, #Evangelicos
  ESEB2022$D10 == 97 ~ 3, #Sem religiao
  ESEB2022$D10 %in% c(1, 2, 4, 6, 7, 9, 10, 96, 100, 101, 102) ~ 4, #Outras
  ESEB2022$D10 %in% c(98, 99) ~ NA_real_, #NS e NR
  TRUE ~ NA_real_
)

ESEB2022$religiao <- as.factor(ESEB2022$religiao) #transformando em fator
table(ESEB2022$religiao)
prop.table(table(ESEB2022$religiao))

#Religiosidade

attributes(ESEB2022$D11)
table(ESEB2022$D11)
prop.table(table(ESEB2022$D11))

ESEB2022$religiosidade <- ifelse(ESEB2022$D11 > 6,NA,ESEB2022$D11) #retirando NS e NR

ESEB2022$religiosidade <- as.numeric(ESEB2022$religiosidade) #transformando em numerica

ESEB2022$religiosidade <- 7 - ESEB2022$religiosidade #invertendo escala

prop.table(table(ESEB2022$religiosidade))
#Interesse por politica

attributes(ESEB2022$Q01)
table(ESEB2022$Q01)
prop.table(table(ESEB2022$Q01))

ESEB2022$interesse <- ifelse(ESEB2022$Q01 > 4,NA,ESEB2022$Q01) #retirando NS e NR
ESEB2022$interesse <- 5 - ESEB2022$interesse #invertendo escala

ESEB2022$interesse <- as.numeric(ESEB2022$interesse) #transformando em numerica

prop.table(table(ESEB2022$interesse))

#Se autodeclarar de direita

attributes(ESEB2022$Q19)
table(ESEB2022$Q19)
prop.table(table(ESEB2022$Q19))

ESEB2022$direita <- ifelse(ESEB2022$Q19 > 10,NA,ESEB2022$Q19)

#Modelo de regressao

ESEB2022$contra_aborto <- ESEB2022$aborto * (-1) #invertendo escala pois escala original tem favoravel ao aborto com positivo

summary(lm(contra_aborto ~ homem+escolarizacao+idade+classe+renda+religiao+religiosidade+
             interesse+direita, data = ESEB2022))

#Grafico 1 - Determinantes do Aborto

#Transformando modelo em um objeto

modelo_aborto <- lm(contra_aborto ~ homem + escolarizacao + idade + classe +
                      renda + religiao + religiosidade + interesse + direita,
                    data = ESEB2022)

library(broom)
library(dplyr)
library(ggplot2)

#Extraindo coeficeintes

coef_aborto <- tidy(modelo_aborto, conf.int = TRUE) |>
  filter(term != "(Intercept)")

#Criando uma matrix de modelo de desvio padrao para padronizar os outputs

X <- model.matrix(modelo_aborto)[, -1] #tira o intercepto
y <- model.response(model.frame(modelo_aborto))

sd_y <- sd(y, na.rm = TRUE)
sd_x <- apply(X, 2, sd, na.rm = TRUE) #um desvio-padrao por preditor

#garante que os nomes batem com `term`
sd_x <- sd_x[coef_aborto$term]

#Transformar tudo em coeficiente padronizado (incluindo IC)

coef_aborto <- coef_aborto |>
  mutate(
    sd_x = sd_x,
    sd_y = sd_y,
    estimate  = estimate  * sd_x / sd_y,
    conf.low  = conf.low  * sd_x / sd_y,
    conf.high = conf.high * sd_x / sd_y,
    signif = ifelse(p.value < 0.05, "Significativo", "Nao significativo")
  )

#Renomeando rotulos das variaveis para o grafico

coef_aborto <- coef_aborto |>
  mutate(term = dplyr::recode(term,
                              "homem"         = "Homem",
                              "escolarizacao" = "Escolarizacao",
                              "idade"         = "Idade",
                              "classe2"       = "Segundo Setor (ref: Primeiro Setor)",
                              "classe3"       = "Terceiro Setor (ref: Primeiro Setor)",
                              "renda"         = "Renda",
                              "religiao2"     = "Evangelicos (ref: Catolicos)",
                              "religiao3"     = "Sem Religiao (ref: Catolicos)",
                              "religiao4"     = "Outras Religioes (ref: Catolicos)",
                              "religiosidade" = "Religiosidade",
                              "interesse"     = "Interesse por Politica",
                              "direita"       = "Autoposicao a Direita"
  )) |>
  mutate(term = factor(
    term,
    levels = c(
      "Homem",
      "Escolarizacao",
      "Idade",
      "Segundo Setor (ref: Primeiro Setor)",
      "Terceiro Setor (ref: Primeiro Setor)",
      "Renda",
      "Evangelicos (ref: Catolicos)",
      "Sem Religiao (ref: Catolicos)",
      "Outras Religioes (ref: Catolicos)",
      "Religiosidade",
      "Interesse por Politica",
      "Autoposicao a Direita"
    )
  ))

#GRAFICO 1

grafico_aborto <- ggplot(coef_aborto, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(color = signif), size = 3) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high, color = signif),
    height = 0.15
  ) +
  scale_color_manual(values = c(
    "Significativo" = "darkred",
    "Nao significativo" = "black"
  )) +
  labs(
    x = "Associacao com ser contra a legalização do aborto (coeficiente padronizado)",
    y = "",
    title = "",
    color = "Significancia"
  ) +
  theme_bw()

grafico_aborto

ggsave(
  filename = "coeficientes_aborto_ESEB2022.png",
  plot = grafico_aborto,
  width = 9, height = 6, dpi = 300
)

#Parte 2 - Analise descritiva do golpismo

#Apoio ao Golpe Militar

library(psych)
library(dplyr)

#Golpe por muito crime

attributes(ESEB2022$Q30a)
table(ESEB2022$Q30a)

ESEB2022$golpe1 <- ifelse(ESEB2022$Q30a > 2,NA,ESEB2022$Q30a)
ESEB2022$golpe1 <- ifelse(ESEB2022$golpe1 == 1,1,0)
table(ESEB2022$golpe1)

attributes(ESEB2022$Q30b)
table(ESEB2022$Q30b)

#Golpe por muita corrupcao

ESEB2022$golpe2 <- ifelse(ESEB2022$Q30b > 2,NA,ESEB2022$Q30b)
ESEB2022$golpe2 <- ifelse(ESEB2022$golpe2 == 1,1,0)
table(ESEB2022$golpe2)

#Golpe por instabilidade politica

attributes(ESEB2022$Q30c)
table(ESEB2022$Q30c)

ESEB2022$golpe3 <- ifelse(ESEB2022$Q30c > 2,NA,ESEB2022$Q30c)
ESEB2022$golpe3 <- ifelse(ESEB2022$golpe3 == 1,1,0)
table(ESEB2022$golpe3)

#Grafico 7 - Distribuição golpista

library(patchwork)

plot_prop_eseb <- function(data, var, titulo = "") {
  
  df <- as.data.frame(prop.table(table(data[[var]]))) %>%
    rename(valor = Var1, freq = Freq)
  
  df$valor <- factor(
    df$valor,
    levels = c("1", "2", "97", "98"),
    labels = c("Justificado", "Não justificado", "Não sabe", "Não respondeu")
  )
  
  ggplot(df, aes(x = valor, y = freq)) +
    geom_col(fill = "white", colour = "black") +
    geom_text(
      aes(label = percent(freq, accuracy = 0.1)),
      vjust = -0.4,
      size = 3.5
    ) +
    scale_y_continuous(
      labels = percent_format(),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      x = NULL,
      y = "Proporção",
      title = titulo
    ) +
    theme_bw()
}

p30a <- plot_prop_eseb(ESEB2022, "Q30a", "Muito crime")
p30b <- plot_prop_eseb(ESEB2022, "Q30b", "Muita corrupção")
p30c <- plot_prop_eseb(ESEB2022, "Q30c", "Instabilidade política")

(p30a | p30b | p30c)

ggsave(
  filename = "golpismo_ESEB2022_Q30_1x3.png",
  plot     = (p30a | p30b | p30c),
  width    = 14,
  height   = 5,
  dpi      = 300,
  bg       = "white"
)

#Selecionando as tres variaveis

vars_golpe <- ESEB2022 %>% 
  select(golpe1, golpe2, golpe3)

#Analisando adequacao em um unico fator "golpismo"

af_golpe <- fa(vars_golpe, nfactors = 1, fm = "ml", rotate = "varimax")

print(af_golpe)

library(psych)

KMO(vars_golpe)
cortest.bartlett(vars_golpe)

#Robustez

#PCA

library(psych)

pca_golpe <- principal(
  vars_golpe,
  nfactors = 1,
  rotate   = "varimax",
  scores   = TRUE
)

print(pca_golpe)

#TRI

library(mirt)

irt_golpe <- mirt(
  data  = vars_golpe,
  model = 1,
  itemtype = "2PL"
)

summary(irt_golpe)

#Extraindo factor scores

scores_golpe <- factor.scores(vars_golpe, af_golpe, method = "regression")$scores

ESEB2022$golpe <- as.numeric(scores_golpe)

summary(ESEB2022$golpe)

#Grafico 8 - Adesao ao golpismo

df_golpe <- ESEB2022 %>%
  filter(!is.na(golpe))

ggplot(df_golpe, aes(x = golpe)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    bins = 20,
    fill = "white",
    colour = "black"
  ) +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = "Adesão ao golpismo (variável latente)",
    y = "Proporção",
    title = ""
  ) +
  theme_bw()

ggsave(
  filename = "golpismo_latente_ESEB2022.png",
  plot     = last_plot(),
  width    = 8,
  height   = 5,
  dpi      = 300,
  bg       = "white"
)

#Modelo de regressao golpismo

summary(lm(golpe ~ homem+escolarizacao+idade+classe+renda+religiao+religiosidade+
             interesse+direita, data = ESEB2022))

#Grafico 2 - Determinantes do Golpismo

#Modelo
modelo_golpe <- lm(golpe ~ homem + escolarizacao + idade + classe +
                     renda + religiao + religiosidade + interesse + direita,
                   data = ESEB2022)

#Extraindo coeficientes
coef_golpe <- tidy(modelo_golpe, conf.int = TRUE) |>
  filter(term != "(Intercept)")

#Criando matriz de modelo e desvios-padrao para padronizar os outputs
X_g <- model.matrix(modelo_golpe)[, -1]  
y_g <- model.response(model.frame(modelo_golpe))

sd_y_g <- sd(y_g, na.rm = TRUE)
sd_x_g <- apply(X_g, 2, sd, na.rm = TRUE)

sd_x_g <- sd_x_g[coef_golpe$term]

#Transformar tudo em coeficiente padronizado
coef_golpe <- coef_golpe |>
  mutate(
    sd_x = sd_x_g,
    sd_y = sd_y_g,
    estimate  = estimate  * sd_x / sd_y,
    conf.low  = conf.low  * sd_x / sd_y,
    conf.high = conf.high * sd_x / sd_y,
    signif = ifelse(p.value < 0.05, "Significativo", "Nao significativo")
  )

#Renomeando rotulos das variaveis para o grafico
coef_golpe <- coef_golpe |>
  mutate(term = dplyr::recode(term,
                              "homem"         = "Homem",
                              "escolarizacao" = "Escolarizacao",
                              "idade"         = "Idade",
                              "classe2"       = "Segundo Setor (ref: Primeiro Setor)",
                              "classe3"       = "Terceiro Setor (ref: Primeiro Setor)",
                              "renda"         = "Renda",
                              "religiao2"     = "Evangelicos (ref: Catolicos)",
                              "religiao3"     = "Sem Religiao (ref: Catolicos)",
                              "religiao4"     = "Outras Religioes (ref: Catolicos)",
                              "religiosidade" = "Religiosidade",
                              "interesse"     = "Interesse por Politica",
                              "direita"       = "Autoposicao a Direita"
  )) |>
  mutate(term = factor(
    term,
    levels = c(
      "Homem",
      "Escolarizacao",
      "Idade",
      "Segundo Setor (ref: Primeiro Setor)",
      "Terceiro Setor (ref: Primeiro Setor)",
      "Renda",
      "Evangelicos (ref: Catolicos)",
      "Sem Religiao (ref: Catolicos)",
      "Outras Religioes (ref: Catolicos)",
      "Religiosidade",
      "Interesse por Politica",
      "Autoposicao a Direita"
    )
  ))

#GRAFICO 2

grafico_golpe <- ggplot(coef_golpe, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(color = signif), size = 3) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high, color = signif),
    height = 0.15
  ) +
  scale_color_manual(values = c(
    "Significativo" = "darkred",
    "Nao significativo" = "black"
  )) +
  labs(
    x = "Associacao com apoio ao golpe (coeficiente padronizado)",
    y = "",
    title = "",
    color = "Significancia"
  ) +
  theme_bw()

grafico_golpe

ggsave(
  filename = "coeficientes_golpe_ESEB2022.png",
  plot = grafico_golpe,
  width = 9, height = 6, dpi = 300
)

#Parte 3 - Testando hipotese do Acordo Tacito

#Reorganizando voto, para mostrar que o efeito causal de aborto em golpismo desencadeio em apoio a bolsonaro no 1 e 2 turno

#Voto 1 turno

attributes(ESEB2022$Q10P1b)
table(ESEB2022$Q10P1b)
prop.table(table(ESEB2022$Q10P1b))

ESEB2022$voto <- ifelse(ESEB2022$Q10P1b > 60,NA,ESEB2022$Q10P1b)
ESEB2022$voto <- ifelse(ESEB2022$voto == 4,1,0)

table(ESEB2022$voto)
prop.table(table(ESEB2022$voto))

#voto 2 turno

attributes(ESEB2022$Q10P2b)
table(ESEB2022$Q10P2b)
prop.table(table(ESEB2022$Q10P2b))

ESEB2022$voto2 <- ifelse(ESEB2022$Q10P2b > 60,NA,ESEB2022$Q10P2b)
ESEB2022$voto2 <- ifelse(ESEB2022$voto2 == 1,1,0)

table(ESEB2022$voto2)
prop.table(table(ESEB2022$voto2))

#Grafico 9 Votos

plot_prop_bin <- function(data, var, titulo = "") {
  
  df <- as.data.frame(prop.table(table(data[[var]]))) %>%
    rename(valor = Var1, freq = Freq)
  
  df$valor <- factor(
    df$valor,
    levels = c("0", "1"),
    labels = c("Não Bolsonaro", "Bolsonaro")
  )
  
  ggplot(df, aes(x = valor, y = freq)) +
    geom_col(fill = "white", colour = "black") +
    geom_text(
      aes(label = percent(freq, accuracy = 0.1)),
      vjust = -0.4,
      size = 3.5
    ) +
    scale_y_continuous(
      labels = percent_format(),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      x = NULL,
      y = "Proporção",
      title = titulo
    ) +
    theme_bw()
}

p_voto1 <- plot_prop_bin(ESEB2022, "voto",  "Voto no 1º turno")
p_voto2 <- plot_prop_bin(ESEB2022, "voto2", "Voto no 2º turno")

(p_voto1 | p_voto2)

ggsave(
  filename = "voto_ESEB2022_1x2.png",
  plot     = (p_voto1 | p_voto2),
  width    = 14,
  height   = 5,
  dpi      = 300,
  bg       = "white"
)

#Matching

library(MatchIt)
library(tidyr)

ESEB2022_clean <- ESEB2022 %>%
  drop_na(aborto,golpe,voto,voto2,D02,D03,D07a,D09_RENDAF,D10,Q01,Q19,T_D01A_3,D11)

matching2022 <- matchit(
  aborto ~ D02+D03+D07a+D09_RENDAF+D10+Q01+Q19+T_D01A_3+D11,
  data   = ESEB2022_clean,
  method = "nearest",
  ratio  = 1
)

summary(matching2022)

matching_data2022 <- match.data(matching2022)

matching_grafico <- plot(matching2022, type = "jitter")

matching_data2022$aborto <- ifelse(matching_data2022$aborto == 1,0,1)

#SEM

#1 Turno

library(lavaan)

modelo_sem <- '
  # Regressoes com labels
  golpe ~ a*aborto + religiosidade
  voto  ~ b*golpe
  
  # Efeito indireto: usa os labels a e b, nao os nomes das variaveis
  ind_aborto_voto := a*b
'

ajuste_sem <- sem(modelo_sem, data = matching_data2022, estimator = "ML")

summary(ajuste_sem, standardized = TRUE, fit.measures = TRUE)

library(semPlot)

#Grafico

pe    <- parameterEstimates(ajuste_sem)
edges <- subset(pe, op == "~")

edge_labels <- sapply(seq_len(nrow(edges)), function(i) {
  est <- round(edges$est[i], 3)
  se  <- round(edges$se[i], 3)
  p   <- edges$pvalue[i]
  ptxt <- if (p < 0.001) "p < .001" else paste0("p = ", round(p, 3))
  paste0(est, " (", se, ")\n", ptxt)
})

#Efeito indireto
ind_row <- subset(pe, op == ":=" & lhs == "ind_aborto_voto")

ind_est <- round(ind_row$est, 3)
ind_se  <- round(ind_row$se, 3)
ind_p   <- ind_row$pvalue
ind_p_txt <- if (ind_p < 0.001) "p < .001" else paste0("p = ", round(ind_p, 3))

ind_label <- paste0(
  "Efeito indireto do aborto\n",
  "sobre o voto via golpismo:\n",
  ind_est, " (", ind_se, ") ",
  ind_p_txt
)

#Caixa de medidas de ajuste
fits <- fitMeasures(ajuste_sem, c("chisq","df","cfi","tli","srmr","rmsea"))
chi2df <- fits["chisq"] / fits["df"]

fit_label <- paste0(
  "Medidas de ajuste\n",
  "χ²/df = ", sprintf("%.2f", chi2df), "\n",
  "CFI   = ", sprintf("%.3f", fits["cfi"]), "\n",
  "TLI   = ", sprintf("%.3f", fits["tli"]), "\n",
  "SRMR  = ", sprintf("%.3f", fits["srmr"]), "\n",
  "RMSEA = ", sprintf("%.3f", fits["rmsea"])
)

#Diagrama com semPlot
semPaths(
  ajuste_sem,
  what          = "path",
  edgeLabels    = edge_labels,
  layout        = "tree2",
  rotation      = 2,
  sizeMan       = 10,
  edge.label.cex = 1.2,
  nCharNodes    = 0,
  residuals     = FALSE,
  intercepts    = FALSE,
  asize         = 2.2,
  edge.width    = 1.5,
  color = list(
    lat   = "white",
    man   = "white",
    edge  = "black",
    arrow = "black"
  ),
  border.color = "black",
  edge.color   = "black"
)

usr <- par("usr")
par(xpd = NA)

mtext("1º Turno", side = 3, line = 1, cex = 1.3, adj = 0.5)

#Adicionando a caixa
x_min <- usr[1] + 0.60 * (usr[2] - usr[1])
x_max <- usr[1] + 0.97 * (usr[2] - usr[1])
y_min <- usr[3] + 0.01 * (usr[4] - usr[3])
y_max <- usr[3] + 0.15 * (usr[4] - usr[3])

rect(x_min, y_min, x_max, y_max, border = "black", lwd = 1)
text(
  x      = (x_min + x_max) / 2,
  y      = (y_min + y_max) / 2,
  labels = ind_label,
  cex    = 0.75
)

#Caixa do ajuste
x_min_fit <- usr[1] + 0.60 * (usr[2] - usr[1])
x_max_fit <- usr[1] + 0.97 * (usr[2] - usr[1])
y_min_fit <- usr[3] + 0.15 * (usr[4] - usr[3])
y_max_fit <- usr[3] + 0.41 * (usr[4] - usr[3])

rect(x_min_fit, y_min_fit, x_max_fit, y_max_fit, border = "black", lwd = 1)

text(
  x      = (x_min_fit + x_max_fit) / 2,
  y      = (y_min_fit + y_max_fit) / 2,
  labels = fit_label,
  cex    = 0.75
)

#Guardando plot
g1 <- recordPlot()

#2 Turno

library(lavaan)

modelo_sem2 <- '
  # Regressoes com labels
  golpe ~ a*aborto + religiosidade
  voto2  ~ b*golpe
  
  # Efeito indireto: usa os labels a e b, nao os nomes das variaveis
  ind_aborto_voto := a*b
'

ajuste_sem2 <- sem(modelo_sem2, data = matching_data2022, estimator = "ML")

summary(ajuste_sem2, standardized = TRUE, fit.measures = TRUE)

library(semPlot)

summary(ajuste_sem2, standardized = TRUE, fit.measures = TRUE)

pe    <- parameterEstimates(ajuste_sem2)
edges <- subset(pe, op == "~")

edge_labels <- sapply(seq_len(nrow(edges)), function(i) {
  est <- round(edges$est[i], 3)
  se  <- round(edges$se[i], 3)
  p   <- edges$pvalue[i]
  
  ptxt <- if (p < 0.001) "p < .001" else paste0("p = ", round(p, 3))
  
  paste0(est, " (", se, ")\n", ptxt)
})

ind_row <- subset(pe, op == ":=" & lhs == "ind_aborto_voto")

ind_est <- round(ind_row$est, 3)
ind_se  <- round(ind_row$se, 3)
ind_p   <- ind_row$pvalue

ind_p_txt <- if (ind_p < 0.001) "p < .001" else paste0("p = ", round(ind_p, 3))

ind_label <- paste0(
  "Efeito indireto do aborto\n",
  "sobre o voto via golpismo:\n",
  ind_est, " (", ind_se, ") ",
  ind_p_txt
)

fits <- fitMeasures(
  ajuste_sem2,
  c("chisq", "df", "cfi", "tli", "srmr", "rmsea")
)

chi2df <- fits["chisq"] / fits["df"]

fit_label <- paste0(
  "Medidas de ajuste\n",
  "χ2/df = ", sprintf("%.2f", chi2df), "\n",
  "CFI   = ", sprintf("%.3f", fits["cfi"]), "\n",
  "TLI   = ", sprintf("%.3f", fits["tli"]), "\n",
  "SRMR  = ", sprintf("%.3f", fits["srmr"]), "\n",
  "RMSEA = ", sprintf("%.3f", fits["rmsea"])
)

semPaths(
  ajuste_sem2,
  what          = "path",
  edgeLabels    = edge_labels,
  layout        = "tree2",
  rotation      = 2,
  sizeMan       = 10,
  edge.label.cex = 1.2,
  nCharNodes    = 0,
  residuals     = FALSE,
  intercepts    = FALSE,
  asize         = 2.2,
  edge.width    = 1.5,
  color = list(
    lat   = "white",
    man   = "white",
    edge  = "black",
    arrow = "black"
  ),
  border.color = "black",
  edge.color   = "black"
)

usr <- par("usr")

par(xpd = NA)

mtext("2º Turno", side = 3, line = 1, cex = 1.3, adj = 0.5)

x_min <- usr[1] + 0.60 * (usr[2] - usr[1])
x_max <- usr[1] + 0.97 * (usr[2] - usr[1])
y_min <- usr[3] + 0.01 * (usr[4] - usr[3])
y_max <- usr[3] + 0.15 * (usr[4] - usr[3])

rect(x_min, y_min, x_max, y_max, border = "black", lwd = 1)
text(
  x      = (x_min + x_max) / 2,
  y      = (y_min + y_max) / 2,
  labels = ind_label,
  cex    = 0.75
)

x_min_fit <- usr[1] + 0.60 * (usr[2] - usr[1])
x_max_fit <- usr[1] + 0.97 * (usr[2] - usr[1])
y_min_fit <- usr[3] + 0.15 * (usr[4] - usr[3])
y_max_fit <- usr[3] + 0.41 * (usr[4] - usr[3])

rect(x_min_fit, y_min_fit, x_max_fit, y_max_fit, border = "black", lwd = 1)

text(
  x      = (x_min_fit + x_max_fit) / 2,
  y      = (y_min_fit + y_max_fit) / 2,
  labels = fit_label,
  cex    = 0.75
)

g2 <- recordPlot()

#Salvando plots pra depois colar
  png("g1.png", width = 1000, height = 750, res = 200)
  replayPlot(g1)
  dev.off()
  
  png("g2.png", width = 1000, height = 750, res = 200)
  replayPlot(g2)
  dev.off()
  #Colando plots
  library(magick)
  
  img1 <- image_read("g1.png")
  img2 <- image_read("g2.png")
  
  arranjo <- image_append(c(img1, img2), stack = TRUE)
  
  image_write(arranjo, "arranjo_vertical.png")
  
#Robustez
  
  #SEM
  
  #1 Turno

  
  library(lavaan)
  library(semPlot)
  
  modelo_sem <- '
  golpe ~ a*contra_aborto + religiosidade
  voto  ~ b*golpe
  ind_aborto_voto := a*b
'
  
  ajuste_sem <- sem(modelo_sem, data = ESEB2022, estimator = "ML")
  
  pe    <- parameterEstimates(ajuste_sem)
  edges <- subset(pe, op == "~")
  
  edge_labels <- sapply(seq_len(nrow(edges)), function(i) {
    est <- round(edges$est[i], 3)
    se  <- round(edges$se[i], 3)
    p   <- edges$pvalue[i]
    ptxt <- if (p < 0.001) "p < .001" else paste0("p = ", round(p, 3))
    paste0(est, " (", se, ")\n", ptxt)
  })
  
  ind_row <- subset(pe, op == ":=" & lhs == "ind_aborto_voto")
  
  ind_est <- round(ind_row$est, 3)
  ind_se  <- round(ind_row$se, 3)
  ind_p   <- ind_row$pvalue
  ind_p_txt <- if (ind_p < 0.001) "p < .001" else paste0("p = ", round(ind_p, 3))
  
  ind_label <- paste0(
    "Efeito indireto do aborto\n",
    "sobre o voto via golpismo:\n",
    ind_est, " (", ind_se, ") ",
    ind_p_txt
  )
  
  fits <- fitMeasures(ajuste_sem, c("chisq","df","cfi","tli","srmr","rmsea"))
  chi2df <- fits["chisq"] / fits["df"]
  
  fit_label <- paste0(
    "Medidas de ajuste\n",
    "χ²/df = ", sprintf("%.2f", chi2df), "\n",
    "CFI   = ", sprintf("%.3f", fits["cfi"]), "\n",
    "TLI   = ", sprintf("%.3f", fits["tli"]), "\n",
    "SRMR  = ", sprintf("%.3f", fits["srmr"]), "\n",
    "RMSEA = ", sprintf("%.3f", fits["rmsea"])
  )
  
  semPaths(
    ajuste_sem,
    what          = "path",
    edgeLabels    = edge_labels,
    layout        = "tree2",
    rotation      = 2,
    sizeMan       = 10,
    edge.label.cex = 1.2,
    nCharNodes    = 0,
    residuals     = FALSE,
    intercepts    = FALSE,
    asize         = 2.2,
    edge.width    = 1.5,
    color = list(
      lat   = "white",
      man   = "white",
      edge  = "black",
      arrow = "black"
    ),
    border.color = "black",
    edge.color   = "black"
  )
  
  usr <- par("usr")
  par(xpd = NA)
  
  mtext("1º Turno", side = 3, line = 1, cex = 1.3, adj = 0.5)
  
  x_min <- usr[1] + 0.60 * (usr[2] - usr[1])
  x_max <- usr[1] + 0.97 * (usr[2] - usr[1])
  y_min <- usr[3] + 0.01 * (usr[4] - usr[3])
  y_max <- usr[3] + 0.15 * (usr[4] - usr[3])
  
  rect(x_min, y_min, x_max, y_max, border = "black", lwd = 1)
  text(
    x      = (x_min + x_max) / 2,
    y      = (y_min + y_max) / 2,
    labels = ind_label,
    cex    = 0.75
  )
  
  x_min_fit <- usr[1] + 0.60 * (usr[2] - usr[1])
  x_max_fit <- usr[1] + 0.97 * (usr[2] - usr[1])
  y_min_fit <- usr[3] + 0.15 * (usr[4] - usr[3])
  y_max_fit <- usr[3] + 0.41 * (usr[4] - usr[3])
  
  rect(x_min_fit, y_min_fit, x_max_fit, y_max_fit, border = "black", lwd = 1)
  
  text(
    x      = (x_min_fit + x_max_fit) / 2,
    y      = (y_min_fit + y_max_fit) / 2,
    labels = fit_label,
    cex    = 0.75
  )
  
  g1 <- recordPlot()
  
#2 TURNO
  
  library(lavaan)
  library(semPlot)
  
  modelo_sem2 <- '
  # Regressões com labels
  golpe ~ a*contra_aborto + religiosidade
  voto2 ~ b*golpe
  
  # Efeito indireto: usa os labels a e b, nao os nomes das variaveis
  ind_aborto_voto := a*b
'
  
  ajuste_sem2 <- sem(modelo_sem2, data = ESEB2022, estimator = "ML")
  
  summary(ajuste_sem2, standardized = TRUE, fit.measures = TRUE)
  
  pe    <- parameterEstimates(ajuste_sem2)
  edges <- subset(pe, op == "~")
  
  edge_labels <- sapply(seq_len(nrow(edges)), function(i) {
    est <- round(edges$est[i], 3)
    se  <- round(edges$se[i], 3)
    p   <- edges$pvalue[i]
    
    ptxt <- if (p < 0.001) "p < .001" else paste0("p = ", round(p, 3))
    
    paste0(est, " (", se, ")\n", ptxt)
  })
  
  ind_row <- subset(pe, op == ":=" & lhs == "ind_aborto_voto")
  
  ind_est <- round(ind_row$est, 3)
  ind_se  <- round(ind_row$se, 3)
  ind_p   <- ind_row$pvalue
  
  ind_p_txt <- if (ind_p < 0.001) "p < .001" else paste0("p = ", round(ind_p, 3))
  
  ind_label <- paste0(
    "Efeito indireto do aborto\n",
    "sobre o voto via golpismo:\n",
    ind_est, " (", ind_se, ") ",
    ind_p_txt
  )
  
  fits <- fitMeasures(
    ajuste_sem2,
    c("chisq", "df", "cfi", "tli", "srmr", "rmsea")
  )
  
  chi2df <- fits["chisq"] / fits["df"]
  
  fit_label <- paste0(
    "Medidas de ajuste\n",
    "χ2/df = ", sprintf("%.2f", chi2df), "\n",
    "CFI   = ", sprintf("%.3f", fits["cfi"]), "\n",
    "TLI   = ", sprintf("%.3f", fits["tli"]), "\n",
    "SRMR  = ", sprintf("%.3f", fits["srmr"]), "\n",
    "RMSEA = ", sprintf("%.3f", fits["rmsea"])
  )
  
  semPaths(
    ajuste_sem2,
    what          = "path",
    edgeLabels    = edge_labels,
    layout        = "tree2",
    rotation      = 2,
    sizeMan       = 10,
    edge.label.cex = 1.2,
    nCharNodes    = 0,
    residuals     = FALSE,
    intercepts    = FALSE,
    asize         = 2.2,
    edge.width    = 1.5,
    color = list(
      lat   = "white",
      man   = "white",
      edge  = "black",
      arrow = "black"
    ),
    border.color = "black",
    edge.color   = "black"
  )
  
  usr <- par("usr")
  
  par(xpd = NA)
  
  mtext("2º Turno", side = 3, line = 1, cex = 1.3, adj = 0.5)
  
  x_min <- usr[1] + 0.60 * (usr[2] - usr[1])
  x_max <- usr[1] + 0.97 * (usr[2] - usr[1])
  y_min <- usr[3] + 0.01 * (usr[4] - usr[3])
  y_max <- usr[3] + 0.15 * (usr[4] - usr[3])
  
  rect(x_min, y_min, x_max, y_max, border = "black", lwd = 1)
  text(
    x      = (x_min + x_max) / 2,
    y      = (y_min + y_max) / 2,
    labels = ind_label,
    cex    = 0.75
  )
  
  x_min_fit <- usr[1] + 0.60 * (usr[2] - usr[1])
  x_max_fit <- usr[1] + 0.97 * (usr[2] - usr[1])
  y_min_fit <- usr[3] + 0.15 * (usr[4] - usr[3])
  y_max_fit <- usr[3] + 0.41 * (usr[4] - usr[3])
  
  rect(x_min_fit, y_min_fit, x_max_fit, y_max_fit, border = "black", lwd = 1)
  
  text(
    x      = (x_min_fit + x_max_fit) / 2,
    y      = (y_min_fit + y_max_fit) / 2,
    labels = fit_label,
    cex    = 0.75
  )
  
  g2 <- recordPlot()
  
  png("g1_robustez.png", width = 1000, height = 750, res = 200)
  replayPlot(g1)
  dev.off()
  
  png("g2_robustez.png", width = 1000, height = 750, res = 200)
  replayPlot(g2)
  dev.off()
  
  library(magick)
  
  img1 <- image_read("g1_robustez.png")
  img2 <- image_read("g2_robustez.png")
  
  arranjo <- image_append(c(img1, img2), stack = TRUE)
  
  image_write(arranjo, "arranjo_vertical_robustez.png")
  

  