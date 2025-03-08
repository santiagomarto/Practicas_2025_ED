# Instalación de librerias ------------------------------------------------
  # Para crear firltros ctrl + shift + R
  # Para Comentar codigo ctrl + shift + c

# install.packages("dplyr")
# install.packages("dplyrAssist")
# install.packages("tidyr")
#install.packages("esquisse")
#install.packages("plotly")
#install.packages('DT')
#install.packages('data.table')
library(dplyr)
library(dplyrAssist)
library(data.table)
library(readxl)
library(esquisse)
# library(DT)
# library(data.table)

options(scipen = 999 )

# Carga de Datos ----------------------------------------------------------

  # para hacer el %>% el comando es ctrl + shift + M 

data = fread(input = "Data/CNA2014_ENCABEZADO_15.csv",
             sep=",") %>% 
  select(COD_VEREDA,TIPO_UC,S05_TENENCIA,P_S5PAUTOS,P_S7P82,P_S7P84F,P_S7P85B) %>%
  filter(TIPO_UC == 1) %>% 
  mutate(S05_TENENCIA = as.character(S05_TENENCIA))

str(data)
glimpse(data)

# Limpieza de datos -------------------------------------------------------

t_homologacion_7 = read_excel(
  path = 'Data/Tablasdehomologacion.xlsx',
  sheet = 'Hoja2') %>% 
  mutate(S05_TENENCIA = as.character(S05_TENENCIA))

str(t_homologacion_7)

data_dep = data %>% 
  left_join(t_homologacion_7, by = 'S05_TENENCIA' ) %>% 
  select(Predominancia,P_S7P85B) %>% 
  na.omit()


# Tablas de distribución de frecuencias (TDF) para Var cualitativa --------------

tdf_S05_TENENCIA = data_dep %>% 
  group_by(Predominancia) %>% 
  summarise(n_i = n()) %>% 
  arrange(desc(n_i)) %>% 
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i)*100,
         F_i = cumsum(f_i))

# Grafico

barplot(table(data_dep$Predominancia))

#esquisse::esquisser(viewer = "browser") sirve para sacar el codigo 
# de los diagramas de manera mas automatica

ggplot(data_dep) +
  aes(x = Predominancia) +
  geom_bar(fill = "#B22222") +
  theme_minimal()


# Como saber el Número de clases ------------------------------------------

#N_clases(K) n=10, 100, 1000

### tres formas

## Criterio de la Raiz

# K=sqrt(n)

## Regla empirica

# 5 <= K <= 20

# Regla Sturges 

#K= 1 + 3.3*Log(n)

k = round(1 + 3.3 *log10(nrow(data_dep)))



# Como hacer el Rango -------------------------------------------------------------------

rango = max(data_dep$P_S7P85B, na.rm = T) -
  min(data_dep$P_S7P85B, na.rm = T)
rango


# Como hacer la Longitud ----------------------------------------------------------------


longitud = rango/k 


# Como hacer los cortes ------------------------------------------------------------------

cortes = min(data_dep$P_S7P85B, na.rm = T ) + c(seq(0,k,1))*longitud
cortes

# TDF de varibale cuantitativa --------------------------------------------

TDF_P_S7P85B = data_dep %>%
  mutate(P_S7P85B_c = as.factor(cut(P_S7P85B,
                          breaks = cortes,
                          levels = cortes,
                          include.lowest = T,
                          dig.lab = 6))) %>% 
  group_by(P_S7P85B_c, .drop = F , .add = F) %>% 
  summarise(n_i = n()) %>% 
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i)*100,
         F_i = cumsum(f_i),
         x_i = cortes[1:k] + longitud/2,
         c_i = abs(cortes[1:k] - cortes[2:(k+1)]),
         d_i = n_i/c_i)


# Histograma --------------------------------------------------------------


hist(data_dep$P_S7P85B)

mean(data_dep$P_S7P85B)
median(data_dep$P_S7P85B)


