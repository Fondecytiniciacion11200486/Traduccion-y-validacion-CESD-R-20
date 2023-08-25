# Material de replicación
# https://github.com/Fondecytiniciacion11200486/Traduccion-y-validacion-CESD-R-20
# Created 230700
# Updated 230823
# jcabezas@umd.edu; espinoza.anakaren@gmail.com


# Pramble
rm(list=ls())

if(!require(readr)){install.packages("readr")}
if(!require(psych)){install.packages("psych")}
if(!require(EGAnet)){install.packages("EGAnet")}
if(!require(stargazer)){install.packages("stargazer")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(dplyr)){install.packages("dplyr")}


# Dataset ----
survey <- read_csv('data/survey_imputed.csv')

## Subsets -----
todos <- survey %>%  select(-c(string_id, espanol, mujer))
kreole <- survey %>% filter(espanol==1) %>%  select(-c(string_id, espanol, mujer))
espanol <- survey %>% filter(espanol==0) %>%  select(-c(string_id, espanol, mujer))

## Descriptivos ----
survey %>% 
  as.data.frame() %>% 
  stargazer::stargazer(type="text", median=T,
                       out="tablas/descriptivos.html")
survey %>% 
  as.data.frame() %>% 
  stargazer::stargazer(type="text", median=T,
                       out="tablas/descriptivos_kreole.html")
survey %>% 
  as.data.frame() %>% 
  stargazer::stargazer(type="text", median=T,
                       out="tablas/descriptivos_espanol.html")

# Análisis Estadístico ----

## Alfa de Cronbach ----

### Todos ----
espanol %>% psych::alpha(.)

### Español ----
espanol %>% psych::alpha(.)

### Kreole ----
kreole %>% psych::alpha(.)


## Análisis Factorial ----
### AF Exploratorio ----
todos %>% psych::fa()
espanol %>% psych::fa()
kreole %>% psych::fa()

### AF Confirmatorio ----
todos %>% psych::fa(nfactors=3)
espanol %>% psych::fa(nfactors=3)
kreole %>% psych::fa(nfactors=3)


# Análisis de Factores Principales
## Exploratirio ----
todos %>% psych::principal()
espanol %>% psych::principal()
kreole %>% psych::principal()

todos %>% psych::principal(., nfactors=3)
espanol %>% psych::principal(., nfactors=3)
kreole %>% psych::principal(., nfactors=3)



# Tests de robustés ----

## Parellel analysis ----

### Todos ----
png('figuras/graficosed_all.png', width=4, height=3, units="in", res=108, pointsize = 7)
par(mai=c(.3,.5,.2,.1))
psych::fa.parallel(todos, cor="cor",
                   main="Todos los casos",
                   ylabel = "Valores propios")
dev.off()


#### Espanol ----
png('figuras/graficosed_esp.png', width=4, height=3, units="in", res=108, pointsize = 7)
par(mai=c(.3,.5,.2,.1))
psych::fa.parallel(espanol, cor="cor",
                   main="Español",
                   ylabel = "Valores propios")
dev.off()


#### Kreole ----
png('figuras/graficosed_kre.png', width=4, height=3, units="in", res=108, pointsize = 7)
par(mai=c(.3,.5,.2,.1))
psych::fa.parallel(kreole, cor="cor",
                   main="Kreole",
                   ylabel = "Valores propios")
dev.off()



### EGA ----
### Análisis Gráfico Exploratorio ----
#### Todos ----
ega_todos <- EGAnet::EGA(todos)
ega_todos
plot(ega_todos) + ggtitle("Todos los casos") + theme(legend.position="none")
ggsave('figuras/ega_todos.png', width=4, height=3, units="in")


#### Espanol ----
ega_esp <- EGAnet::EGA(espanol)
ega_esp
plot(ega_esp) + ggtitle("Cuestionario en Español") + theme(legend.position="none")
ggsave('figuras/ega_esp.png', width=4, height=3, units="in")


#### Kreole ----
ega_kre <- EGAnet::EGA(kreole)
ega_kre
plot(ega_kre) + ggtitle("Cuestionario en Kreole")
ggsave('figuras/ega_kre.png', width=4, height=3, units="in")

# save.image('replication.RData')