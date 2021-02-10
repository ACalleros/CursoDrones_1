Ejercicio 1
================

\`\`\`{r message=FALSE, warning=FALSE, paged.print=FALSE}

library(tidyverse) library(DataEditR) library(magrittr)
library(gghighlight) library(ggrepel) library(scales) library(patchwork)
library(sf) library(fuzzyjoin)

mx &lt;- st\_read(“D:/Documents/GitHub/Tesis/shapes/mx.gpkg”) %&gt;%
rename(entidad = 1)

mx*e**n**t**i**d**a**d* &lt;  − *a**s*.*c**h**a**r**a**c**t**e**r*(*f**c**t*<sub>*r*</sub>*e**c**o**d**e*(*m**x*entidad,
“Coahuila” = “Coahuila De Zaragoza”, “Ciudad de México” = “Distrito
Federal”, “México” = “Mexico”, “Michoacán” = “Michoacan”, “Nuevo León” =
“Nuevo Leon”, “Querétaro” = “Queretaro De Arteaga” ))

incendios &lt;- read\_csv(‘incendios mexico.csv’) reforestaciones &lt;-
read\_csv(‘reforestacion.csv’)

incendios$\`Entidad federativa\` &lt;- fct\_recode(incendios$`Entidad federativa`,
“Ciudad de México” = “Ciudad de Méxic”, “México” = “Méxic”, “Michoacán”
= “Michoacá”, “Nuevo León” = “Nuevo Leó”, “Querétaro” = “Querétar”,
“Yucatán” = “Yucatá” )

reforestaciones$\`Entidad federativa\` &lt;- fct\_recode(reforestaciones$`Entidad federativa`,
“Ciudad de México” = “Ciudad de Méxic”, “México” = “Méxic”, “Michoacán”
= “Michoacá”, “Nuevo León” = “Nuevo Leó”, “Querétaro” = “Querétar”,
“Yucatán” = “Yucatá” )

incendios &lt;- incendios %&gt;% rename(entidad = 1) %&gt;%
filter(entidad != ‘Nacional’) %&gt;% pivot\_longer(!c(entidad),
names\_to = ‘año’, values\_to = ‘superficie’) %&gt;% mutate(año =
parse\_number(año)) %&gt;% group\_by(entidad) %&gt;% mutate(acumulado =
cumsum(superficie), t = año - lag(año), cociente = superficie /
lag(superficie), a = cociente ^ (1/t), tasa = round(a - 1, 2)) %&gt;%
mutate(tipo = ‘incendios’, entidad = as.character(entidad)) %&gt;%
select(!t:a)

superficie\_incendios &lt;- ggplot(incendios, aes(año, acumulado, color
= entidad)) + geom\_line(size = 2) + geom\_point(size = 3) +
gghighlight(max(acumulado), max\_highlight = 10, use\_direct\_label = F,
unhighlighted\_params = list(alpha = 0.3)) +
facet\_wrap(\~fct\_reorder(entidad, acumulado, .fun = max, .desc = T))+
theme\_minimal() + scale\_y\_continuous(labels = comma) + labs(x =
‘Año’, y = ‘Hectáreas’) + theme(legend.position = ‘none’)

reforestaciones &lt;- reforestaciones %&gt;% rename(entidad = 1) %&gt;%
filter(entidad != ‘Nacional’) %&gt;% pivot\_longer(!c(entidad),
names\_to = ‘año’, values\_to = ‘superficie’) %&gt;% mutate(año =
parse\_number(año)) %&gt;% group\_by(entidad) %&gt;% mutate(acumulado =
cumsum(superficie), t = año - lag(año), cociente = superficie /
lag(superficie), a = cociente ^ (1/t), tasa = round(a - 1, 2)) %&gt;%
mutate(tipo = ‘incendios’, entidad = as.character(entidad)) %&gt;%
select(!t:a)

superficie\_reforestaciones &lt;- ggplot(reforestaciones, aes(año,
acumulado, color = entidad)) + geom\_line(size = 2) + geom\_point(size =
3) + gghighlight(max(acumulado), max\_highlight = 10, use\_direct\_label
= F, unhighlighted\_params = list(alpha = 0.3)) +
facet\_wrap(\~fct\_reorder(entidad, acumulado, .fun = max, .desc = T))+
theme\_minimal() + scale\_y\_continuous(labels = comma, limits = c(0,
150000)) + labs(x = ‘Año’, y = ‘Hectáreas’) + theme(legend.position =
‘none’)

superficie\_incendios / superficie\_reforestaciones

\`\`\`
