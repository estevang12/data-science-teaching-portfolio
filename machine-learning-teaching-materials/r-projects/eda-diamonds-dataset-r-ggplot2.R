library(tidyverse)
library(datos)
library(ggplot2)
ggplot(data = diamantes) +
   geom_bar(mapping = aes(x = corte))

ggplot(data = diamantes) +
  stat_count(mapping = aes(x = corte))

demo <- tribble(
  ~corte, ~freq,
  "Regular", 1610,
  "Bueno", 4906,
  "Muy Bueno", 12082,
  "Premium", 13791,
  "Ideal", 21551
)

demo <- tribble(
 ~corte, ~freq,
 "Regular", 1610,
 "Bueno", 4906,
 "Muy Bueno", 12082,
 "Premium", 13791,
 "Ideal", 21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = corte, y = freq), stat = "identity")

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, y = stat(prop), group = 1))

ggplot(data = diamantes) +
   stat_summary(
     mapping = aes(x = corte, y = profundidad),
     fun.min = min,
     fun.max = max,
    fun = median
  )

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, colour = corte))

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = corte))

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = claridad))

ggplot(data = diamantes, mapping = aes(x = corte, fill = claridad)) +
   geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamantes, mapping = aes(x = corte, colour = claridad)) +
  geom_bar(fill = NA, position = "identity")

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = claridad), position = "fill")

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = claridad), position = "dodge")

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), position = "jitter")

ggplot(data = millas, mapping = aes(x = clase, y = autopista)) +
   geom_boxplot()

ggplot(data = millas, mapping = aes(x = clase, y = autopista)) +
  geom_boxplot() +
  coord_flip()

nz <- map_data("nz")
 ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")
 
 ggplot(nz, aes(long, lat, group = group)) +
   geom_polygon(fill = "white", colour = "black") +
   coord_quickmap()
 
 bar <- ggplot(data = diamantes) +
   geom_bar(
     mapping = aes(x = corte, fill = corte),
      show.legend = FALSE,
      width = 1
      ) +
   theme(aspect.ratio = 1) +
   labs(x = NULL, y = NULL)
 bar + coord_flip()
 bar + coord_polar()