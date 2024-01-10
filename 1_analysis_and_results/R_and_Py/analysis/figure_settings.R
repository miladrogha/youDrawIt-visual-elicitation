my_theme = theme_apa() +
  theme(text = element_text(size=11),
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5, unit='pt'))

theme_set(my_theme)
