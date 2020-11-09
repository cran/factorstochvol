## ----setup, include=FALSE, cache=FALSE----------------------------------------
knitr::render_sweave()
knitr::opts_chunk$set(prompt = TRUE,
                      fig.show = "hide",
                      warning = FALSE,
                      error = FALSE,
                      message = FALSE,
                      echo = FALSE,
                      cache = TRUE,
                      fig.path = "Figures/article-")
base::options(continue = "+  ", prompt = "R> ")
#used_packages <- c("LSD", "RColorBrewer")
#for (p in used_packages) {
#  if (!require(p, character.only = TRUE)) {
#    install.packages(p)
#  }
#}

## ----voltimeplot, echo=2:3, fig.width = 10, fig.height = 3, cache.rebuild = TRUE----
opar <- par(mgp = c(1.7, 0.5, 0), mar = c(2, 1.5, 1, 0.5))
palette(RColorBrewer::brewer.pal(7, "Dark2")[-5])
voltimeplot(res, legend = "top")
par(opar)

