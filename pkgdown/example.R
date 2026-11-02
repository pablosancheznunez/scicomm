# Builds the example reports published on the website.
library(scicomm)

out <- "_site/example"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

en <- sc_example("en")
sc_report(en$finding, en$versions, file = file.path(out, "report.html"))
es <- sc_example("es")
sc_report(es$finding, es$versions, file = file.path(out, "report-es.html"))
