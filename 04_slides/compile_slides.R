# Compilar presentación
tinytex::latexmk("04_slides/slides.tex")

# Eliminar archivos auxiliares
extensiones <- c(
  "aux", "log", "nav", "snm",
  "toc", "out", "synctex.gz",
  "fls", "fdb_latexmk"
)

unlink(file.path(
  "04_slides",
  paste0("slides.", extensiones)
))

rm(extensiones)