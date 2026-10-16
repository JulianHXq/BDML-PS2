# Compilar presentación
# Requiere el paquete tinytex y una instalación de LaTeX (por ejemplo,
# tinytex::install_tinytex()). Si no están disponibles, se avisa y se sigue:
# las diapositivas son el único paso del proyecto que depende de LaTeX, y su
# ausencia no debe impedir reproducir los datos, los modelos y los resultados.
if (!requireNamespace("tinytex", quietly = TRUE) || Sys.which("pdflatex") == "") {
  message("No se compilan las diapositivas: no se encontró LaTeX. ",
          "Instálelo con tinytex::install_tinytex() para compilarlas.")
} else {
  # LaTeX resuelve las rutas de slides.tex (../02_outputs/...) desde la
  # carpeta en la que se compila; por eso se compila dentro de 04_slides/.
  # El bloque finally garantiza que se borren los auxiliares y se vuelva a la
  # raíz del proyecto aunque la compilación falle.
  extensiones <- c(
    "aux", "log", "nav", "snm",
    "toc", "out", "synctex.gz",
    "fls", "fdb_latexmk"
  )
  
  dir_raiz <- getwd()
  setwd("04_slides")
  tryCatch(
    tinytex::latexmk("slides.tex"),
    finally = {
      unlink(paste0("slides.", extensiones))
      setwd(dir_raiz)
    }
  )
  
  rm(dir_raiz, extensiones)
}
