# Install latex packages using tinytex

# This would happen automatically anyways when rendering the Rmd to pdf, 
# but requires downloading packages and may not work during updates of Tex Live. 
# Better to install to the docker image once and keep them there.

tinytex::tlmgr_update()

latex_packages <- c(
  "koma-script",
  "amsmath",
  "latex-amsmath-dev",
  "iftex",
  "unicode-math",
  "xcolor",
  "fancyvrb",
  "booktabs",
  "etoolbox",
  "mdwtools",
  "float",
  "caption",
  "tcolorbox",
  "pgf",
  "environ",
  "pdfcol",
  "ltxcmds",
  "infwarerr",
  "hyperref",
  "kvoptions",
  "bookmark"
)

tinytex::tlmgr_install(latex_packages)
