## R CMD check results

0 errors | 0 warnings | 1 note

* The examples in emupicture are wrapped in \donttest{} because they take too long to run. Some other examples (praatanimation.Rd, talking_praatpicture.Rd), make_TextGrid.Rd, tg_createTier.rd) are wrapped in \dontrun{} because they rely on the RStudio Viewer, i.e they use rstudioapi and will fail on checks. The examples in shiny_praatpicture.Rd are wrapped in \dontrun{} because it initiates a Shiny app.
* The libraries shinyjs and bslib are listed as imports because they are called in the Shiny code in inst/shiny/shiny_praatpicture. They are not directly used by any functions, which triggers a R CMD check note.
