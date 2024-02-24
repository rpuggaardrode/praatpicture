## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmitted new release.
* Thanks for comments on the previous version. I hope I've resolved the issues.
* There is presently no reference describing the methods in the package, but a conference paper will come out in several months. 
* The examples in emupicture are wrapped in \donttest{} because they take too long to run. Some other examples (praatanimation.Rd, talking_praatpicture.Rd), make_TextGrid.Rd, tg_createTier.rd) are wrapped in \dontrun{} because they rely on the RStudio Viewer, i.e they use rstudiapi and will fail on checks.
