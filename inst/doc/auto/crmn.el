(TeX-add-style-hook "crmn"
 (lambda ()
    (LaTeX-add-bibitems
     "RedestigXXXX"
     "SysiAho2007")
    (LaTeX-add-labels
     "fig:tz"
     "fig:plot"
     "fig:compare")
    (TeX-run-style-hooks
     "hyperref"
     "latex2e"
     "art10"
     "article"
     "a4paper")))

