


list.of.packages = c("kableExtra",
                     "foreign",
                     "stringr",
                     "Hmisc",
                     "reshape2",
                     "dplyr",
                     "broom",
                     "tableHTML",
                     "condformat",
                     "factoextra",
                     "stargazer",
                     "xtable",
                     "pixiedust",
                     "RColorBrewer",
                     "glmnet",
                     "mice",
                     "basicTrendline",
                     "oaxaca",
                     "GeneralOaxaca",
                     "plm",
                     "BaylorEdPsych",
                     "jtools",
                     "lfe",
                     "openxlsx",
                     "lavaan"
                     )




new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {install.packages(new.packages)}

lapply(list.of.packages, require, character.only=T)








