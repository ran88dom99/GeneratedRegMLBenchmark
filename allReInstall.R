(instild <- installed.packages())
instild <- as.data.frame(instild)
 needreinst <- as.character(instild$Built) %in% c("3.4.1","3.4.2","3.4.3","3.4.4")
 sum(needreinst)
 instild <- instild[needreinst,]
 #instild <- old.packages()
 (instild$Package)
for(i in unique(instild$Package)){
 install.packages(i)#,type="binary")
}

#devtools::install_github("berndbischl/ParamHelpers") # version >= 1.11 needed.
#devtools::install_github("jakob-r/mlrHyperopt", dependencies = TRUE)
 