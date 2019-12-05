(instild <- installed.packages())
instild <- as.data.frame(instild)
 needreinst <- as.character(instild$Built) %in% c("3.4.1","3.4.2","3.4.3","3.4.4")
 sum(needreinst)
for(i in instild$Package[needreinst]){
 install.packages(i)
}
 