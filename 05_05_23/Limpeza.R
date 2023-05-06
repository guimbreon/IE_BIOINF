
#LIMPEZA DE DADOS
#Horas Estudo
for(i in 1:nrow(Grupo10))
  if(!(is.na(Grupo10$HorasEstudo[i])) & Grupo10$HorasEstudo[i] < 24 ){
    Grupo10$HorasEstudo[i] = Grupo10$HorasEstudo[i]
  }else {
    Grupo10$HorasEstudo[i] = NA
  }

#Horas Redes
for(i in 1:nrow(Grupo10))
  if(!(is.na(Grupo10$HorasRedes[i])) & Grupo10$HorasRedes[i] < 24 ){
    Grupo10$HorasRedes[i] = Grupo10$HorasRedes[i]
  }else {
    Grupo10$HorasRedes[i] = NA
  }

#Horas TV
for(i in 1:nrow(Grupo10))
  if(!(is.na(Grupo10$HorasTV[i])) & Grupo10$HorasTV[i] < 24 ){
    Grupo10$HorasTV[i] = Grupo10$HorasTV[i]
  }else {
    Grupo10$HorasTV[i] = NA
  }

#Horas Sono
for(i in 1:nrow(Grupo10))
  if(!(is.na(Grupo10$HorasSono[i])) & Grupo10$HorasSono[i] < 24 ){
    Grupo10$HorasSono[i] = Grupo10$HorasSono[i]
  }else {
    Grupo10$HorasSono[i] = NA
  }

#FIM LIMPEZA DE DADOS