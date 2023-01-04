suppl_denomination=c()
suppl_adresse=c()
suppl_cp=c()
suppl_ville=c()
suppl_codenuts=c()
suppl_nboffrerecu=c()
suppl_siret=c()
lot_intitule=c()
lot_cpv=c()
suppl_dateattribution=c()
id=c()
k=1

for(i in 1:length(list_webid_unique)){
  for(j in 1:ifelse(is.na(df[i,]$nbr_lots),1,df[i,]$nbr_lots)){running_case = make_df(i)
  suppl_denomination[k]<-tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$denomination, error = function(e) NA)
  suppl_adresse[k]<-tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$adresse, error = function(e) NA)
  suppl_cp[k]<-tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$cp, error = function(e) NA)
  suppl_ville[k]<-tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$ville, error = function(e) NA)
  suppl_codenuts[k]<-tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$codenuts, error = function(e) NA)
  suppl_nboffrerecu[k]<-tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$nboffrerecu, error = function(e) NA)
  lot_cpv[k]<-tryCatch(as.data.frame(running_case$donnees$objet$lots[1]$lot)$cpv[[j]]$principal, error = function(e) NA)
  lot_intitule[k]<-tryCatch(running_case$donnees$attribution$decision$intitule[[j]], error = function(e) NA)
  suppl_siret[k]<-tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$codeidentnational[1], error = function(e) NA)
  suppl_dateattribution[k]<-tryCatch(str_c(as.Date(as.POSIXct(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$dateattribution[2]/1000, origin = "1970-01-01")))
                                            , error = function(e) NA)
  id[k] <- ifelse(is.null(running_case$gestion$reference$idweb), NA, running_case$gestion$reference$idweb)
  k=k+1
  }
}
df[47,]$nbr_lots
test$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[2]][1,]$denomination

suppl_df = cbind(id, lot_intitule, suppl_denomination, suppl_siret, suppl_adresse,suppl_ville,suppl_cp,suppl_codenuts,suppl_nboffrerecu, lot_cpv,suppl_dateattribution)
suppl_df<-as.data.frame(suppl_df)