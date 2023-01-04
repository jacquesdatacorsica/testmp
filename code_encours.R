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
  suppl_denomination[k]<-tryCatch(ifelse(!is.null(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$denomination),running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$denomination,NA), error = function(e) NA)
  suppl_adresse[k]<-tryCatch(ifelse(!is.null(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$adresse),running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$adresse,NA), error = function(e) NA)
  suppl_cp[k]<-tryCatch(ifelse(!is.null(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$cp),running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$cp,NA), error = function(e) NA)
  suppl_ville[k]<-tryCatch(ifelse(!is.null(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$ville),running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$ville,NA), error = function(e) NA)
  suppl_codenuts[k]<-tryCatch(ifelse(!is.null(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$codenuts),running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$codenuts,NA), error = function(e) NA)
  suppl_nboffrerecu[k]<-tryCatch(ifelse(!is.null(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$nboffrerecu),running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$nboffrerecu,NA), error = function(e) NA)
  lot_cpv[k]<-tryCatch(ifelse(!is.null(as.data.frame(running_case$donnees$objet$lots[1]$lot)$cpv[[j]]$principal),as.data.frame(running_case$donnees$objet$lots[1]$lot)$cpv[[j]]$principal,NA), error = function(e) NA)
  lot_intitule[k]<-tryCatch(ifelse(!is.null(running_case$donnees$attribution$decision$intitule[[j]]),running_case$donnees$attribution$decision$intitule[[j]],NA), error = function(e) NA)
  suppl_siret[k]<-tryCatch(ifelse(!is.null(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$codeidentnational[1]),running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$codeidentnational[1],NA), error = function(e) NA)
  suppl_dateattribution[k]<-tryCatch(ifelse(!is.null(str_c(as.Date(as.POSIXct(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$dateattribution[2]/1000, origin = "1970-01-01")))),str_c(as.Date(as.POSIXct(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$dateattribution[2]/1000, origin = "1970-01-01"))),NA)
                                            , error = function(e) NA)
  id[k] <- ifelse(is.null(running_case$gestion$reference$idweb), NA, running_case$gestion$reference$idweb)
  k=k+1
  }
}


suppl_df = cbind(id, lot_intitule, suppl_denomination, suppl_siret, suppl_adresse,suppl_ville,suppl_cp,suppl_codenuts,suppl_nboffrerecu, lot_cpv,suppl_dateattribution)
suppl_df<-as.data.frame(suppl_df)

test$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[1]]$nboffrerecu
test$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[1]]$montant[[2,1]]
test$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[1]]$offrebasse[[2,1]]

test$donnees$objet$lots$lot[[1]]$criteresattribution$criterescctp
as.data.frame(test$donnees$objet$lots$lot[[1]]$criteresattribution$criteresqualite$critere[1])$poids

test=make_df(31)
as.data.frame(test$donnees$objet$lots$lot[[1]]$criteresattribution[1,])$criterescctp
as.data.frame(as.data.frame(test$donnees$objet$lots$lot[[1]]$criteresattribution[1,2])$critere)$value
as.data.frame(as.data.frame(test$donnees$objet$lots$lot[[1]]$criteresattribution[1,2])$critere)$poids

as.data.frame(test$donnees$objet$lots$lot[[1]]$criteresattribution[1,4])$poids


BOAMP_df[31,]$nbr_lots                                                                        
as.data.frame(test$donnees$objet$lots$lot)
