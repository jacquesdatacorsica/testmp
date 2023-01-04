library(tidyverse)
library(banR)
library(jsonlite)
library(magrittr)
library(data.tree)
library(geosphere)
library(sf)
library(cartography)
library(stringr)
library(httr)
library(dplyr)
library(stringr)
library(DT)

#creating the data repository
data_dir <- 'data'
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}

BOAMP_destfile <-  str_c(data_dir, "/BOAMP_2021_df.Rda")
if (!file.exists(BOAMP_destfile)) {
  
  #============================================
  #==== First, GET the web id =================
  #============================================
  
  #================ useful for criterion request ===================
  space = "%20" # 1 space
  slash.sign = "%2F" # /
  equal.sign = "%3D" # =
  greater.than.sign = "%3E" # >
  less.than.sign = "%3C" # <
  greater.than.or.equal.to.sign = paste0(greater.than.sign,equal.sign) # >=
  less.than.or.equal.to.sign = paste0(less.than.sign,equal.sign) # <=
  # ==============================================
  
  url_start="http://api.dila.fr/opendata/api-boamp/annonces/search"
  
  # ========== Define timelaps ===================
  

  
  periods=c("01-01","05-01","09-01","13-01","17-01","21-01","25-01","28-01",
            "01-02","05-02","09-02","13-02","17-02","21-02","25-02","28-02",
            "01-04","05-04","09-04","13-04","17-04","21-04","25-04","28-04",
            "01-05","05-05","09-05","13-05","17-05","21-05","25-05","28-05",
            "01-06","05-06","09-06","13-06","17-06","21-06","25-06","28-06",
            "01-07","05-07","09-07","13-07","17-07","21-07","25-07","28-07",
            "01-08","05-08","09-08","13-08","17-08","21-08","25-08","28-08",
            "01-09","05-09","09-09","13-09","17-09","21-09","25-09","28-09",
            "01-10","05-10","09-10","13-10","17-10","21-10","25-10","28-10",
            "01-11","05-11","09-11","13-11","17-11","21-11","25-11","28-11",
            "01-12","05-12","09-12","13-12","17-12","21-12","25-12","28-12","31-12")
  # ============== GET Function ====================
  get_webid<-function(day_start, month_start, day_stop, month_stop){fromJSON(rawToChar(httr::GET(paste0(url_start, 
                                                                                                        "?criterion=",
                                                                                                        "nature_cat", equal.sign, "attribution",
                                                                                                        space, "AND", space,
                                                                                                        "dateparution", greater.than.or.equal.to.sign, "2021", slash.sign, month_start, slash.sign, day_start, # you can change the year just here
                                                                                                        space, "AND", space, 
                                                                                                        "dateparution", less.than.or.equal.to.sign, "2021", slash.sign, month_stop, slash.sign, day_stop))$content))$item$value} # you can change the year just here
  
  # ============= List of all the web id =================
  rm(list_webid)
  list_webid=c()
  for(t in 1:length(periods)-1){list_webid<-c(list_webid,get_webid(unlist(strsplit(periods[t], split="-"))[1], unlist(strsplit(periods[t], split="-"))[2],
                                                                   unlist(strsplit(periods[t+1], split="-"))[1],unlist(strsplit(periods[t+1], split="-"))[2]))}
  
  #=======================================================
  # =========GET the award notices based on the web id ===
  #=======================================================
  
  output.dir = "~/BOAMP-tender-data-2021"
  dir.create(output.dir, recursive=FALSE, showWarnings=FALSE)
  for(i in 1:length(list_webid)){
    api.output.dir = file.path(output.dir,list_webid[i])
    http.request = 
      paste0("http://api.dila.fr/opendata/api-boamp/annonces/v230/",list_webid[i])
    r = GET(http.request, add_headers(Accept = "application/json"))
    bin <- content(r, "raw")
    writeBin(bin, file.path(paste0(api.output.dir,".json")))}
  
  
  # ========================================
  # =========== generate the dataframe ====
  # =======================================
  
  output.dir = "~/BOAMP-tender-data-2021"
  
  list.files(output.dir)
  list_temp <-c(list.files(output.dir))
  list_webid<-substr(list_temp, 1, nchar(list_temp)-5)
  list_webid_unique <-unique(list_webid)
  
  # =======================================
  # =========  test the json files ========
  #========================================
  
  list_webid_inval=c()
  
  for(i in 1:length(list_webid_unique)){if(file.info(paste0(file.path(output.dir,list_webid_unique[i]),".json"))$size==92) {list_webid_inval[i]<-i}}
  
  for(i in 1:length(list_webid_unique)){if(file.info(paste0(file.path(output.dir,list_webid_unique[i]),".json"))$size<200){
   api.output.dir = file.path(output.dir,list_webid_unique[i])
    http.request = 
      paste0("http://api.dila.fr/opendata/api-boamp/annonces/v230/",list_webid_unique[i])
    r = GET(http.request, add_headers(Accept = "application/json"))
    bin <- content(r, "raw")
    writeBin(bin, file.path(paste0(api.output.dir,".json")))}}
  
  make_df(2734)
  
  file.info(paste0(file.path(output.dir,list_webid_unique[2891]),".json"))$size
  
  api.output.dir = file.path(output.dir,list_webid_unique[2734])
  http.request = 
    paste0("http://api.dila.fr/opendata/api-boamp/annonces/v230/",list_webid_unique[2734])
  r = GET(http.request, add_headers(Accept = "application/json"))
  bin <- content(r, "raw")
  writeBin(bin, file.path(paste0(api.output.dir,".json")))
  
  make_df<-function(rank){fromJSON(paste0(file.path(output.dir,list_webid_unique[rank]),".json"))}
  id=c()
  sociaux=c()
  envir=c()
  adjudicateurnuts=c()
  denomination_ach=c()
  adresse_ach=c()
  codepostal_ach=c()
  ville_ach=c()
  typepouvoiradjudicateur=c()
  nbr_lots=c()
  typeprocedure=c()
  typeorganisme=c()
  siret_ach=c()
  datepublication=c()
  datefindiffusion=c()
  rm(i)
  
  
  for(i in 1:length(list_webid_unique)){test=make_df(i)
  datepublication[i]<-ifelse(is.null(test$gestion$indexation$datepublication),NA,str_c(as.Date(as.POSIXct(test$gestion$indexation$datepublication/1000, origin = "1970-01-01"))))
  datefindiffusion[i]<-ifelse(is.null(test$gestion$indexation$datefindiffusion),NA,str_c(as.Date(as.POSIXct(test$gestion$indexation$datefindiffusion/1000, origin = "1970-01-01"))))
  siret_ach[i]<-ifelse(is.null(test$donnees$identite$codeidentnational), NA, test$donnees$identite$codeidentnational)
  id[i]<-ifelse(is.null(test$gestion$reference$idweb), NA, test$gestion$reference$idweb)
  denomination_ach[i]<-ifelse(is.null(test$donnees$identite$denomination), NA,test$donnees$identite$denomination)
  adresse_ach[i]<-ifelse(is.null(test$donnees$identite$adresse), NA,test$donnees$identite$adresse)
  codepostal_ach[i]<-ifelse(is.null(test$donnees$identite$cp), NA,test$donnees$identite$cp)
  ville_ach[i]<-ifelse(is.null(test$donnees$identite$ville), NA,test$donnees$identite$ville)
  sociaux[i]<-ifelse(is.null(test$gestion$indexation$criteressociauxenv$sociaux),0,1)
  envir[i]<-ifelse(is.null(test$gestion$indexation$criteressociauxenv$environnementaux),0,1)
  adjudicateurnuts[i]<-ifelse(is.null(test$donnees$identite$adjudicateurnuts), NA,test$donnees$identite$adjudicateurnuts)
  tryCatch(nbr_lots[i]<-ifelse(is.na(test$donnees$objet$lots),NA,ifelse(nrow(as.data.frame(test$donnees$objet$lots$lot))==0,1,nrow(as.data.frame(test$donnees$objet$lots$lot)))), error = function(e) nbr_lots[i]<-1)
  
  typepouvoiradjudicateur[i]<-ifelse(is.null(test$donnees$typepouvoiradjudicateur),NA,
                                     ifelse(!is.null(test$donnees$typepouvoiradjudicateur$minautoritenationale),"minautoritenationale",
                                            ifelse(!is.null(test$donnees$typepouvoiradjudicateur$agencenational), "agencenational",
                                                   ifelse(!is.null(test$donnees$typepouvoiradjudicateur$autotriteregional), "autoriteregional",
                                                          ifelse(!is.null(test$donnees$typepouvoiradjudicateur$agenceregional),"agenceregionale",
                                                                 ifelse(!is.null(test$donnees$typepouvoiradjudicateur$orgdroitpublic),"orgdroitpublic",
                                                                        ifelse(!is.null(test$donnees$typepouvoiradjudicateur$institutioneuropeenne),"institutioneuropeenne","autre"
                                                                        )
                                                                 ))))))
  typeprocedure[i]<-ifelse(is.null(test$donnees$procedure$typeprocedure),NA,
                           ifelse(!is.null(test$donnees$procedure$typeprocedure$ouvert), "ouvert",
                                  ifelse(!is.null(test$donnees$procedure$typeprocedure$restreint), "restreint",
                                         ifelse(!is.null(test$donnees$procedure$typeprocedure$negocie), "negocie",
                                                ifelse(!is.null(test$donnees$procedure$typeprocedure$procedureadapte), "procedureadapte",
                                                       ifelse(!is.null(test$donnees$procedure$typeprocedure$dialoguecompetitif), "dialoguecompetitif",
                                                              ifelse(!is.null(test$donnees$procedure$typeprocedure$partenariatinnovation), "partenariatinnovation",NA
                                                                     
                                                              )))))))
  
  typeorganisme[i]<-ifelse(is.null(test$donnees$typeorganisme),NA,
                           ifelse(!is.null(test$donnees$typeorganisme$institutioneuropeenne), "institutioneuropeenne",
                                  ifelse(!is.null(test$donnees$typeorganisme$etat),"etat",
                                         ifelse(!is.null(test$donnees$typeorganisme$region),"region",
                                                ifelse(!is.null(test$donnees$typeorganisme$departement),"departement",
                                                       ifelse(!is.null(test$donnees$typeorganisme$commune),"commune",
                                                              ifelse(!is.null(test$donnees$typeorganisme$epn), "epn",
                                                                     ifelse(!is.null(test$donnees$typeorganisme$ept), "ept",
                                                                            ifelse(!is.null(test$donnees$typeorganisme$autre),"autre",NA)))))))))
  }
  
  
  df<-cbind(id, datepublication, datefindiffusion,  siret_ach, denomination_ach,adresse_ach,codepostal_ach,ville_ach ,adjudicateurnuts,typepouvoiradjudicateur, typeorganisme  ,nbr_lots, typeprocedure, sociaux, envir)
  df<-as.data.frame(df)
  df$sociaux <-as.numeric(df$sociaux)
  df$envir <- as.numeric(df$envir)
  summary(df)
  
  #######################################
  # DATABASE RELATIVE TO SELECTED SUPPLIERS 
  ########################################
  
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
  criterescctp=c()
  list_criteria=c()
  weight=c()
  price_weight=c()
  nboffrerecu=c()
  montant=c()
  offrebasse=c()
  accordcadre=c()
  k=1
  
  
  
  
  for(i in 1:length(list_webid_unique)){
    for(j in 1:ifelse(is.na(df[i,]$nbr_lots),1,df[i,]$nbr_lots)){running_case = make_df(i)
    
    accordcadre[k]<- ifelse(!is.null(tryCatch(ifelse(!is.null(running_case$donnees$procedure$avisimplique$accordcadreoui),1,0), error = function(e) NA)),tryCatch(ifelse(!is.null(running_case$donnees$procedure$avisimplique$accordcadreoui),1,0), error = function(e) NA),NA)
    
    nboffrerecu[k]<-ifelse(!is.null(tryCatch(as.data.frame(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$nboffrerecu)[[2,1]], error = function(e) NA)),tryCatch(as.data.frame(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$nboffrerecu)[[2,1]], error = function(e) NA),NA)
    
    montant[k]<-ifelse(!is.null(tryCatch(as.data.frame(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$montant)[[2,1]], error = function(e) NA)),tryCatch(as.data.frame(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$montant)[[2,1]], error = function(e) NA),NA)
    
    offrebasse[k]<-ifelse(!is.null(tryCatch(as.data.frame(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$offrebasse)[[2,1]], error = function(e) NA)),tryCatch(as.data.frame(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$offrebasse)[[2,1]], error = function(e) NA),NA)
    
    criterescctp[k]<-ifelse(!is.null(tryCatch( as.data.frame(running_case$donnees$objet$lots$lot[[1]]$criteresattribution[j,])$criterescctp, error = function(e) NA)),tryCatch( as.data.frame(running_case$donnees$objet$lots$lot[[1]]$criteresattribution[1,])$criterescctp, error = function(e) NA),NA)
    
    
    list_criteria[k]<-ifelse(!is.null(tryCatch( paste(as.data.frame(running_case$donnees$objet$lots$lot[[1]]$criteresattribution[1,2]$critere))[1]
                                                ,  error = function(e) NA)),tryCatch( paste(as.data.frame(running_case$donnees$objet$lots$lot[[1]]$criteresattribution[1,2]$critere))[1]
                                                                                      ,  error = function(e) NA),NA)
    
    weight[k]<- ifelse(!is.null(tryCatch(paste(as.data.frame(running_case$donnees$objet$lots$lot[[1]]$criteresattribution[1,2]$critere))[2]  
                                         , error = function(e) NA)),tryCatch(paste(as.data.frame(running_case$donnees$objet$lots$lot[[1]]$criteresattribution[1,2]$critere))[2]  
                                                                             , error = function(e) NA),NA)
    
    price_weight[k]<-ifelse(!is.null(tryCatch( as.data.frame(running_case$donnees$objet$lots$lot[[1]]$criteresattribution[j,4])$poids,  error = function(e) NA)),tryCatch( as.data.frame(running_case$donnees$objet$lots$lot[[1]]$criteresattribution[j,4])$poids,  error = function(e) NA),NA)
    
    suppl_denomination[k]<-ifelse(!is.null(tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$denomination, error = function(e) NA)),
                                  tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$denomination, error = function(e) NA),NA)
    
    suppl_adresse[k]<-ifelse(!is.null(tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$adresse, error = function(e) NA)),
                             tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$adresse, error = function(e) NA),NA)
    
    suppl_cp[k]<-ifelse(!is.null(tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$cp, error = function(e) NA)),
                        tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$cp, error = function(e) NA),NA)
    
    suppl_ville[k]<-ifelse(!is.null(tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$ville, error = function(e) NA)),
                           tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$ville, error = function(e) NA),NA)
    
    suppl_codenuts[k]<-ifelse(!is.null(tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$codenuts, error = function(e) NA)),
                              tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$codenuts, error = function(e) NA),NA)
    
    suppl_nboffrerecu[k]<-ifelse(!is.null(tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$nboffrerecu, error = function(e) NA)),
                                 tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]][1,]$nboffrerecu, error = function(e) NA),NA)
    
    
    lot_cpv[k]<-ifelse(!is.null(tryCatch(as.data.frame(running_case$donnees$objet$lots[1]$lot)$cpv[[j]]$principal, error = function(e) NA)),
                       tryCatch(as.data.frame(running_case$donnees$objet$lots[1]$lot)$cpv[[j]]$principal, error = function(e) NA),NA)
    
    lot_intitule[k]<-ifelse(!is.null(tryCatch(running_case$donnees$attribution$decision$intitule[[j]], error = function(e) NA)),
                            tryCatch(running_case$donnees$attribution$decision$intitule[[j]], error = function(e) NA),NA)
    
    suppl_siret[k]<-ifelse(!is.null(tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$codeidentnational[1], error = function(e) NA)),
                           tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$codeidentnational[1], error = function(e) NA),NA)
    
    suppl_dateattribution[k]<-ifelse(!is.null(tryCatch(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$dateattribution[2], error = function(e) NA)), tryCatch(str_c(as.Date(as.POSIXct(running_case$donnees$attribution$decision$titulaireandRENSEIGNEMENT[[j]]$dateattribution[2]/1000, origin = "1970-01-01"))), error = function(e) NA),NA)
    
    id[k] <- ifelse(is.null(running_case$gestion$reference$idweb), NA, running_case$gestion$reference$idweb)
    
    k=k+1
    }
  }
  
  
  
  suppl_df = cbind(id, lot_intitule, suppl_denomination, suppl_siret, suppl_adresse,suppl_ville,suppl_cp,suppl_codenuts,suppl_nboffrerecu, lot_cpv,suppl_dateattribution, criterescctp, list_criteria, weight, price_weight, nboffrerecu, montant, offrebasse, accordcadre )
  suppl_df<-as.data.frame(suppl_df)
  
  # ================================
  # JOIN DATABASES
  # ================================
  suppl_df$id<-as.character(suppl_df$id)
  BOAMP_2021_df<-left_join(suppl_df, df)
  
  
  # ====================================
  # ======= save the file ==============
  saveRDS(BOAMP_2021_df, file=BOAMP_destfile)
  
  
  
  # ========================
  # additionnal variables
  # ========================
  
  # ======================
  # manage criteria
  # =====================
  
  BOAMP_2016_df$list_criteria<-str_replace(BOAMP_2016_df$list_criteria, fixed("c("), "")
  
  BOAMP_2016_df$list_criteria<-str_replace(BOAMP_2016_df$list_criteria, fixed(")"), "")
  
  BOAMP_2016_df$nbr_criteria<-str_count(BOAMP_2016_df$list_criteria, fixed(", "))+1
  
  # suppress the un-allocated markets
  
  BOAMP_2016_df<-BOAMP_2016_df[which(!is.na(BOAMP_2016_df$suppl_denomination)),]
  
  
  #correct SIRET
  BOAMP_2016_df$suppl_siret_clean<- str_remove_all(BOAMP_2016_df$suppl_siret," ")
  BOAMP_2016_df$siret_ach_clean<-str_remove_all(BOAMP_2016_df$siret_ach ," ")
  
  BOAMP_2016_df$suppl_siret_clean<-ifelse(str_length(BOAMP_2016_df$suppl_siret_clean)==14,BOAMP_2016_df$suppl_siret_clean,NA)
  BOAMP_2016_df$siret_ach_clean<-ifelse(str_length(BOAMP_2016_df$siret_ach_clean)==14,BOAMP_2016_df$siret_ach_clean,NA)
  
BOAMP_4<-rbind(BOAMP_2016_df,BOAMP_3)

saveRDS(BOAMP_4, file="data/BOAMP_4.Rda")
rm(list=setdiff(ls(), "BOAMP_4"))


group_mean <- aggregate(sociaux ~ substr(lot_cpv,1,2), data = BOAMP_4, mean)
group_mean <- aggregate(sociaux ~ substr(id,1,2), data = BOAMP_4, mean)
group_mean <- aggregate(envir ~ substr(datepublication,1,7), data = BOAMP_4, mean)

group_mean
plot[group_mean[6:11,]]
BOAMP_4$nbr_lots<- as.numeric(BOAMP_4$nbr_lots)  

group_mean <- aggregate(nbr_lots ~ substr(lot_cpv,1,2), data = BOAMP_4, mean)

summary(BOAMP_4)


#### restauration base globale

BOAMP_2021<-readRDS("data/BOAMP_2021_df.Rda")
BOAMP_2020<-readRDS("data/BOAMP_df.Rda")
BOAMP_2019<-readRDS("data/BOAMP_2019_df.Rda")
BOAMP_2018<-readRDS("data/BOAMP_2018_df.Rda")
BOAMP_2017<-readRDS("data/BOAMP_2017_df.Rda")
BOAMP_2016<-readRDS("data/BOAMP_2016_df.Rda")
BOAMP_2015<-readRDS("data/BOAMP_2015_df.Rda")

BOAMP<-rbind(BOAMP_2021,BOAMP_2020,BOAMP_2019,BOAMP_2018,BOAMP_2017,BOAMP_2016, BOAMP_2015)
rm(list=setdiff(ls(), "BOAMP"))


group_mean <- aggregate(envir ~ substr(datepublication,1,7), data = BOAMP, mean)
group_mean_2 <- aggregate(sociaux ~ substr(datepublication,1,7), data = BOAMP, mean)

colnames(group_mean)<-c("date","environnementales")
colnames(group_mean_2)<-c("date","sociales")

group_mean$date<-str_c(group_mean$date,"-01")
group_mean_2$date<-str_c(group_mean_2$date,"-01")
group_mean_2$date<-as.Date(group_mean_2$date)

group_mean$date<-as.Date(group_mean$date)
View(group_mean)

group<-left_join(group_mean, group_mean_2)

group2 <- gather(
  data = group,
  key = TYPE,
  value = VAL,
  sociales, environnementales)

ggplot(
  group2,
  aes(x = date, y = VAL*100, color = TYPE)
) +labs(y="% de marchés à clause", x="Données mensuelles")+
  geom_line() +theme_economist() + 
  scale_color_economist()+
  ggtitle("Évolution de l'usage des clauses  - BOAMP 2016/21")

library(ggplot2)
install.packages("ggthemes") # Installer 
library(ggthemes) # Charger
theme_set(theme_bw()())

ggplot(group_mean, aes(x=date)) +
  geom_line(aes(y=envir*100), color = "#00AFBB") +
  labs(y="% de marchés à clause envir", x="Année")

ggplot(group_mean_2, aes(x=date)) +
  geom_line(aes(y=sociaux*100), color = "#00AFBB") +
  labs(y="% de marchés à clause sociales", x="Année")

group_mean

mydata<-economics
ggplot(mydata, aes(x=date)) +
  geom_line(aes(y=unemploy))

summary(mydata)

# ======================
# manage criteria
# =====================

BOAMP$list_criteria<-str_replace(BOAMP$list_criteria, fixed("c("), "")
BOAMP$weight<-str_replace(BOAMP$weight, fixed("c("), "")

BOAMP$list_criteria<-str_replace(BOAMP$list_criteria, fixed(")"), "")
BOAMP$weight<-str_replace(BOAMP$weight, fixed(")"), "")

BOAMP$nbr_criteria<-str_count(BOAMP$list_criteria, fixed(", "))+1


data=as.vector( BOAMP[114019:114079,]$weight[14])
substr(BOAMP[114019:114079,]$weight[14],2,str_length(BOAMP[114019:114079,]$weight[14]))


gr <- gregexpr("[0-9\\.]+" , data )
sapply(regmatches(data , gr) , as.numeric)

#correct SIRET
BOAMP$suppl_siret_clean<- str_remove_all(BOAMP$suppl_siret," ")
BOAMP$siret_ach_clean<-str_remove_all(BOAMP$siret_ach ," ")

BOAMP$suppl_siret_clean<-ifelse(str_length(BOAMP$suppl_siret_clean)==14,BOAMP$suppl_siret_clean,NA)
BOAMP$siret_ach_clean<-ifelse(str_length(BOAMP$siret_ach_clean)==14,BOAMP$siret_ach_clean,NA)


group_mean_3 <- aggregate(nbr_criteria ~ substr(datepublication,1,7), data = BOAMP, mean)
colnames(group_mean_3)<-c("date","nbr_crit")

group_mean_3$date<-str_c(group_mean_3$date,"-01")
group_mean_3$date<-as.Date(group_mean_3$date)

View(group_mean_3)
ggplot(group_mean_3, aes(x=date)) +
  geom_line(aes(y=nbr_crit), color = "#00AFBB") +
  labs(y="nombre de critères", x="Année")


conseil<-BOAMP[which(substr(BOAMP$lot_cpv,1,3)=="794"),]
conseil$montant<-as.numeric(conseil$montant)
sum(conseil$montant, na.rm = TRUE)

# suppress the un-allocated markets

BOAMP<-BOAMP[which(!is.na(BOAMP$suppl_denomination)),]



# =======================
# catégorisation
# =======================



BOAMP$categorie<-ifelse(substr(BOAMP$lot_cpv, 1,2)=="45", "travaux", 
                        ifelse(substr(BOAMP$lot_cpv,1,2)>="48", "services",
                        ifelse(substr(BOAMP$lot_cpv,1,2)<"45", "fournitures",
                        ifelse(substr(BOAMP$lot_cpv,1,2)=="48", "fournitues","NonA"))))

group3<-as.data.frame(table(BOAMP$categorie,substr(BOAMP$datepublication,1,7)))
colnames(group3)<-c("catégorie","date","nbr")
group3$date<-str_c(group3$date,"-01")
group3$date<-as.Date(group3$date)

ggplot(
  group3,
  aes(x = date, y = nbr, color = catégorie)
) +labs(y="Nbr de marchés", x="moyennes mensuelles")+
  geom_line() +theme_economist() + 
  scale_color_economist()+
  ggtitle("Évolution des attributions - BOAMP 2016/20")
prop.table(BOAMP$categorie)


table(substr(conseil$datepublication,1,4))
unique(conseil$suppl_denomination)
table(conseil$suppl_denomination)
group4<-as.data.frame(prop.table(substr(conseil$datepublication,1,7)))
cconseil<-conseil[which(conseil$typepouvoiradjudicateur=="minautoritenationale"),]
