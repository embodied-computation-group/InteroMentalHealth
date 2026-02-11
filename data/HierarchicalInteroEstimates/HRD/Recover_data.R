# Script to recover the trial level HRD data and put in a Stan ready format

recover_data = function(){
  library(tidyverse)
  
  #recover data file
  raw_hrd <- read.delim(here::here("HRD_intero","Data","merged_hrd.tsv"))
  
  hrd<-raw_hrd%>%
    #select conditions of interest  
    filter(BreathCondition=='nan',Modality=="Intero")%>% 
    #condensate the table so that there iss one line per intensity
    group_by(Alpha,participant_id)%>%
    summarise(x=mean(Alpha),
              x_ratio=mean(responseBPM/listenBPM),
              n=sum(nTrials<1000),
              y=sum(Decision=="More"),
              s=0,
              ID=mean(as.numeric(str_replace(participant_id,'sub-',''))))%>%
    ungroup()%>%
    select(x,x_ratio,n,y,ID,s)%>%
    #exclude participant for which there was an error during data acquisition
    filter(ID<263|ID>295)
  
  #change id so that it start at 1 and goes up to N_participant in increment of 1  
  old_ID<-unique(hrd$ID)
  
  for(idx in 1:length(old_ID)){
    hrd$s[hrd$ID==old_ID[idx]]<-idx
  }
  hrd<-select(hrd,x,x_ratio,n,y,s)
  
  #save dataframe
  #write_csv(hrd,'preped_hrd_data.csv')
  return(hrd)
}



recover_data_extero = function(){
  library(tidyverse)
  
  #recover data file
  raw_hrd <- read.delim(here::here("HRD_intero","Data","merged_hrd.tsv"))
  
  hrd<-raw_hrd%>%
    #select conditions of interest  
    filter(BreathCondition=='nan')%>% 
    #condensate the table so that there iss one line per intensity
    group_by(Alpha,participant_id,Modality)%>%
    summarise(x=mean(Alpha),
              x_ratio=mean(responseBPM/listenBPM),
              n=sum(nTrials<1000),
              y=sum(Decision=="More"),
              s=0,
              ID=mean(as.numeric(str_replace(participant_id,'sub-',''))))%>%
    ungroup()%>%
    select(x,x_ratio,n,y,ID,Modality,s)%>%
    #exclude participant for which there was an error during data acquisition
    filter(ID<263|ID>295)
  
  #change id so that it start at 1 and goes up to N_participant in increment of 1  
  old_ID<-unique(hrd$ID)
  
  for(idx in 1:length(old_ID)){
    hrd$s[hrd$ID==old_ID[idx]]<-idx
  }
  hrd<-select(hrd,x,x_ratio,n,y,s,Modality)
  
  #save dataframe
  #write_csv(hrd,'preped_hrd_data.csv')
  return(hrd)
}




recover_data2 = function(){
  library(tidyverse)
  
  #recover data file
  raw_hrd <- read.delim(here::here("HRD_intero","Data","merged_hrd.tsv"))
  
  hrd<-raw_hrd%>%
    #select conditions of interest  
    filter(BreathCondition=='nan',Modality=="Intero")%>% 
    #condensate the table so that there is one line per intensity
    group_by(nTrials,Alpha,participant_id)%>%
    summarise(x=mean(Alpha),
              rt = DecisionRT,
              x_ratio=mean(responseBPM/listenBPM),
              n=sum(nTrials<1000),
              y=sum(Decision=="More"),
              s=0,
              ID=mean(as.numeric(str_replace(participant_id,'sub-',''))))%>%
    ungroup()%>%
    select(x,x_ratio,n,y,rt,ID,s)%>%
    #exclude participant for which there was an error during data acquisition
    filter(ID<263|ID>295)
  
  #change id so that it start at 1 and goes up to N_participant in increment of 1  
  old_ID<-unique(hrd$ID)
  
  for(idx in 1:length(old_ID)){
    hrd$s[hrd$ID==old_ID[idx]]<-idx
  }
  hrd<-select(hrd,x,x_ratio,n,rt,y,s)
  
  #save dataframe
  #write_csv(hrd,'preped_hrd_data2.csv')
  return(hrd)
}



recover_data2_extero = function(){
  library(tidyverse)
  
  #recover data file
  raw_hrd <- read.delim(here::here("HRD_intero","Data","merged_hrd.tsv"))
  
  hrd<-raw_hrd%>%
    #select conditions of interest  
    filter(BreathCondition=='nan')%>% 
    #condensate the table so that there is one line per intensity
    group_by(nTrials,Alpha,participant_id,Modality)%>%
    summarise(x=mean(Alpha),
              rt = DecisionRT,
              x_ratio=mean(responseBPM/listenBPM),
              n=sum(nTrials<1000),
              y=sum(Decision=="More"),
              s=0,
              ID=mean(as.numeric(str_replace(participant_id,'sub-',''))))%>%
    ungroup()%>%
    select(x,x_ratio,n,y,rt,ID,s,Modality)%>%
    #exclude participant for which there was an error during data acquisition
    filter(ID<263|ID>295)
  
  #change id so that it start at 1 and goes up to N_participant in increment of 1  
  old_ID<-unique(hrd$ID)
  
  for(idx in 1:length(old_ID)){
    hrd$s[hrd$ID==old_ID[idx]]<-idx
  }
  hrd<-select(hrd,x,x_ratio,n,rt,y,s,Modality)
  
  #save dataframe
  #write_csv(hrd,'preped_hrd_data2.csv')
  return(hrd)
}




get_real_data = function(){
  
  df_multi = data.frame()
  
  files = list.files(here::here("..","..","..","mnt","raid0","scratch","BIDS"), recursive = F)
  
  sub_files = files[grepl("sub",files)]
  
  #test that everyone only has 1 file
  ses = list.files(paste0(here::here("..","..","..","mnt","raid0","scratch","BIDS"),"/",sub_files), recursive = F)
  #unique(ses)
  
  for(subs in sub_files){
    
    hrd_place = list.files(paste0(here::here("..","..","..","mnt","raid0","scratch","BIDS"),"/",subs,"/","ses-session1","/","beh"))
  
    if(is.na(dir.exists(hrd_place)[1])){
      next
    }
    
    hrd_data = hrd_place[grepl("task-hrd(\\d+)",hrd_place) & grepl("tsv",hrd_place)]
    
    if(length(hrd_data) == 0){
      next
    }
    
    #get sessions number
    ses_number <- "task-hrd(\\d+)"
    
    
    
    #loop through sessions (when they have more than 1)
    for(i in 1:length(hrd_data)){
      df_sing = read.delim(paste0(here::here("..","..","..","mnt","raid0","scratch","BIDS"),"/",subs,"/","ses-session1","/","beh","/",hrd_data[i])) %>% 
        mutate(session = str_match(hrd_data[i], ses_number)[2])
      df_multi = rbind(df_multi,df_sing)
    }
  
  }
  
  return(write.csv(df_multi,here::here("HRD_intero","Data","raw_hrd.csv")))
}


prep_data = function(raw_hrd){
  
  second_ses = raw_hrd %>% filter(session == 2)
  first_ses = raw_hrd %>% filter(!participant_id %in% unique(second_ses$participant_id))
  
  raw_hrd = rbind(second_ses,first_ses)
  
  hrd<-raw_hrd%>%
    #condensate the table so that there is one line per intensity
    group_by(nTrials,Alpha,participant_id,Modality)%>%
    summarise(x=mean(Alpha),
              rt = DecisionRT,
              Confidence = Confidence,
              x_ratio=mean(responseBPM/listenBPM),
              n=sum(nTrials<1000),
              y=sum(Decision=="More"),
              s=0,
              ID=mean(as.numeric(str_replace(participant_id,'sub-',''))))%>%
    ungroup()%>%
    select(x,x_ratio,n,y,rt,ID,s,Modality,participant_id,nTrials, Confidence) %>%
    #exclude participant for which there was an error during data acquisition (Ask Nice or Leah about this IDK)
    filter((ID<263|ID>295))
  
  #change id so that it start at 1 and goes up to N_participant in increment of 1  
  old_ID<-unique(hrd$ID)
  
  for(idx in 1:length(old_ID)){
    
    hrd$s[hrd$ID==old_ID[idx]]<-idx
  }
  hrd<-select(hrd,x,x_ratio,n,rt,y,s,Modality,nTrials, Confidence,participant_id ) %>% 
  
  #save dataframe
  #write_csv(hrd,'preped_hrd_data2.csv')
  return(hrd)
  
  
}


prep_data_ses = function(raw_hrd){
  
  # second_ses = raw_hrd %>% filter(session == 2)
  # first_ses = raw_hrd %>% filter(!participant_id %in% unique(second_ses$participant_id))
  
  raw_hrd = raw_hrd %>% filter(Modality == "Intero")
  
  ids_with_2_sessions = raw_hrd %>% group_by(id,session) %>% summarize(n()) %>% filter(session == 2) %>% .$id
  
  raw_hrd1 = raw_hrd %>% filter(id %in% ids_with_2_sessions)
  
  raw_hrd = raw_hrd1
  
  hrd<-raw_hrd%>%
    #condensate the table so that there is one line per intensity
    group_by(nTrials,Alpha,participant_id,Modality)%>%
    summarise(x=mean(Alpha),
              rt = DecisionRT,
              Confidence = Confidence,
              x_ratio=mean(responseBPM/listenBPM),
              n=sum(nTrials<1000),
              y=sum(Decision=="More"),
              s=0,
              session = session,
              ID=mean(as.numeric(str_replace(participant_id,'sub-',''))))%>%
    ungroup()%>%
    select(x,x_ratio,n,y,rt,ID,s,Modality,participant_id,nTrials, Confidence,session) %>%
    #exclude participant for which there was an error during data acquisition (Ask Nice or Leah about this IDK)
    filter((ID<263|ID>295))
  
  #change id so that it start at 1 and goes up to N_participant in increment of 1  
  old_ID<-unique(hrd$ID)
  
  for(idx in 1:length(old_ID)){
    
    hrd$s[hrd$ID==old_ID[idx]]<-idx
  }
  hrd<-select(hrd,x,x_ratio,n,rt,y,s,Modality,nTrials, Confidence,session)
  
  #save dataframe
  #write_csv(hrd,'preped_hrd_data2.csv')
  return(hrd)
  
  
}



prep_data_ses_ex = function(raw_hrd){
  
  # second_ses = raw_hrd %>% filter(session == 2)
  # first_ses = raw_hrd %>% filter(!participant_id %in% unique(second_ses$participant_id))
  
  raw_hrd = raw_hrd
  
  ids_with_2_sessions = raw_hrd %>% group_by(id,session) %>% summarize(n()) %>% filter(session == 2) %>% .$id
  
  raw_hrd1 = raw_hrd %>% filter(id %in% ids_with_2_sessions)
  
  raw_hrd = raw_hrd1
  
  hrd<-raw_hrd%>%
    #condensate the table so that there is one line per intensity
    group_by(nTrials,Alpha,participant_id,Modality)%>%
    summarise(x=mean(Alpha),
              rt = DecisionRT,
              Confidence = Confidence,
              x_ratio=mean(responseBPM/listenBPM),
              n=sum(nTrials<1000),
              y=sum(Decision=="More"),
              s=0,
              session = session,
              ID=mean(as.numeric(str_replace(participant_id,'sub-',''))))%>%
    ungroup()%>%
    select(x,x_ratio,n,y,rt,ID,s,Modality,participant_id,nTrials, Confidence,session) %>%
    #exclude participant for which there was an error during data acquisition (Ask Nice or Leah about this IDK)
    filter((ID<263|ID>295))
  
  #change id so that it start at 1 and goes up to N_participant in increment of 1  
  old_ID<-unique(hrd$ID)
  
  for(idx in 1:length(old_ID)){
    
    hrd$s[hrd$ID==old_ID[idx]]<-idx
  }
  hrd<-select(hrd,x,x_ratio,n,rt,y,s,Modality,nTrials, Confidence,session)
  
  #save dataframe
  #write_csv(hrd,'preped_hrd_data2.csv')
  return(hrd)
  
  
}
