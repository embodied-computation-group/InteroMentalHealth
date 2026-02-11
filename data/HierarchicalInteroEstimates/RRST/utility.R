


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
  
  return(write.csv(df_multi,here::here("Data","raw_hrd.csv")))
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
  hrd<-select(hrd,x,x_ratio,n,rt,y,s,Modality,nTrials, Confidence)
  
  #save dataframe
  #write_csv(hrd,'preped_hrd_data2.csv')
  return(hrd)
  
  
}


get_real_data_rrst = function(){
  
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
    
    rrst_data = hrd_place[grepl("task-rrst",hrd_place) & grepl("tsv$",hrd_place)]
    
    if(length(rrst_data) == 0){
      next
    }
    
    if(length(rrst_data) > 1){
      print(rrst_data)
      break
    }
    
    
    
    df_sing = read.delim(paste0(here::here("..","..","..","mnt","raid0","scratch","BIDS"),"/",subs,"/","ses-session1","/","beh","/",rrst_data)) 
    df_multi = rbind(df_multi,df_sing)
    
    
  }
  
  return(write.csv(df_multi,here::here("raw_rrst.csv")))
}