get_icd10 = function(){
  #https://icd.who.int/browse10/2016/en#/XX
  #https://icd.who.int/browse/2024-01/mms/en#1435254666
  icd10_df <- read_excel(paste0(data_folder,"icd10_icd11_mapping.xlsx"))
  icd10_df %>% pull(`10ClassKind`) %>% unique()
  icd10_chapter = icd10_df %>% filter(`10ClassKind`=="Chapter",`10DepthInKind`==1) %>%
    dplyr::select(icd10Chapter,icd10Title) %>% unique()
  icd10_block = icd10_df %>% filter(`10ClassKind`=="Block",`10DepthInKind`==1) %>%
    dplyr::select(icd10Code,icd10Chapter,icd10Title) %>% 
    tidyr::separate(icd10Code,c("start","end"),sep="-")# %>% 
  #dplyr::mutate(letter=gsub("[^A-Z]","",start)) %>% 
  #dplyr::mutate_at(c("start","end"),function(x) as.numeric(gsub("[^0-9]","",x)))
  icd10_cat = icd10_df %>% filter(`10ClassKind`=="Category",`10DepthInKind`==2) %>%
    dplyr::select(icd10_cat = icd10Code,icd10Chapter,icd10Title_cat=icd10Title) %>% 
    dplyr::mutate(icd10 = gsub("\\.[0-9]*","",icd10_cat))
  
  get_seq_icd10 = function(start,end){
    d = data.frame(letters = LETTERS) %>% 
      cross_join(data.frame(numbers=as.character(0:99)) %>% 
                   dplyr::mutate(numbers = ifelse(nchar(numbers)==1,paste0("0",numbers),as.character(numbers)))) %>% 
      unite(icd10,c(letters,numbers),sep="")
    return(d[which(d$icd10==start):which(d$icd10==end),"icd10"])
  }
  icd10_block_long=list()
  for(i in 1:dim(icd10_block)[1]){
    seq = get_seq_icd10(start=as.character(icd10_block[i,"start"]),end=as.character(icd10_block[i,"end"]))
    icd10_block_long[[i]] = icd10_block[i,] %>% 
      cross_join(data.frame(icd10 = seq)) %>% 
      # cross_join(data.frame(block_n = as.numeric(b[i,"start"]):as.numeric(b[i,"end"]))) %>% 
      # dplyr::mutate(block_n = ifelse(nchar(block_n)==1,paste0("0",block_n),as.character(block_n))) %>% 
      # unite(icd10,c(letter,block_n),remove=FALSE,sep="") %>% 
      dplyr::select(icd10Chapter,icd10,icd10Title)
    #print(i)
  }
  icd10_block_long <- do.call("rbind", icd10_block_long)
  icd10_chapter_block = icd10_block_long %>% dplyr::rename(icd10Title_block=icd10Title) %>% 
    left_join(icd10_chapter %>% dplyr::rename(icd10Title_chapter=icd10Title),by="icd10Chapter") %>% 
    dplyr::select(icd10Chapter, icd10, icd10Title_chapter, icd10Title_block) %>% 
    left_join(icd10_df %>% filter(`10ClassKind`=="Category",`10DepthInKind`==1) %>% 
                dplyr::select(icd10=icd10Code, icd10Title_cat1 = icd10Title))
  
  icd10_chapter_block_cat = icd10_chapter_block %>% 
    left_join(icd10_cat,by=c("icd10Chapter","icd10"))
  
  if(FALSE){
    #check special categories
    icd10_chapter_block_cat %>% filter(grepl("U",icd10))
  }
  
  return(list(icd10_chapter_block = icd10_chapter_block,
              icd10_chapter = icd10_chapter,
              icd10_cat = icd10_cat))
}