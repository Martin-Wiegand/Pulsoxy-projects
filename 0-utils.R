check_site <- function(x){
  if(substr(x,1,1) == "0"){
    return("North")
  }else{
    if(substr(x,1,1) == "1"){
      return("South")
    }else{
      return(NA_character_)
    }
  }
}

get_practice <- function(x){
  return(substr(x,1,2))
}

demqol <- function(data){
  
}

demqolc <- function(data){
  data = data %>% select(contains("DEMQOLC"))
  
  data = 
    bind_cols(data %>% ungroup %>% select(contains("rev")) %>% rowwise %>% mutate_all(score_rev),
              data %>% ungroup %>% select(!contains("rev") & !contains("30")) %>% rowwise %>% mutate_all(score_c))
  
  # data %>%
  #   select(ID,event,everything())
  # 
  # data %>% filter(is.na(DEMQOLP_1_rev)) %>% print(n = Inf)
  
  return(rowSums(data))
}

demqolp <- function(data){
  data = data %>% select(contains("DEMQOLP"))
  
  data = 
    bind_cols(data %>% ungroup %>% select(contains("rev")) %>% rowwise %>% mutate_all(score_rev),
              data %>% ungroup %>% select(!contains("rev") & !contains("32")) %>% rowwise %>% mutate_all(score))
  
  # data %>%
  #   select(ID,event,everything())
  # 
  # data %>% filter(is.na(DEMQOLP_1_rev)) %>% print(n = Inf)
  
  return(rowSums(data))
}


score <- function(x){
  if(all(x %in% c("A lot","Quite a bit","A little","Not at all"))){
    if(x == "A lot"){return(1)}
    if(x == "Quite a bit"){return(2)}
    if(x == "A little"){return(3)}
    if(x == "Not at all"){return(4)}}else{
      return(NA_real_)
    }
}

score_rev <- function(x){
  if(all(x %in% c("A lot","Quite a bit","A little","Not at all"))){
    if(x == "A lot"){return(4)}
    if(x == "Quite a bit"){return(3)}
    if(x == "A little"){return(2)}
    if(x == "Not at all"){return(1)}
  }else{
    return(NA_real_)
  }
  
}

repl_na <- function(x){
  return(sum(is.na(x)))
}

decrease <- function(x){x - 1}

summary_print <- function(x,d = 2){
  paste0(mean(x,na.rm = T) %>% round(digits = d)," [",quantile(x,prob = 0.25,na.rm = T),",",quantile(x,prob = 0.75,na.rm = T),"] (",min(x,na.rm = T),",",max(x,na.rm = T),"); SD: ",sd(x,na.rm = T) %>% round(digits = d)) %>% return
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

hads_conversion <- function(x){
  
  check =
    tibble(x = x %>% unique) %>%
    filter(grepl(x = x,pattern = ")",ignore.case = T)) %>%
    mutate(y = case_when(grepl(x,pattern = "(A1)",ignore.case = T) ~ 1,
                         grepl(x,pattern = "(A2)",ignore.case = T) ~ 2,
                         grepl(x,pattern = "(A3)",ignore.case = T) ~ 3,
                         grepl(x,pattern = "(D1)",ignore.case = T) ~ 1,
                         grepl(x,pattern = "(D2)",ignore.case = T) ~ 2,
                         grepl(x,pattern = "(D3)",ignore.case = T) ~ 3,
                         grepl(x,pattern = "(0)",ignore.case = T) ~ 0,
                         is.na(x) ~ NA_real_,
                         TRUE ~ 99)) 
  
  check_2 =
    check %>%
    mutate(x_short = substr(x,start =  1,stop = nchar(x)-4)) %>%
    mutate(x_short = case_when(substrRight(x_short,1) == " " ~ substr(x_short,start =  1,stop = nchar(x_short)-1),
                               TRUE ~ x_short))
  
  # mutate(new = case_when(grepl(x,pattern = "\\(0",ignore.case = T) ~ substr(x,start =  1,stop = nchar(x)-4),
  #                        grepl(x,pattern = "\\(A",ignore.case = T) ~ substr(x,start =  1,stop = nchar(x)-5),
  #                        grepl(x,pattern = "\\(D",ignore.case = T) ~ substr(x,start =  1,stop = nchar(x)-5),
  #                      TRUE ~ NA_real_))
  
  
  
  tibble(x_data = x) %>%
    left_join(check_2 %>% select(x_short,y),by = c("x_data" = "x_short")) %>%
    left_join(check_2 %>% select(x,y),by = c("x_data" = "x")) %>%
    mutate(y = case_when(is.na(y.x) & !is.na(y.y) ~ y.y,
                         is.na(y.y) & !is.na(y.x) ~ y.x,
                         TRUE ~ NA_real_)) %>%
    # filter(!is.na(x_data)) %>%
    pull(y) %>%
    as.numeric() %>%
    return()
  
  # tibble(x = x) %>%
  #   mutate(x = case_when(grepl(x,pattern = "(A1)",ignore.case = T) ~ 1,
  #                        grepl(x,pattern = "(A2)",ignore.case = T) ~ 2,
  #                        grepl(x,pattern = "(A3)",ignore.case = T) ~ 3,
  #                        grepl(x,pattern = "(D1)",ignore.case = T) ~ 1,
  #                        grepl(x,pattern = "(D2)",ignore.case = T) ~ 2,
  #                        grepl(x,pattern = "(D3)",ignore.case = T) ~ 3,
  #                        grepl(x,pattern = "(0)",ignore.case = T) ~ 0,
  #                        is.na(x) ~ NA_real_,
  #                        TRUE ~ 99)) %>%
  #   pull(x) %>%
  #   as.numeric %>%
  #   return()
}

not_na <- function(x){
  return(!is.na(x))
}

print_continuous <- function(x){
  mn <- mean(x,na.rm = T) %>% round(digits = 2)
  iqr <- quantile(x,probs = c(0.25,0.75),na.rm = T) %>% round(digits = 2)
  rg <- range(x,na.rm = T)%>% round(digits = 12)
  nas <- sum(is.na(x))
  
  return(paste0(mn," [",iqr[1],",",iqr[2],"] (",rg[1],",",rg[2],") NA: ",nas))
}

print_binary <- function(x){
  mn <- mean(x,na.rm = T) %>% round(digits = 2)*100
  number <- sum(x,na.rm = T)
  nas <- sum(is.na(x))
  
  return(paste0(number," (",mn,"%) NA: ",nas))
}

print_classes <- function(x,empties = T,dgs = 1,replacement = "No/Unknown"){
  if(empties){
    x = ifelse(x == "",replacement,x)
  }
  
  tbl = tibble(x) %>% group_by(x) %>% tally %>% arrange(desc(x))
  ntotal = length(x)
  
  tbl %>%
    mutate(n = paste0(n," (",round(n/ntotal*100,digits = dgs),"%)")) %>%
    return
}


service_use_check <- function(x,data,title,type = "CSRI",printplot = T,printtext = T){
  
  baseline_numbers_used <-
    data %>% 
    group_by(.data[[paste0(x,"0")]]) %>%
    tally %>%
    rename(did_use = .data[[paste0(x,"0")]]) %>%
    mutate(time = "Baseline")
  
  fu_numbers_used <-
    data %>% 
    group_by(.data[[paste0(x,"12")]]) %>%
    tally %>%
    rename(did_use = .data[[paste0(x,"12")]]) %>%
    mutate(time = "Follow-up")
  
  use <-
    bind_rows(baseline_numbers_used,
              fu_numbers_used) %>% complete(did_use,time,fill = list(n = 0))
  
  if(type == "CSRI"){suffix = "_no_visits"}
  if(type == "ER"){suffix = "_no"}
  if(type == "CSRI"){}
  
  baseline_times_used <-
    data %>%
    group_by(.data[[paste0(x,suffix,"0")]]) %>%
    tally %>%
    rename(did_use = .data[[paste0(x,suffix,"0")]]) %>%
    mutate(time = "Baseline")
  
  fu_times_used <-
    data %>%
    group_by(.data[[paste0(x,suffix,"12")]]) %>%
    tally %>%
    rename(did_use = .data[[paste0(x,suffix,"12")]]) %>%
    mutate(time = "Follow-up")
  
  times <-
    bind_rows(baseline_times_used,
              fu_times_used) 
  # group_by(time) %>%
  # complete(did_use,time,fill = list(n = 0)) %>% 
  # arrange(time)
  
  tops <- times %>% group_by(time) %>% summarise(max(did_use,na.rm = T))
  
  times =
    bind_rows(
      tibble(time = "Baseline",did_use = c(0:as.numeric(tops[1,2]),NA_real_)),
      tibble(time = "Follow-up",did_use = c(0:as.numeric(tops[2,2]),NA_real_))
    ) %>%
    left_join(times,by = c("did_use","time")) %>%
    mutate(n = ifelse(is.na(n),0,n))
  
  
  plot_use <-
    ggplot(use) +
    geom_bar(aes(x = ifelse(did_use == 1,"Service use","No use"),y = n,fill = time),alpha = 0.5,stat="identity",position = "dodge") +
    theme_bw() +
    labs(x = "",
         y = "Number of patients",
         fill = "",
         title = paste0(title))+
    theme(legend.position = "none",
          panel.grid.minor = NULL,
          panel.grid.major.x = NULL,
          panel.grid.minor.x = NULL) +
    scale_y_continuous(breaks = seq(0,50,by = 10),minor_breaks = NULL) 
  
  plot_times <-
    ggplot(times) +
    geom_bar(aes(x = as.factor(did_use),y = n,fill = time),alpha = 0.5,stat="identity",position = "dodge",na.rm = F) +
    theme_bw() +
    labs(x = "",
         y = "",
         fill = "",
         title = paste0("Frequency of use (Times)")) +
    # scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = seq(0,50,by = 10),minor_breaks = NULL) +
    theme(legend.position = "bottom",
          panel.grid.major = NULL,
          panel.grid.minor = NULL,
          panel.grid.major.x = NULL,
          panel.grid.minor.x = NULL) 
  
  legend <- ggpubr::get_legend(plot_times)
  plot_times <- plot_times +
    theme(legend.position = "none")
  
  CombPlot <- arrangeGrob(plot_use,plot_times,legend,layout_matrix = rbind(c(1,2),c(3,3)),heights = c(1,0.1))
  
  if(printplot){ggsave(plot_file(paste0(x,".jpg")),plot = CombPlot,height = cm(2),width = cm(3))}
  
  times_wide =
    times %>%
    pivot_wider(values_from = n,
                names_from = time) 
  
  totals = 
    times_wide %>%
    filter(did_use > 0) %>%
    colSums()
  
  totals[1] = "Any Service Use"
  
  if(printtext){
    write.table(file = csv_out_file("Tables_Output"),x = 
                  rbind(c(title,"",""),
                        rbind(times_wide,totals) %>%
                          mutate(did_use = ifelse(did_use == "0","No Service Use",did_use)) %>%
                          arrange(factor(did_use,levels = c("No Service Use","Any Service Use","1","2","3","4","5","6","7","8","9","10",NA_character_))) %>% 
                          as.data.frame()) %>%
                  print(row.names = FALSE),append = TRUE)
  }
  
  
  return(list(Use = use,
              UseFrequency = times,
              PlotUse = plot_use,
              PlotUseFrequency = plot_times,
              CombPlot = comb_plot))
}

age_hr_table <- read.csv(csv_in_file("HR_cat.csv")) %>% as.tibble

hr_age_adjustment <- function(hr,age){
  age_hr_table %>% 
    filter(age < Years) %>%
    slice(1) %>%
    mutate(class = case_when(hr <= X1st ~ "<1",
                             hr > X1st & hr <= X5th ~ "1-5",
                             hr > X5th & hr <= X10th ~ "5-10",
                             hr > X10th & hr <= X50th ~ "10-50",
                             hr > X50th & hr <= X90th ~ "50-90",
                             hr > X90th & hr <= X95th ~ "90-95",
                             hr > X95th & hr <= X99th ~ "95-99",
                             hr > X99th ~ ">99",
                             TRUE ~ NA_character_)) %>%
    pull(class) %>%
    return
}

errfun <- function(x,line){
  return(mean(abs(plnorm(c(0,as.numeric(age_hr_table[line,-1]),240),meanlog =  x[1],sdlog = x[2]) - c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1))))
}

reg1 = optim(par = c(1,1),fn = function(x)errfun(x,line = 1))
reg2 = optim(par = c(1,1),fn = function(x)errfun(x,line = 2))
reg3 = optim(par = c(1,1),fn = function(x)errfun(x,line = 3))
reg4 = optim(par = c(1,1),fn = function(x)errfun(x,line = 4))
reg6 = optim(par = c(1,1),fn = function(x)errfun(x,line = 5))
reg8 = optim(par = c(1,1),fn = function(x)errfun(x,line = 6))
reg12 = optim(par = c(1,1),fn = function(x)errfun(x,line = 7))
reg15 = optim(par = c(1,1),fn = function(x)errfun(x,line = 8))
reg18 = optim(par = c(1,1),fn = function(x)errfun(x,line = 9))


age1_fun <- function(x){plnorm(x,meanlog = reg1$par[1],sdlog = reg1$par[2])}
age2_fun <- function(x){plnorm(x,meanlog = reg2$par[1],sdlog = reg2$par[2])}
age3_fun <- function(x){plnorm(x,meanlog = reg3$par[1],sdlog = reg3$par[2])}
age4_fun <- function(x){plnorm(x,meanlog = reg4$par[1],sdlog = reg4$par[2])}
age6_fun <- function(x){plnorm(x,meanlog = reg6$par[1],sdlog = reg6$par[2])}
age8_fun <- function(x){plnorm(x,meanlog = reg8$par[1],sdlog = reg8$par[2])}
age12_fun <- function(x){plnorm(x,meanlog = reg12$par[1],sdlog = reg12$par[2])}
age15_fun <- function(x){plnorm(x,meanlog = reg15$par[1],sdlog = reg15$par[2])}
age18_fun <- function(x){plnorm(x,meanlog = reg18$par[1],sdlog = reg18$par[2])}


hr_age_interpol <- function(hr,age){
  if(age == 0){return(age1_fun(hr))}
  if(age == 1){return(age2_fun(hr))}
  if(age == 2){return(age3_fun(hr))}
  if(age == 3){return(age4_fun(hr))}
  if(age %in% c(4,5)){return(age6_fun(hr))}
  if(age %in% c(6,7)){return(age8_fun(hr))}
  if(age %in% 8:11){return(age12_fun(hr))}
  if(age %in% 12:14){return(age15_fun(hr))}
  if(age %in% 15:18){return(age18_fun(hr))}
}


ecdf(hr_sort)(120)
hr_sort = sort(merged_data_set %>% filter(!is.na(hrpaed_)) %>% pull(hrpaed_))

quartile <- function(x){
  mean(x > hr_sort) %>% return
}
