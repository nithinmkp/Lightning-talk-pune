# Regresions

library(gapminder)
library(tidyverse)
library(broom)
library(kableExtra)
library(flextable)

data<-gapminder::gapminder

nest_data<- data %>% group_by(country) %>% nest()

nest_mod<- nest_data %>% 
        mutate(model=map(.x=data,~lm(lifeExp~pop,data = .x)))
nest_coef1<-nest_mod %>% mutate(coef=map(model,~tidy(.x))) %>% 
        unnest(coef) %>% select(term,estimate,p.value)



countrywise<-split(nest_coef1,nest_coef1$country)

mast_table<-map(countrywise,~.x %>% pivot_wider(names_from = term,-country,
                                                values_from = c("estimate","p.value")))

tab1<-map(countrywise,~discard(.x,names(.x)=="country"))
tab1 <-map(tab1, ~ .x %>% mutate(across(where(is.double), formatC,digits=3,
                                        format="f",
                                        width=3)))

names_country<-names(countrywise)

library(rtf)
tabs<-RTF(file = "tables.rtf")
table_fn<-function(dest,x,y){
        addParagraph(dest,x,"\n")
        addTable(dest,y,
                 header.col.justify=c("L","C","C"),
                 col.justify=c("L","C","C"))
        addParagraph(dest,"\n")
}

walk2(names_country,tab1,~table_fn(tabs,.x,.y))
done(tabs)

#Time Series
library(tseries)
data("USeconomic")
adf.test(USeconomic[,1]) # One way
test_adf<-list()
for(i in 1:ncol(USeconomic)){
        test_adf[i]<-list(adf.test(USeconomic[,i])) #Second
}




lapply(USeconomic,function(x){
        list(adf.test(x) %>% tidy(),
        pp.test(x) %>% tidy()) %>% bind_rows()
})

source("unitroots.R")
tab1<-unitroot_fn(data.frame(USeconomic))
varnames<-rep(c("Statistic","P-Value","Lag Length"),3)
names(tab1)[-1]<-varnames

kbl(tab1,booktabs = T) %>% 
        kable_classic(latex_options = "scale_down") %>% 
        add_header_above(c(" "=1,"ADF Test"=3, "PP Test"=3)) %>% 

save_kable("table.jpg")

tab2<-tab1 %>% flextable()
tab3<-tab2 %>% add_header_row(colwidths = c(1,3,3),
                              values = c(" ","ADF Test", "PP Test")) %>% 
        align( i = 1, part = "header", align = "center") %>% 
        theme_booktabs() %>% 
        merge_h(part = "header",i=1) %>% 
        vline(j = c(1, 4, 7), border = fp_border_default()) %>% 
        merge_v(part = "header", j = 1)
save_as_docx(tab3,path = "tabe3.docx")

diff(USeconomic)



data<-data.frame(USeconomic)


vars<-names(data)


ts_fn<-function(df,cols,order=1){
        for(col in cols){
             df<-mutate(df,"ln_{col}":=log(get(col)))  
        }
        varnames<-df %>% select(cols,starts_with("ln")) %>% 
                names()
        
        diff_fn<- function(x){
                assign(paste0("diff_",x),diff(x))
        }
        
        diff_lst<-df %>% select(varnames) %>% map(diff_fn)%>% 
                map(bind_cols)%>% 
                map_dfc(bind_cols) %>% 
                setNames(paste0("diff_",varnames))
        
        df<-as_tibble(df)
        ln_df<-df %>% select(starts_with("ln"))
        df<-df %>% select(-starts_with("ln"))
        
        
        lst<-list(levels=df,
                  log_levels=ln_df,
                  diff=diff_lst)
        return(lst)
}

x<-ts_fn(data,vars)

y<-map(x,unitroot_fn) %>% map_df(bind_rows)


#ggplot

data(gapminder)
dir.create("ggplots")

plot_fn<-function(country,coef){
        coef %>% ggplot(aes(x=term,y=estimate))+
                geom_col()+
                labs(title = country)
} 

gapminder %>% group_by(country,continent) %>% 
        nest() %>% 
        mutate(model=map(.x=data,~lm(lifeExp~log(pop),data = .x)))%>% 
        mutate(coef=map(model,~tidy(.x))) %>% ungroup() %>% 
        select(country,coef) %>% 
        mutate(coef=map(.x=coef,function(x){
                x$term[which(x$term=="(Intercept)")]<-"Constant"
                return(x)
        })) %>% 
        mutate(plot=map2(country,coef,plot_fn)) %>% head() %>% 
        mutate(filename=paste0("ggplots/",country,".png")) %>% 
        select(-coef,-country) %>% pwalk(ggsave)  
        
        
        

 # Another example


nest_mod %>% mutate(coef=map(model,~tidy(.x))) %>% ungroup() %>% 
        mutate(plot=map(model,dwplot(model,show_intercept = T))) %>% head() %>% 
        select(plot) %>% walk(print)

.Last.value$coef[1] %>% dwplot(show_intercept = T)+theme_bw()

nest_list<-nest_mod %>% mutate(coef=map(model,~tidy(.x))) %>% unnest(coef)
mods<-split(nest_list,nest_list$country) %>% map(~.x %>% select(-c(1:3))) %>% 
        head()

        
map2(mods,names(mods),~dwplot(.x,show_intercept = T)+
            theme_bw()+theme(legend.position = "none")+
            ggtitle(.y))


