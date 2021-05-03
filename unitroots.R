
# Unit root tests-Table ---------------------------------------------------

unit_tests<-function(x){
        
        list("ADF"=adf.test(x) %>% tidy() ,
             "PP"=pp.test(x) %>% tidy()
        )
}

unitroot_fn<-function(x){
        r1<-map(x,~unit_tests(.x)) %>% map(bind_rows)
        methods<-map_dfr(r1,"method")
        methods<-methods[,1,drop=T]
        r2<-r1  %>% 
                map_df(bind_rows,.id = "Variable")%>% 
                select(-alternative) %>% 
                pivot_wider(names_from = "method",
                            values_from = c("statistic","p.value","parameter"),
                            names_glue = "{method}_{.value}") %>% 
                select(Variable,starts_with(methods))
        return(r2)
        
}