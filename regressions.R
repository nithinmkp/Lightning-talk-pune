# Regresions

library(gapminder)

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

names_country<-names(countrywise)

library(rtf)
tabs<-RTF(file = "tables.rtf")
table_fn<-function(dest,x,y){
        addParagraph(dest,x,"\n")
        addTable(dest,y)
        addParagraph(dest,"\n")
}

walk2(names_country,tab1,~table_fn(tabs,.x,.y))
done(tabs)

## ggplot

