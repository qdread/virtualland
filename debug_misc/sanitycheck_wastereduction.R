toget100 <- function(w) {
  100/(prod(1-w))
}

w <- c(.331, .043, .2)

toget100(w)
toget100(w/2)/toget100(w)

toget100(w * c(1, .5, .5))/toget100(w)

w2 <- c(0, 0.043, 0.2)
toget100(w2/2)/toget100(w2)

### Double sanity check.
# Load the old and new versions of the foreign waste reduction production factors and ensure they are the same
old <- read_csv('/nfs/qread-data/cfs_io_analysis/backup_27jan2022/bea_consumption_factors_diet_waste_scenarios_foreign.csv')
new <- read_csv('/nfs/qread-data/cfs_io_analysis/bea_consumption_factors_diet_waste_scenarios_foreign.csv')
all.equal(old,new) 

# What about domestic
old <- read_csv('/nfs/qread-data/cfs_io_analysis/backup_27jan2022/bea_consumption_factors_diet_waste_scenarios.csv')
new <- read_csv('/nfs/qread-data/cfs_io_analysis/bea_consumption_factors_diet_waste_scenarios.csv')
all.equal(old,new)

# Load old and new versions of the joined lafa
old_dom <- read_csv('/nfs/qread-data/cfs_io_analysis/backup_27jan2022/lafa_with_production_factors_diet_x_waste.csv')
new_dom <- read_csv('/nfs/qread-data/cfs_io_analysis/lafa_with_production_factors_diet_x_waste.csv')
all.equal(old_dom, new_dom)

old_for <- read_csv('/nfs/qread-data/cfs_io_analysis/backup_27jan2022/lafa_with_production_factors_diet_x_waste_foreign.csv')
new_for <- read_csv('/nfs/qread-data/cfs_io_analysis/lafa_with_production_factors_diet_x_waste_foreign.csv')
all.equal(old_for, new_for)

old_dom %>% select(Category, contains("prod_factor")) %>% filter(Category == "Pork")
new_dom %>% select(Category, contains("prod_factor")) %>% filter(Category == "Pork")
old_for %>% select(Category, contains("prod_factor")) %>% filter(Category == "Pork")
new_for %>% select(Category, contains("prod_factor")) %>% filter(Category == "Pork")

old_dom %>% select(Category, contains("prod_factor")) %>% filter(Category == "Skim milk")
new_dom %>% select(Category, contains("prod_factor")) %>% filter(Category == "Skim milk")
old_for %>% select(Category, contains("prod_factor")) %>% filter(Category == "Skim milk")
new_for %>% select(Category, contains("prod_factor")) %>% filter(Category == "Skim milk")

old_dom %>% select(Category, contains("prod_factor")) %>% filter(Category == "Beef")
new_dom %>% select(Category, contains("prod_factor")) %>% filter(Category == "Beef")
old_for %>% select(Category, contains("prod_factor")) %>% filter(Category == "Beef")
new_for %>% select(Category, contains("prod_factor")) %>% filter(Category == "Beef")

old_dom %>% select(Category, contains("prod_factor")) %>% filter(Category == "Fresh bananas")
new_dom %>% select(Category, contains("prod_factor")) %>% filter(Category == "Fresh bananas")
old_for %>% select(Category, contains("prod_factor")) %>% filter(Category == "Fresh bananas")
new_for %>% select(Category, contains("prod_factor")) %>% filter(Category == "Fresh bananas")

old_dom %>% select(Category, contains("prod_factor")) %>% filter(Category == "Frozen peaches")
new_dom %>% select(Category, contains("prod_factor")) %>% filter(Category == "Frozen peaches")
old_for %>% select(Category, contains("prod_factor")) %>% filter(Category == "Frozen peaches")
new_for %>% select(Category, contains("prod_factor")) %>% filter(Category == "Frozen peaches")

data.frame(cat1=new_for$Category,cat2=old_for$Category, pf1=new_for$consumer_waste_reduction_prod_factor,pf2=old_for$consumer_waste_reduction_prod_factor)
data.frame(cat1=new_dom$Category,cat2=old_dom$Category, pf1=new_dom$preconsumer_waste_reduction_prod_factor,pf2=old_dom$preconsumer_waste_reduction_prod_factor)
