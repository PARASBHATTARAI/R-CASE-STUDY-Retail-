

#BUSINESS PROBLEM: 



###A Retail store is required to analyze the day-to-day transactions and keep a 
###track of its customers spread across various locations along with their 
###purchases/returns across various categories. 
###Create an RMarkdown report and display the below calculated metrics, 
###reports and inferences


###DATA AVAILABLE 

#### Customer: Customers information including demographics 
#### Transaction: Transactions of customers 
#### Product Hierarchy: Product information (category, sub category etc...)



####1. Merge the datasets Customers, Product Hierarchy and Transactions as 
####Customer_Final. Ensure to keep all customers who have done 
####transactions with us and select the join type accordingly. 


####a. Use the base merge() 


Customer = read.csv("Customer.csv")
prod_cat = read.csv("prod_cat_info.csv")        
Transactions =read.csv("Transactions.csv")
PREDATA <-merge(x=Customer, y=Transactions, by.x ="customer_Id", by.y ="cust_id", all = FALSE)

Customer_Final <- merge(x= PREDATA, y = prod_cat, by.x = c("prod_cat_code", "prod_subcat_code"),
                        by.y = c("prod_cat_code","prod_sub_cat_code"), all = FALSE)

##### Checking NA Value In  The Dataset

sum(is.na(Customer_Final))

####Droping NA Value as Number of NA Value is very less
require(tidyr)
Customer_Final <- tidyr::drop_na(Customer_Final)


#### b. Dplyr merge functions 

require(dplyr)
DATA <- dplyr::inner_join(Customer,Transactions, by =c("customer_Id" = "cust_id"))

DATAFINAL <- dplyr::inner_join(DATA,prod_cat, by =c("prod_cat_code" = "prod_cat_code",
                                                    "prod_subcat_code" = "prod_sub_cat_code" ))


#### 2. Prepare a summary report for the merged data set. 


#### a. Get the column names and their corresponding data types 


str(Customer_Final)

#### b. Top/Bottom 10 observations 

head(Customer_Final,10)
tail(Customer_Final,10)

#### c. "Five-number summary" for continuous variables (min, Q1, median,Q3 and max) 

summary(Customer_Final)

#### d. Frequency tables for all the categorical variables

##### Using Customerfinal Dataset for Analysis

FREQ_STORETYPE <- Customer_Final %>% dplyr::group_by(Store_type) %>% dplyr::summarise(FREQ = 
                                                                                        n()) %>% dplyr::arrange( desc(FREQ))
FREQ_STORETYPE

FREQ_PRODCAT <- Customer_Final %>% dplyr::group_by(prod_cat) %>% dplyr::summarise(FREQ = 
                                                                                    n()) %>% dplyr::arrange( desc(FREQ))
FREQ_PRODCAT


FREQ_PRODSUB <- Customer_Final %>% dplyr::group_by(prod_subcat) %>% dplyr::summarise(FREQ =
                                                                                       n()) %>% dplyr::arrange( desc(FREQ))
FREQ_PRODSUB



#### 3. Generate histograms for all continuous variables and frequency bars for 
#### categorical variables. 


hist(Customer_Final$Tax)
hist(Customer_Final$total_amt)


barplot(FREQ_STORETYPE$FREQ, names.arg = FREQ_STORETYPE$Store_type)
barplot(FREQ_PRODCAT$FREQ, names.arg = FREQ_PRODCAT$prod_cat)
barplot(FREQ_PRODSUB$FREQ, names.arg = FREQ_PRODSUB$prod_subcat)





#### 4. Calculate the following information using the merged dataset : 
  
#### a. Time period of the available transaction data 
 

require(lubridate)

Customer_Final$TRAN_DATE_FORMAT<- lubridate::dmy(Customer_Final$tran_date)

  A <- min(Customer_Final$TRAN_DATE_FORMAT)
  B <- max(Customer_Final$TRAN_DATE_FORMAT)

  
  (B-A)

  
#### b. Count of transactions where the total amount of transaction was negative  
   
Negative_transaction <- Customer_Final[(Customer_Final$total_amt < 0),]

length(Negative_transaction$transaction_id) 



#### 5. Analyze which product categories are more popular among females vs male customers.


Customer_FinalM <- Customer_Final[(Customer_Final$Gender =="M"),]
Popularity_columnM <- Customer_FinalM %>%  dplyr::group_by(Gender,prod_cat) %>% dplyr::summarise(Total_count = 
                                                                                                   sum(Qty)) %>% dplyr::arrange( desc(Total_count))
Customer_FinalF <- Customer_Final[(Customer_Final$Gender =="F"),]
Popularity_columnF <- Customer_FinalF %>%  dplyr::group_by(Gender,prod_cat) %>% dplyr::summarise(Total_count = 
                                                                                                   sum(Qty)) %>% dplyr::arrange( desc(Total_count))
Popularity_column <-cbind(Popularity_columnM,Popularity_columnF)
colnames(Popularity_column) <- c("GenderM","Prod_CategoryM", "Toal_CountM","GenderF","Prod_CategoryF", "Toal_CountF"  )
Popularity_column


#### 6. Which City code has the maximum customers and what was the percentage of customers from that city?


Customer_City <- Customer_Final %>% dplyr::group_by(city_code) %>% dplyr::summarise(Total_Customer 
                          = n(), Percentage =round(n()*100/length(Customer_Final$city_code),2))
Customer_City




#### 7. Which store type sells the maximum products by value and by quantity? 


 Sell_By_value_And_Quantity <- Customer_Final %>% dplyr::group_by(Store_type) %>% dplyr::summarise(Quantity
                                                                     = sum(Qty), Revenue = sum(total_amt) )
Sell_By_value_And_Quantity 



#### 8. What was the total amount earned from the "Electronics" and "Clothing" categories from Flagship Stores? 


FLAGSHIP_STORE <- Customer_Final[ Customer_Final$Store_type == "Flagship store",]

CATEGORY <- FLAGSHIP_STORE[(FLAGSHIP_STORE$prod_cat == "Clothing") |(FLAGSHIP_STORE$prod_cat == "Electronics"),  ]
CATEGORY_Elec_Clot <- CATEGORY %>% dplyr::group_by(prod_cat) %>% dplyr::summarise(Amount_Earned = 
                                                                 sum(total_amt)) %>% dplyr::arrange(desc(Amount_Earned))

CATEGORY_Elec_Clot

#### 9. What was the total amount earned from "Male" customers under the "Electronics" category? 

Customer_Male_Elec <- Customer_Final[ (Customer_Final$prod_cat == "Electronics") & (Customer_Final$Gender =="M"),]

Amount_Male_Elec <- Customer_Male_Elec  %>% dplyr::group_by(Gender,prod_cat) %>% summarise(Total_Amount = sum(total_amt))

Amount_Male_Elec


#### 10. How many customers have more than 10 unique transactions, after removing all transactions which have any negative amounts?

Without_Negative <- Customer_Final[Customer_Final$total_amt >0 ,]

Transaction_Count <- Without_Negative %>% dplyr::group_by(customer_Id) %>% summarise(Total_Count = n())
Transaction_Count

GREATERTHAN10 <- Transaction_Count[Transaction_Count$Total_Count >10,]
length(GREATERTHAN10$customer_Id)



#### 11. For all customers aged between 25 - 35, find out: 

  
 
#### a. What was the total amount spent for "Electronics" and "Books" product categories?

DATE <- lubridate::dmy(Customer_Final$DOB)
TODAY_DATE <- Sys.Date()

Customer_Final$DIFFERENCE <- TODAY_DATE-DATE

AGE25_35_DATA <- Customer_Final[(Customer_Final$DIFFERENCE > 25*365.25) & (Customer_Final$DIFFERENCE < 35*365.25), ]

AGE25_35_ELCBOOK <- AGE25_35_DATA[ (AGE25_35_DATA$prod_cat == "Electronics") | (AGE25_35_DATA$prod_cat =="Books"),]

AGEBOOK_ELC <- AGE25_35_ELCBOOK %>% dplyr::group_by(prod_cat) %>% summarise(Total_Sum = sum(total_amt)) %>%dplyr::arrange(desc(prod_cat))

AGEBOOK_ELC

####  b. What was the total amount spent by these customers between 1st Jan, 2014 to 1st Mar, 2014?


AGE25_35_DATA$New_tran_Date <- lubridate::dmy(AGE25_35_DATA$tran_date)

DATEBETWEEN_JANMARCH <- AGE25_35_DATA[(AGE25_35_DATA$New_tran_Date >= "2014-01-01") & (AGE25_35_DATA$New_tran_Date <= "2014-03-01"),]

AMOUNT_SPENT_BETWEEN <- DATEBETWEEN_JANMARCH  %>% dplyr::summarise(REVENUE = sum(total_amt))

AMOUNT_SPENT_BETWEEN

