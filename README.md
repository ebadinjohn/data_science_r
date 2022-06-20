# DATA SCIENCE PROJECTS EXECUTED IN R

Highlights of some the data science projects I have executed in R are presented here:


## List of Projects

- Dynamic and Interactive Benchmarking Dashboard for Meshh Ltd UK.
- Analysis of Rent Type and Duration of Tenancy in the UK- Classification, ARM, and Cluster Analysis
- Analysis of Merchandise and Services Exports among Africa’s Top Ten Economies – Focus on Nigeria. 
## Author

- [@ebadinjohn](https://www.github.com/ebadinjohn)


## Dynamic and Interactive Benchmarking Dashboard for Meshh Ltd. UK 

This was executed using an integration of Tableau, R and Maria DB. Codes cannot be displayed here as a matter of professional confidentiality.
## Analysis of Rent Type and Duration of Tenancy in the UK - Classification, ARM, and Cluster Analysis

This study predicts rent type (social-rented and private rented housing) and duration of tenancy 
(short-term and long-term tenancy) in the UK, it finds association rules that describes rent type 
and duration of tenancy. Furthermore, it clusters tenants to understand the distribution of 
tenants in the UK based on rent type and duration of tenancy. 

_**The Family Resources Survey – Household Dataset, 2019/2020**_ obtained from the UK Data 
Service was utilized for this study. _**Predictive modelling**_ using Decision Tree and Random 
Forest, _**Association Rule Mining (ARM)**_ and _**K-means clustering**_ were performed on the FRS 
household dataset in _**R-Studio**_ and _**SAS Enterprise Miner**_.

Prediction of rent type in R using decision tree gave an accuracy of 81%, random forest gave 
an accuracy 84% while decision tree in SAS gave an accuracy of 90%. Prediction of duration 
of tenancy in R with decision tree gave an accuracy of 70%, random forest gave an accuracy 
of 69% while decision tree in SAS gave an accuracy of 82%. Overall SAS decision tree models 
performed best.

ARM found that _**living in a conventional house under long-term tenancy implied that tenant 
was in social-rented housing**_. Also, _**being of white ethnicity and living in conventional 
social-rented housing with at least two adults implied that the tenant was on a long-term tenancy 
agreement**_.

K-means Clustering in R revealed four clusters of tenants as follows; _**tenants in private-rented housing 
on short-term tenancies**_ – these are highly skilled professionals not fully settled in a location, 
_**tenants in private-rented housing on long-term tenancies**_ – these are skilled professionals or 
business owners that are fully settled in a location, _**tenants in social-rented housing on short-term tenancies**_ – 
these are working class or retired tenants who have no jobs or refuse to work 
or are unable work and _**tenants in social-rented housing on long-term tenancies**_ – these are old 
or retiree tenants that have no houses or are unable to rent one at market price.
### Codes for this project are presented as follows:
- Data preparation codes
- Classification codes
- ARM codes
- K-means clustering codes
## Analysis of Merchandise and Services Exports among Africa's Top Ten Economies - Focus on Nigeria

Here, R was basically utilized for data preparation before visualization in Tableau.
