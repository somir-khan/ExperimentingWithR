# # change in variance
# library(changepoint)
# set.seed(1)
# x=c(rnorm(100,0,1),rnorm(100,0,10))
# ansvar=cpt.var(x)
# plot(ansvar)
# print(ansvar) # identifies 1 changepoint at 100
# # change in mean
# y=c(rnorm(100,0,1),rnorm(100,5,1))
# ansmean=cpt.mean(y)
# plot(ansmean,cpt.col='blue')
# print(ansmean)
# # change in mean and variance
# z=c(rnorm(100,0,1),rnorm(100,2,10))
# ansmeanvar=cpt.meanvar(z)
# plot(ansmeanvar,cpt.width=3)
# print(ansmeanvar)

##code for installing necessary packge to load sqlite database
# install.packages(c("RSQLite", "dbplyr"))

# Load the necessary libraries
library(RSQLite)
library(dbplyr)
library(changepoint)

# Establish a connection to the SQLite database
conn <- dbConnect(RSQLite::SQLite(), "../CitationDataset.sqlite3")

# Load the data from the database into a dataframe
data <- dbGetQuery(conn, 'SELECT authors.field,authors.sub_field,authors.scholar_id,
                        publication_url,publication_year, total_authors 
                        FROM publications_computed_fields join authors
                        on publications_computed_fields.scholar_id=authors.scholar_id where publication_year<2024 
                        and publication_year>=1977
                        and publication_year is not NULL')

# Close the connection to the database
dbDisconnect(conn)

print(data[1,])

counts_by_year <- data %>%
  dplyr::group_by(publication_year) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::ungroup() %>%
  as.data.frame()
print(counts_by_year)
plot(counts_by_year)
counts_by_year <- counts_by_year %>%
  dplyr::mutate(
    publication_year = as.numeric(as.character(publication_year)),
    count = as.numeric(as.character(count))
  )
ansvar=cpt.meanvar(counts_by_year$count,method="PELT")
plot(ansvar)
print(ansvar)
counts_by_year_mean_authors <- data %>%
  dplyr::group_by(publication_year) %>%
  dplyr::summarise(mean_authors = mean(total_authors, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  as.data.frame()
print(counts_by_year_mean_authors)
plot(counts_by_year_mean_authors,type='l')
counts_by_year_mean_authors_numeric <- counts_by_year_mean_authors %>%
  dplyr::mutate(
    publication_year = as.numeric(as.character(publication_year)),
    mean_authors = as.numeric(as.character(mean_authors))
  )
print(counts_by_year_mean_authors_numeric)
ansvar=cpt.var(counts_by_year_mean_authors_numeric$mean_authors,
method="PELT")
plot(ansvar)
print(ansvar)
plot(counts_by_year_mean_authors$publication_year, cpt.var(counts_by_year_mean_authors$mean_authors,method='PELT'), type = "l")
print(is.numeric(cpt.var(counts_by_year_mean_authors$mean_total_authors,method='PELT')))
