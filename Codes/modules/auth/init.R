library(dplyr)

# add_table <- function(db, df, name = deparse(substitute(df)), key = names(df)[1]) {
#   copy_to(db, df, name = name, temporary = F, overwrite = T)[[1]][[1]] %>%
#     dbExecute(glue_sql('ALTER TABLE {`name`} ADD PRIMARY KEY ({`key`*});', name = name, key = key, .con = .)) %>%
#     {if (is.numeric(.)) tbl(db, name) else print('ERROR')}
# }

# User Table
if (!exists("user_base")) {
  
  # User Table
  user_base <- as_tibble(
    list(
      'username' = c('admin', 'demo', 'user1', 'user2'),
      'password_hash' = sapply(c('admin', 'demo', 'user1', 'user2'), sodium::password_store, USE.NAMES = F),
      'permissions' = c('admin', 'demo', 'user', 'user'),
      'name' = c('Admin', 'Demo Account', 'User 1', 'User 2')
    )
  )
}




