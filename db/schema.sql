
DROP TABLE IF EXISTS users;
-- This table exists more for namespacing.
-- Not for security.
CREATE TABLE users (
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  display_name TEXT DEFAULT 'USER'
);

DROP TABLE IF EXISTS items;
CREATE TABLE items (
  id TEXT PRIMARY KEY NOT NULL,
  access_token TEXT,
  institution_id TEXT,
  institution_name TEXT,
  user_id INTEGER,
  FOREIGN KEY(user_id) REFERENCES users(id)
);
CREATE INDEX items_user_id ON items(user_id);

DROP TABLE IF EXISTS accounts;
CREATE TABLE accounts (
  id TEXT PRIMARY KEY NOT NULL,
  item_id TEXT,
  balance INTEGER NOT NULL,
  name TEXT,
  mask TEXT, -- last 4 digits to display
  type TEXT,
  FOREIGN KEY(item_id) REFERENCES items(id)
);
CREATE INDEX accounts_item_id ON accounts(item_id);

DROP TABLE IF EXISTS balance_history;
CREATE TABLE balance_history (
  -- represents the balance history
  -- unfortunately plaid doesn't have this historical data so we have
  -- to maintain it
  date_of DATE NOT NULL,
  account_id TEXT NOT NULL,
  balance INTEGER NOT NULL,
  PRIMARY KEY(account_id, date_of),
  FOREIGN KEY(account_id) REFERENCES accounts(id)
);

DROP TABLE IF EXISTS transactions;
CREATE TABLE transactions (
  id TEXT PRIMARY KEY NOT NULL,
  account_id TEXT NOT NULL,
  categories TEXT, -- JSON array like ["Shopping", "Electronics"]
  category_id TEXT,
  amount INTEGER NOT NULL, -- number of cents. Can be negative
  date_of DATE NOT NULL,
  location TEXT, -- JSON
  name TEXT, -- store name?
  FOREIGN KEY(account_id) REFERENCES accounts(id)
);

CREATE INDEX transactions_account_id ON transactions(account_id);
CREATE INDEX transactions_categories ON transactions(categories);
CREATE INDEX transactions_in_time    ON transactions(date_of);
#' Get the logged in user's email and other info

#' 

#' @param id ID of the person to get the profile data for. 'me' to get current user.

#' 

#' @return A People resource

#' 

#' https://developers.google.com/+/web/api/rest/latest/people#resource-representations

#' 

#' @seealso https://developers.google.com/+/web/api/rest/latest/people

#' 

#' @export

#' 

#' @examples 

#' 

#' \dontrun{

#' library(googleAuthR)

#' library(googleID)

#' options(googleAuthR.scopes.selected = 

#'    c("https://www.googleapis.com/auth/userinfo.email",

#'      "https://www.googleapis.com/auth/userinfo.profile"))

#'                                         

#' googleAuthR::gar_auth()

#' 

#' ## default is user logged in

#' user <- get_user_info()

#' }

#' 

get_user_info <- function(id = "me"){



  

  url <- sprintf("https://www.googleapis.com/plus/v1/people/%s", id)

  

  g <- googleAuthR::gar_api_generator(url, "GET")

  

  req <- g()

  

  req$content

  

}



#' Whitelist check

#' 

#' After a user logs in, check to see if they are on a whitelist

#' 

#' @param user_info the object returned by \link{get_user_info}

#' @param whitelist A character vector of emails on whitelist

#' 

#' @return TRUE if on whitelist or no whitelist, FALSE if not

#' @export

#' 

#' @examples 

#' 

#' \dontrun{

#' library(googleAuthR)

#' library(googleID)

#' options(googleAuthR.scopes.selected = 

#'    c("https://www.googleapis.com/auth/userinfo.email",

#'      "https://www.googleapis.com/auth/userinfo.profile"))

#'                                         

#' googleAuthR::gar_auth()

#' 

#' ## default is user logged in

#' user <- get_user_info()

#' 

#' the_list <- whitelist(user, c("your@email.com", 

#'                               "another@email.com", 

#'                               "yet@anotheremail.com"))

#' 

#' if(the_list){

#'   message("You are on the list.")

#' } else {

#'   message("If you're not on the list, you're not getting in.")

#'}

#' 

#' 

#' 

#' }

whitelist <- function(user_info, whitelist = NULL){

  

  if(user_info$kind != "plus#person"){

    stop("Invalid user object used for user_info")

  }

  

  out <- FALSE

  

  if(is.null(whitelist)){

    message("No whitelist found")

    out <- TRUE

  }

  

  check <- user_info$emails$value

  

  if(is.null(check)){

    stop("No user email found")

  }

  

  if(any(check %in% whitelist)){

    message(check, " is in whitelist ")

    out <- TRUE

  } else {

    message(check, " is NOT on whitelist")

  }

  

  out

  

}
