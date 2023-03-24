#include "ldapr.h"

// [[Rcpp::export]]
DataFrame ldapr_search(SEXP f,
                 SEXP at,
                 SEXP l,
                 SEXP bd) {
  LDAPMessage *res, *msg; 
  char *dn;
  int msgtype = 0;
  // convert to char
  const char *filter    = Rcpp::as<const char *>(f);
  const char *base_dn   = Rcpp::as<const char *>(bd);
  CharacterVector x_vec = Rcpp::as<CharacterVector>(at);

  // char *attrs[] = {LDAP_ALL_USER_ATTRIBUTES, NULL}; // request all attributes
  // get the external pointer back
  Rcpp::XPtr<LDAP> ll(l);
  LDAP *ld = ll.get();

 // Create a new char** array of the same size as the vector
  char** attrs = new char*[x_vec.size()+1];

  // Copy each string to the array
  for (int i = 0; i < x_vec.size(); i++) {
    attrs[i] = (char *)Rcpp::as<const char *>(x_vec[i]);
  }
  attrs[x_vec.size()]=NULL;
  
  // perform the search
  int result = ldap_search_ext_s( 
      ld, 
      base_dn, 
      LDAP_SCOPE_SUBTREE, 
      filter, 
      attrs, 
      0,
      NULL, 
      NULL, 
      NULL, 
      0, 
      &res );

  delete[] attrs;

  // error if unsuccessful
  if(result != LDAP_SUCCESS){
    stop("Failure to search LDAP server: %s\n", ldap_err2string(result));
  }

  LDAPMessage *entry = ldap_first_entry(ld,res);

  // get the list of attribute names for the entry
  BerElement *ber;
  char *attr;
  std::vector<std::string> attribute_names;
  for (attr = ldap_first_attribute(ld, entry, &ber); attr != NULL; attr = ldap_next_attribute(ld, entry, ber)) {
    attribute_names.push_back(std::string(attr));
    ldap_memfree(attr);
  }
  if (ber != NULL) {
    ber_free(ber, 0);
  }

   // create a list to hold the columns of the data frame
  List columns = List::create();

  // iterate over each attribute and get its values for each entry in the search result
  for (auto attr_name : attribute_names) {
    // create a vector to hold the values for this attribute
    std::vector<std::string> attr_values;

    // iterate over each entry in the search result and get the values for this attribute
    for (entry = ldap_first_entry(ld,res); entry != NULL; entry = ldap_next_entry(ld, entry)) {
      // get the values for this attribute
      char **values = ldap_get_values(ld, entry, const_cast<char *>(attr_name.c_str()));
      if (values != NULL) {
        // add each value to the vector
        attr_values.push_back(std::string(values[0]));
        /*
        for (int i = 0; values[i] != NULL; i++) {
          attr_values.push_back(std::string(values[i]));
        }
        */
        // free the values array
        ldap_value_free(values);
      } else {
        // if the attribute has no value, add an empty string to the vector
        attr_values.push_back("");
      }
    } 
       // add the vector of attribute values to the list of columns
    columns[attr_name] = wrap(attr_values);
  }

  // create a data frame from the list of columns
  DataFrame df(columns);

  // set the column names to be the attribute names
  CharacterVector col_names(attribute_names.begin(), attribute_names.end());
  df.attr("names") = col_names;

  // loop through search results
  /*
  for(msg = ldap_first_message(ld, res); msg != NULL; msg = ldap_next_message(ld, msg)){
    msgtype = ldap_msgtype(msg);
    switch(msgtype){
    case LDAP_RES_SEARCH_ENTRY:
      if (( dn = ldap_get_dn( ld, res )) != NULL ) {
        search_result.push_back(dn);
        ldap_memfree( dn ); 
      }
    }
  }
  */

  ldap_msgfree(res);
  
  return(df);
}
