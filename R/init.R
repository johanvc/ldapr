# Initialize the LDAP object
ldap_init <- function(
  self, 
  private,
  uri,
  base_dn
){
  assert_character(uri)
  assert_character(base_dn)
  
  ldap_uri <- uri
  uri_check <- ldapr_url_parse(ldap_uri)

  #' private$host <- host
  #' private$port <- port
  private$base_dn <- base_dn
  private$uri <- ldap_uri
  private$handle <- ldapr_init(ldap_uri)
  self
}
