# search
ldap_search <- function(
  filter,
  attrs,
  private
){
  if(!private$authenticated){
    stop(
      paste0("No authentication to ", private$uri, ". Cannot perform search"), 
      call. = FALSE
    )
  }
  assert_character(filter)
  
  ldapr_search(
    filter,
    attrs,
    private$handle,
    private$base_dn
  )
}
